{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

-- | Pure address and credential discovery for Shelley-style accounts.
--
-- 'discover' derives a contiguous batch of payment addresses for both the
-- external and internal (change) chains, together with the single stake and
-- governance credentials that belong to an account public key. It does /not/
-- query the chain; callers are expected to narrow the result themselves if
-- they already know the last used address index.
--
-- === Example
--
-- @
-- >>> import Cardano.Address (NetworkTag (..), bech32)
-- >>> import Cardano.Address.Derivation (Depth (..), GenMasterKey (..), HardDerivation (..), XPrv, XPub, toXPub)
-- >>> import Cardano.Address.Discovery
-- >>> import Cardano.Address.Style.Shelley (Shelley)
-- >>> import Cardano.Mnemonic (mkSomeMnemonic)
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> let (Right mw) = mkSomeMnemonic @'[12] ["test","walk","nut","penalty","hip","pave","soap","entry","language","right","filter","choice"]
-- >>> let rootK = genMasterKeyFromMnemonic mw mempty :: Shelley 'RootK XPrv
-- >>> let accXPub = toXPub <$> deriveAccountPrivateKey rootK minBound :: Shelley 'AccountK XPub
-- >>> let Right result = discover (NetworkTag 1) Base 1 accXPub
-- >>> length (paymentExternal result)
-- 1
-- >>> bech32 . address . head $ paymentExternal result
-- "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwqfjkjv7"
-- @

module Cardano.Address.Discovery
    ( -- * Address discovery
      DiscoverAddressType (..)
    , DiscoveryResult (..)
    , DiscoveredAddress (..)
    , DiscoveredCredential (..)
    , ErrDiscovery (..)
    , prettyErrDiscovery
    , discover
    ) where

import Prelude

import Cardano.Address
    ( Address, NetworkTag (..), unNetworkTag )
import Cardano.Address.Derivation
    ( Depth (..), DerivationType (..), Index, XPub, indexFromWord32, indexToWord32 )
import Cardano.Address.KeyHash
    ( KeyHash, KeyRole (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , Role (..)
    , Shelley
    , delegationAddress
    , deriveAddressPublicKeyShelley
    , getKey
    , hashKey
    , liftXPub
    , paymentAddress
    )

import Control.DeepSeq
    ( NFData )
import Data.Maybe
    ( fromJust )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

-- | Address type to generate during discovery.
--
-- @since 4.1.1
data DiscoverAddressType
    = Base
      -- ^ Produce base addresses, pairing each payment key with the account's
      -- stake key.
    | Enterprise
      -- ^ Produce enterprise (payment-only) addresses.
    deriving (Generic, Show, Eq)
instance NFData DiscoverAddressType

-- | Result of 'discover': a snapshot of the addresses and credentials that can
-- be derived from a single account key without querying the chain.
--
-- @since 4.1.1
data DiscoveryResult = DiscoveryResult
    { paymentExternal :: ![DiscoveredAddress]
      -- ^ Payment addresses for the external chain (role 0), indices @[0, N)@.
    , paymentInternal :: ![DiscoveredAddress]
      -- ^ Payment addresses for the internal / change chain (role 1), indices @[0, N)@.
    , stakeCredential :: !(Maybe DiscoveredCredential)
      -- ^ The account's stake credential (role 2, index 0).
    , drepCredential :: !(Maybe DiscoveredCredential)
      -- ^ The account's DRep credential (role 3, index 0).
    , committeeColdCredential :: !(Maybe DiscoveredCredential)
      -- ^ The account's constitutional committee cold credential (role 4, index 0).
    , committeeHotCredential :: !(Maybe DiscoveredCredential)
      -- ^ The account's constitutional committee hot credential (role 5, index 0).
    } deriving (Generic, Show, Eq)
instance NFData DiscoveryResult

-- | A discovered address together with its derivation index.
--
-- @since 4.1.1
data DiscoveredAddress = DiscoveredAddress
    { addressIndex :: !Word32
      -- ^ Soft derivation index.
    , address :: !Address
      -- ^ The discovered address.
    } deriving (Generic, Show, Eq)
instance NFData DiscoveredAddress

-- | A discovered on-chain credential (a 28-byte key hash) together with its
-- derivation index.
--
-- @since 4.1.1
data DiscoveredCredential = DiscoveredCredential
    { credentialIndex :: !Word32
      -- ^ Soft derivation index.
    , credential :: !KeyHash
      -- ^ The discovered credential.
    } deriving (Generic, Show, Eq)
instance NFData DiscoveredCredential

-- | Errors that can occur during discovery.
--
-- @since 4.1.1
data ErrDiscovery
    = ErrGapTooLarge !Word32
      -- ^ The requested gap is larger than the soft derivation range allows.
    | ErrInvalidNetworkTag !NetworkTag
      -- ^ The network tag is not a valid 4-bit network discriminator.
    deriving (Generic, Show, Eq)
instance NFData ErrDiscovery

-- | Pretty-print a discovery error.
--
-- @
-- >>> prettyErrDiscovery (ErrInvalidNetworkTag (NetworkTag 16))
-- "Network tag 16 is invalid: must be between 0 and 15."
-- >>> prettyErrDiscovery (ErrGapTooLarge 2147483649)
-- "Discovery gap 2147483649 exceeds the maximum soft derivation range."
-- @
--
-- @since 4.1.1
prettyErrDiscovery :: ErrDiscovery -> String
prettyErrDiscovery = \case
    ErrGapTooLarge gap ->
        "Discovery gap " <> show gap <> " exceeds the maximum soft derivation range."
    ErrInvalidNetworkTag (NetworkTag tag) ->
        "Network tag " <> show tag <> " is invalid: must be between 0 and 15."

-- | Discover all addresses and credentials derivable from an account public key
-- within the given gap, for the given network and address type.
--
-- This function does __not__ query the chain. It simply returns the contiguous
-- range of payment indices @[0, N)@ for both external and internal chains, plus
-- the single stake and governance credentials at index 0. Callers are expected
-- to know the "last used index" themselves if they wish to narrow the result.
--
-- === Examples
--
-- Generate one mainnet enterprise address from the account:
--
-- @
-- >>> let Right result = discover (NetworkTag 1) Enterprise 1 accXPub
-- >>> bech32 . address . head $ paymentExternal result
-- "addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8"
-- @
--
-- A gap of zero yields no payment addresses but still derives credentials:
--
-- @
-- >>> let Right gap0 = discover (NetworkTag 1) Base 0 accXPub
-- >>> null (paymentExternal gap0) && null (paymentInternal gap0)
-- True
-- @
--
-- Invalid inputs are rejected:
--
-- @
-- >>> discover (NetworkTag 16) Base 1 accXPub
-- Left (ErrInvalidNetworkTag (NetworkTag {unNetworkTag = 16}))
-- >>> let maxGap = 2147483648 -- 2^31
-- >>> discover (NetworkTag 1) Base (maxGap + 1) accXPub
-- Left (ErrGapTooLarge 2147483649)
-- @
--
-- @since 4.1.1
discover
    :: NetworkTag
    -> DiscoverAddressType
    -> Word32
    -> Shelley 'AccountK XPub
    -> Either ErrDiscovery DiscoveryResult
discover networkTag addrType gap accXPub
    | unNetworkTag networkTag >= 16 =
        Left $ ErrInvalidNetworkTag networkTag
    | fromIntegral gap > maxSoftGap =
        Left $ ErrGapTooLarge gap
    | otherwise =
        Right $ DiscoveryResult
            { paymentExternal = derivePaymentAddresses UTxOExternal stakeHash
            , paymentInternal = derivePaymentAddresses UTxOInternal stakeHash
            , stakeCredential = Just $ deriveCredential Stake 0 Delegation
            , drepCredential = Just $ deriveCredential DRep 0 Representative
            , committeeColdCredential = Just $ deriveCredential CCCold 0 CommitteeCold
            , committeeHotCredential = Just $ deriveCredential CCHot 0 CommitteeHot
            }
  where
    maxSoftGap :: Integer
    maxSoftGap =
        fromIntegral (indexToWord32 (maxBound @(Index 'Soft 'PaymentK))) + 1

    stakeHash :: Maybe KeyHash
    stakeHash = Just $ credential $ deriveCredential Stake 0 Delegation

    derivePaymentAddresses :: Role -> Maybe KeyHash -> [DiscoveredAddress]
    derivePaymentAddresses role mStake
        | gap == 0 = []
        | otherwise =
            [ DiscoveredAddress ix (mkAddress ix)
            | ix <- [0 .. gap - 1]
            ]
      where
        mkAddress :: Word32 -> Address
        mkAddress ix =
            let paymentXPub = liftXPub (deriveXPub role ix) :: Shelley 'PaymentK XPub
                paymentCred = PaymentFromExtendedKey paymentXPub
            in case addrType of
                Enterprise ->
                    paymentAddress networkTag paymentCred
                Base ->
                    case mStake of
                        Nothing ->
                            paymentAddress networkTag paymentCred
                        Just stake ->
                            delegationAddress networkTag paymentCred
                                (DelegationFromKeyHash stake)

    deriveCredential :: Role -> Word32 -> KeyRole -> DiscoveredCredential
    deriveCredential role ix keyRole =
        -- The phantom depth is irrelevant here: 'hashKey' only hashes the raw
        -- XPub bytes. We pick 'PaymentK' as a convenient concrete witness.
        DiscoveredCredential ix (hashKey keyRole (xpub :: Shelley 'PaymentK XPub))
      where
        xpub = liftXPub (deriveXPub role ix)

    deriveXPub :: Role -> Word32 -> XPub
    deriveXPub role ix =
        deriveAddressPublicKeyShelley accPub role index'
      where
        accPub = getKey accXPub
        -- Safe because 'gap' and the governance/stake index 0 are within the
        -- soft derivation range validated above.
        index' = fromJust $ indexFromWord32 @(Index 'Soft 'PaymentK) ix
