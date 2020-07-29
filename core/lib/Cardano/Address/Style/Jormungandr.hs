{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Jormungandr
    ( -- $overview

      -- * Jormungandr
      Jormungandr
    , getKey

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    , deriveStakingPrivateKey

      -- * Addresses
      -- $addresses
    , inspectJormungandrAddress
    , paymentAddress
    , delegationAddress

      -- * Network Discrimination
    , incentivizedTestnet

      -- * Unsafe
    , liftXPrv

      -- Internals
    , minSeedLengthBytes
    , publicKeySize
    , addrSingleSize
    , addrGroupedSize
    ) where

import Prelude

import Cardano.Address
    ( Address
    , AddressDiscrimination (..)
    , HasNetworkDiscriminant (..)
    , NetworkTag (..)
    , invariantNetworkTag
    , invariantSize
    , unAddress
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( AccountingStyle
    , Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , Index
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , xpubPublicKey
    )
import Cardano.Mnemonic
    ( SomeMnemonic, someMnemonicToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Arrow
    ( first )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Data.Aeson
    ( (.=) )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.Bits
    ( shiftR, (.&.) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as Internal
import qualified Cardano.Address.Derivation as Internal
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Aeson as Json
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- $overview
--
-- This module provides an implementation of:
--
-- - 'Cardano.Address.Derivation.GenMasterKey': for generating Jormungandr master keys from mnemonic sentences
-- - 'Cardano.Address.Derivation.HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'Cardano.Address.Derivation.SoftDerivation': for hierarchical soft derivation of parent to child keys
-- - 'Cardano.Address.PaymentAddress': for constructing payment addresses from a address public key
-- - 'Cardano.Address.DelegationAddress': for constructing payment addresses from a address and stake public keys
--
-- == Disclaimer
--
-- Beware that this format of address have been specially crafted for the
-- incentivized testnet (ITN). This will soon be __deprecated__ in favor of
-- 'Cardano.Address.Style.Shelley.Shelley'.
--
-- __New integrations should not use this address style__.

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Jormungandr 'RootK XPrv
-- let accountPubKey  = Jormungandr 'AccountK XPub
-- let addressPubKey  = Jormungandr 'AddressK XPub
-- @
--
-- @since 2.0.0
newtype Jormungandr (depth :: Depth) key = Jormungandr
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 2.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Jormungandr depth))
instance (NFData key) => NFData (Jormungandr depth key)

--
-- Key Derivation
--
-- $keyDerivation
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- > let rootK = genMasterKeyFromMnemonic mw sndFactor :: Jormungandr 'RootK XPrv
--
-- === Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
--
-- > import Cardano.Address.Derivation ( AccountingStyle(..) )
-- >
-- > let accIx = toEnum 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let addIx = toEnum 0x00000014
-- > let addrK = deriveAddressPrivateKey acctK UTxOExternal addIx
--
-- > let stakeK = deriveStakingPrivateKey acctK

instance Internal.GenMasterKey Jormungandr where
    type SecondFactor Jormungandr = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic fstFactor sndFactor =
        Jormungandr $ generateNew seedValidated sndFactor
        where
            seed  = someMnemonicToBytes fstFactor
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed

instance Internal.HardDerivation Jormungandr where
    type AccountIndexDerivationType Jormungandr = 'Hardened
    type AddressIndexDerivationType Jormungandr = 'Soft
    type WithAccountStyle Jormungandr = AccountingStyle

    deriveAccountPrivateKey (Jormungandr rootXPrv) accIx =
        let
            purposeIx =
                toEnum @(Index 'Hardened _) $ fromEnum purposeIndex
            coinTypeIx =
                toEnum @(Index 'Hardened _) $ fromEnum coinTypeIndex
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 rootXPrv purposeIx
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 purposeXPrv coinTypeIx
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 coinTypeXPrv accIx
        in
            Jormungandr acctXPrv

    deriveAddressPrivateKey (Jormungandr accXPrv) accountingStyle addrIx =
        let
            changeCode =
                toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Jormungandr addrXPrv

instance Internal.SoftDerivation Jormungandr where
    deriveAddressPublicKey (Jormungandr accXPub) accountingStyle addrIx =
        fromMaybe errWrongIndex $ do
            let changeCode = toEnum @(Index 'Soft _) $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Jormungandr addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

instance Internal.StakingDerivation Jormungandr where
    deriveStakingPrivateKey (Jormungandr accXPrv) =
        let
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv (toEnum @(Index 'Soft _) 2)
            stakeXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv (minBound @(Index 'Soft _))
        in
            Jormungandr stakeXPrv

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 2.0.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> ScrubbedBytes
        -- ^ An optional second-factor passphrase (or 'mempty')
    -> Jormungandr 'RootK XPrv
genMasterKeyFromMnemonic =
    Internal.genMasterKeyFromMnemonic

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 2.0.0
genMasterKeyFromXPrv
    :: XPrv -> Jormungandr 'RootK XPrv
genMasterKeyFromXPrv =
    Internal.genMasterKeyFromXPrv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 2.0.0
deriveAccountPrivateKey
    :: Jormungandr 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Jormungandr 'AccountK XPrv
deriveAccountPrivateKey =
    Internal.deriveAccountPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an address private key from the given account private key.
--
-- @since 2.0.0
deriveAddressPrivateKey
    :: Jormungandr 'AccountK XPrv
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Jormungandr 'AddressK XPrv
deriveAddressPrivateKey =
    Internal.deriveAddressPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives an address public key from the given account public key.
--
-- @since 2.0.0
deriveAddressPublicKey
    :: Jormungandr 'AccountK XPub
    -> AccountingStyle
    -> Index 'Soft 'AddressK
    -> Jormungandr 'AddressK XPub
deriveAddressPublicKey =
    Internal.deriveAddressPublicKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derive a staking key for a corresponding 'AccountK'. Note that wallet
-- software are by convention only using one staking key per account, and always
-- the first account (with index 0').
--
-- Deriving staking keys for something else than the initial account is not
-- recommended and can lead to incompatibility with existing wallet softwares
-- (Daedalus, Yoroi, Adalite...).
--
-- @since 2.0.0
deriveStakingPrivateKey
    :: Jormungandr 'AccountK XPrv
    -> Jormungandr 'StakingK XPrv
deriveStakingPrivateKey =
    Internal.deriveStakingPrivateKey

--
-- Addresses
--
-- $addresses
-- === Generating a 'PaymentAddress'
--
-- > import Cardano.Address ( bech32 )
-- > import Cardano.Address.Derivation ( AccountingStyle(..), toXPub(..) )
-- >
-- > bech32 $ paymentAddress incentivizedTestnet (toXPub <$> addrK)
-- > "addr1vxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdncxsce5t"
--
-- === Generating a 'DelegationAddress'
--
-- > import Cardano.Address ( DelegationAddress (..) )
-- > import Cardano.Address.Derivation ( StakingDerivation (..) )
-- >
-- > bech32 $ delegationAddress incentivizedTestnet (toXPub <$> addrK) (toXPub <$> stakeK)
-- > "addr1qxpfffuj3zkp5g7ct6h4va89caxx9ayq2gvkyfvww48sdn7nudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyqmkc5xa"

instance Internal.PaymentAddress Jormungandr where
    paymentAddress discrimination k = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 (invariantNetworkTag 255 firstByte)
            putByteString (xpubPublicKey $ getKey k)
      where
          firstByte = networkTag @Jormungandr discrimination
          expectedLength = 1 + publicKeySize

instance Internal.DelegationAddress Jormungandr where
    delegationAddress discrimination paymentKey stakingKey = unsafeMkAddress $
        invariantSize expectedLength $ BL.toStrict $ runPut $ do
            putWord8 (invariantNetworkTag 255 $ NetworkTag $ firstByte + 1)
            putByteString . xpubPublicKey . getKey $ paymentKey
            putByteString . xpubPublicKey . getKey $ stakingKey
      where
          (NetworkTag firstByte) = networkTag @Jormungandr discrimination
          expectedLength = 1 + 2*publicKeySize

-- | Analyze an 'Address' to know whether it's a Jormungandr address or not.
--
-- Returns 'Nothing' if it's not a valid Shelley address, or a ready-to-print
-- string giving details about the 'Jormungandr'.
--
-- @since 2.0.0
inspectJormungandrAddress :: Address -> Maybe Json.Value
inspectJormungandrAddress addr
    | BS.length bytes < 1 + publicKeySize = Nothing
    | otherwise =
        let
            (fstByte, rest) = first BS.head $ BS.splitAt 1 bytes
            addrType = fstByte .&. 0b01111111
            network  = (fstByte .&. 0b10000000) `shiftR` 7
            size = publicKeySize
        in
            case addrType of
                0x03 | BS.length rest == size ->
                    Just $ Json.object
                    [ "address_style"   .= Json.String "Jormungandr"
                    , "address_type"    .= Json.String "single"
                    , "stake_reference" .= Json.String "none"
                    , "spending_key"    .= base16 (BS.take size rest)
                    , "network_tag"     .= network
                    ]
                0x04 | BS.length rest == 2 * size ->
                    Just $ Json.object
                    [ "address_style"   .= Json.String "Jormungandr"
                    , "address_type"    .= Json.String "group"
                    , "stake_reference" .= Json.String "by value"
                    , "spending_key"    .= base16 (BS.take size rest)
                    , "stake_key"       .= base16 (BS.drop size rest)
                    , "network_tag"     .= network
                    ]
                0x05 | BS.length rest == size ->
                    Just $ Json.object
                    [ "address_style"   .= Json.String "Jormungandr"
                    , "address_type"    .= Json.String "account"
                    , "stake_reference" .= Json.String "none"
                    , "account_key"     .= base16 (BS.take size rest)
                    , "network_tag"     .= network
                    ]
                0x06 | BS.length rest == size ->
                    Just $ Json.object
                    [ "address_style"   .= Json.String "Jormungandr"
                    , "address_type"    .= Json.String "multisig"
                    , "stake_reference" .= Json.String "none"
                    , "merkle_root"     .= base16 (BS.take size rest)
                    , "network_tag"     .= network
                    ]
                _ ->
                    Nothing
  where
    bytes = unAddress addr
    base16 = T.unpack . T.decodeUtf8 . encode EBase16

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key to a payment 'Address' valid for the given
-- network discrimination.
--
-- @since 2.0.0
paymentAddress
    :: NetworkDiscriminant Jormungandr
    -> Jormungandr 'AddressK XPub
    -> Address
paymentAddress =
    Internal.paymentAddress

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key and a staking key to a delegation 'Address' valid
-- for the given network discrimination. Funds sent to this address will be
-- delegated according to the delegation settings attached to the delegation
-- key.
--
-- @since 2.0.0
delegationAddress
    :: NetworkDiscriminant Jormungandr
    -> Jormungandr 'AddressK XPub
    -> Jormungandr 'StakingK XPub
    -> Address
delegationAddress =
    Internal.delegationAddress

--
-- Network Discrimination
--

instance HasNetworkDiscriminant Jormungandr where
    type NetworkDiscriminant Jormungandr = NetworkTag
    addressDiscrimination _ = RequiresNetworkTag
    networkTag = id

-- | 'NetworkDiscriminant' for Cardano Incentivized Testnet testnet & Jormungandr
--
-- @since 2.0.0
incentivizedTestnet :: NetworkDiscriminant Jormungandr
incentivizedTestnet = NetworkTag 0x83

--
-- Unsafe
--

-- | Unsafe backdoor for constructing an 'Jormungandr' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPrv :: XPrv -> Jormungandr depth XPrv
liftXPrv = Jormungandr

--
-- Internal
--

-- Size, in bytes, of a public key (without chain code)
publicKeySize :: Int
publicKeySize = Ed25519.publicKeySize

-- Serialized length in bytes of a Single Address
addrSingleSize :: Int
addrSingleSize = 1 + publicKeySize

-- Serialized length in bytes of a Grouped Address
addrGroupedSize :: Int
addrGroupedSize = addrSingleSize + publicKeySize

-- Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Cardano:
--
-- https://github.com/input-output-hk/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073c

-- One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
-- Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- The minimum seed length for 'generateKeyFromSeed' and
-- 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16
