{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Icarus
    ( -- $overview

      -- * Icarus
      Icarus
    , getKey
    , Role (..)
    , roleFromIndex
    , roleToIndex

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey

      -- * Addresses
      -- $addresses
    , AddressInfo (..)
    , eitherInspectAddress
    , inspectAddress
    , inspectIcarusAddress
    , paymentAddress
    , ErrInspectAddress
    , prettyErrInspectAddress

      -- * Network Discrimination
    , icarusMainnet
    , icarusStaging
    , icarusTestnet
    , icarusPreview
    , icarusPreprod

      -- * Unsafe
    , liftXPrv
    , liftXPub

      -- Internals
    , unsafeGenerateKeyFromHardwareLedger
    , minSeedLengthBytes
    ) where

import Prelude

import Cardano.Address
    ( Address
    , AddressDiscrimination (..)
    , HasNetworkDiscriminant (..)
    , NetworkDiscriminant (..)
    , NetworkTag (..)
    , unAddress
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationScheme (..)
    , DerivationType (..)
    , Index (..)
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , indexFromWord32
    , unsafeMkIndex
    , xprvFromBytes
    )
import Cardano.Address.Internal
    ( DeserialiseFailure, WithErrorMessage (..) )
import Cardano.Address.Style.Byron
    ( byronMainnet, byronPreprod, byronPreview, byronStaging, byronTestnet )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy, mnemonicToText )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.DeepSeq
    ( NFData )
import Control.Exception
    ( Exception, displayException )
import Control.Exception.Base
    ( assert )
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Crypto.Hash.Algorithms
    ( SHA256 (..), SHA512 (..) )
import Crypto.MAC.HMAC
    ( HMAC, hmac )
import Data.Aeson
    ( ToJSON (..), (.=) )
import Data.Bifunctor
    ( bimap, first )
import Data.Bits
    ( clearBit, setBit, testBit )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32, Word8 )
import Fmt
    ( format )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as Internal
import qualified Cardano.Address.Derivation as Internal
import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.Aeson as Json
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- $overview
--
-- This module provides an implementation of:
--
-- - 'Cardano.Address.Derivation.GenMasterKey': for generating Icarus master keys from mnemonic sentences
-- - 'Cardano.Address.Derivation.HardDerivation': for hierarchical hard derivation of parent to child keys
-- - 'Cardano.Address.Derivation.SoftDerivation': for hierarchical soft derivation of parent to child keys
-- - 'Cardano.Address.PaymentAddress': for constructing addresses from a public key
--
-- We call 'Icarus' addresses the new format of Cardano addresses which came
-- after 'Cardano.Address.Style.Byron.Byron'. This is the format initially used in /Yoroi/
-- and now also used by /Daedalus/.

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Icarus 'RootK XPrv
-- let accountPubKey  = Icarus 'AccountK XPub
-- let addressPubKey  = Icarus 'PaymentK XPub
-- @
--
-- @since 1.0.0
newtype Icarus (depth :: Depth) key = Icarus
    { getKey :: key
        -- ^ Extract the raw 'XPrv' or 'XPub' wrapped by this type.
        --
        -- @since 1.0.0
    }
    deriving stock (Generic, Show, Eq)

deriving instance (Functor (Icarus depth))
instance (NFData key) => NFData (Icarus depth key)

data Role
    = UTxOExternal
    | UTxOInternal
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData Role

roleFromIndex :: Index 'Soft depth -> Maybe Role
roleFromIndex ix = case indexToWord32 ix of
    0 -> Just UTxOExternal
    1 -> Just UTxOInternal
    _ -> Nothing

roleToIndex :: Role -> Index 'Soft depth
roleToIndex = unsafeMkIndex . \case
    UTxOExternal -> 0
    UTxOInternal -> 1

--
-- Key Derivation
--
-- $keyDerivation
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > :set -XFlexibleContexts
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- > import Cardano.Address ( base58 )
-- > import Cardano.Address.Derivation ( toXPub )
-- > import qualified Cardano.Address.Style.Icarus as Icarus
-- >
-- > let (Right mw) = mkSomeMnemonic @'[15] ["network","empty","cause","mean","expire","private","finger","accident","session","problem","absurd","banner","stage","void","what"]
-- > let sndFactor = mempty -- Or alternatively, a second factor mnemonic transformed to bytes via someMnemonicToBytes
-- > let rootK = Icarus.genMasterKeyFromMnemonic mw sndFactor :: Icarus 'RootK XPrv
--
-- === Deriving child keys
--
-- Let's consider the following 3rd, 4th and 5th derivation paths @0'\/0\/14@
-- === accIx assumes values from 2147483648 (ie. 0x80000000) to 4294967295 (ie. 0xFFFFFFFF)
-- === addIx assume values from 0 to 2147483647 (ie. 7FFFFFFF)
-- > let Just accIx = indexFromWord32 0x80000000
-- === this is the same as
-- > let accIx = minBound @(Index 'Hardened 'AccountK)
-- > let acctK = Icarus.deriveAccountPrivateKey rootK accIx
-- >
-- > let Just addIx = indexFromWord32 0x00000014
-- > let addrK = Icarus.deriveAddressPrivateKey acctK Icarus.UTxOExternal addIx
-- >
-- > base58 $ Icarus.paymentAddress Icarus.icarusMainnet (toXPub <$> addrK)
-- >"Ae2tdPwUPEZ8XpsjgQPH2cJdtohkYrxJ3i5y6mVsrkZZkdpdn6mnr4Rt6wG"

instance Internal.GenMasterKey Icarus where
    type SecondFactor Icarus = ScrubbedBytes

    genMasterKeyFromXPrv = liftXPrv
    genMasterKeyFromMnemonic (SomeMnemonic mw) sndFactor =
        let
            seed  = entropyToBytes $ mnemonicToEntropy mw
            seedValidated = assert
                (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
                seed
        in Icarus $ generateNew seedValidated sndFactor

instance Internal.HardDerivation Icarus where
    type AccountIndexDerivationType Icarus = 'Hardened
    type AddressIndexDerivationType Icarus = 'Soft
    type WithRole Icarus = Role

    deriveAccountPrivateKey (Icarus rootXPrv) accIx =
        let
            Just purposeIx =
                indexFromWord32 @(Index 'Hardened _) purposeIndex
            Just coinTypeIx =
                indexFromWord32 @(Index 'Hardened _) coinTypeIndex
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 rootXPrv purposeIx
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 purposeXPrv coinTypeIx
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 coinTypeXPrv accIx
        in
            Icarus acctXPrv

    deriveAddressPrivateKey (Icarus accXPrv) role addrIx =
        let
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 accXPrv (roleToIndex role)
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 changeXPrv addrIx
        in
            Icarus addrXPrv

instance Internal.SoftDerivation Icarus where
    deriveAddressPublicKey (Icarus accXPub) role addrIx =
        fromMaybe errWrongIndex $ do
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub (roleToIndex role)
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ Icarus addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 1.0.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> ScrubbedBytes
        -- ^ An optional second-factor passphrase (or 'mempty')
    -> Icarus 'RootK XPrv
genMasterKeyFromMnemonic =
    Internal.genMasterKeyFromMnemonic

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 1.0.0
genMasterKeyFromXPrv
    :: XPrv
    -> Icarus 'RootK XPrv
genMasterKeyFromXPrv =
    Internal.genMasterKeyFromXPrv

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 1.0.0
deriveAccountPrivateKey
    :: Icarus 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Icarus 'AccountK XPrv
deriveAccountPrivateKey =
    Internal.deriveAccountPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an address private key from the given account private key.
--
-- @since 1.0.0
deriveAddressPrivateKey
    :: Icarus 'AccountK XPrv
    -> Role
    -> Index 'Soft 'PaymentK
    -> Icarus 'PaymentK XPrv
deriveAddressPrivateKey =
    Internal.deriveAddressPrivateKey

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock
--
-- | Derives an address public key from the given account public key.
--
-- @since 1.0.0
deriveAddressPublicKey
    :: Icarus 'AccountK XPub
    -> Role
    -> Index 'Soft 'PaymentK
    -> Icarus 'PaymentK XPub
deriveAddressPublicKey =
    Internal.deriveAddressPublicKey

--
-- Addresses
--
-- $addresses
-- === Generating a 'PaymentAddress'
--
-- | Possible errors from inspecting a Shelley address
--
-- @since 3.0.0
data ErrInspectAddress
    = UnexpectedDerivationPath
    | DeserialiseError DeserialiseFailure
    deriving (Generic, Show, Eq)
    deriving ToJSON via WithErrorMessage ErrInspectAddress

instance Exception ErrInspectAddress where
  displayException = prettyErrInspectAddress

-- | Pretty-print an 'ErrInspectAddress'
--
-- @since 3.0.0
prettyErrInspectAddress :: ErrInspectAddress -> String
prettyErrInspectAddress = \case
    UnexpectedDerivationPath ->
        "Unexpected derivation path"
    DeserialiseError e ->
        format "Deserialisation error (was: {})" (show e)

-- Determines whether an 'Address' is an Icarus address.
--
-- Returns a JSON object with information about the address, or throws
-- 'ErrInspectAddress' if the address isn't an icarus address.
--
-- @since 2.0.0
inspectIcarusAddress :: MonadThrow m => Address -> m Json.Value
inspectIcarusAddress = inspectAddress
{-# DEPRECATED inspectIcarusAddress "use qualified 'inspectAddress' instead." #-}

-- | Determines whether an 'Address' is an Icarus address.
--
-- Returns a JSON object with information about the address, or throws
-- 'ErrInspectAddress' if the address isn't an icarus address.
-- λ> :set -XOverloadedStrings
-- λ> :set -XTypeApplications
-- λ> :set -XDataKinds
-- λ> :set -XFlexibleContexts
-- λ> import Cardano.Mnemonic ( mkSomeMnemonic )
-- λ> import qualified Cardano.Address.Style.Icarus as Icarus
-- λ> import Cardano.Address.Derivation ( toXPub )
-- λ> import Cardano.Address ( base58 )
-- λ> let (Right mw) = mkSomeMnemonic @'[12] ["moon","fox","ostrich","quick","cactus","raven","wasp","intact","first","ring","crumble","error"]
-- λ> let sndFactor = mempty
-- λ> let rootK = Icarus.genMasterKeyFromMnemonic mw sndFactor :: Icarus 'RootK XPrv
-- λ> let Just accIx = indexFromWord32 0x80000000
-- λ> let acctK = Icarus.deriveAccountPrivateKey rootK accIx
-- λ> let Just addIx = indexFromWord32 0x00000014
-- λ> let addrK = Icarus.deriveAddressPrivateKey acctK Icarus.UTxOExternal addIx
-- λ> (toXPub <$> addrK)
-- Icarus {getKey = XPub {xpubPublicKey = "\223\148\230\206\187\135\253\SO\151\216\183\210]}s:\151\134\174q\173\207\184\202\EM\176\170\220\216\235\&1\243", xpubChaincode = ChainCode "\\\160\196\&8~\208\165\241\138\SOH\222\ETX*\150&\214\185\196 \153\DC2\167\165\243\155\136\228\255\229~d\253"}}
-- λ> base58 $ Icarus.paymentAddress icarusMainnet (toXPub <$> addrK)
-- "Ae2tdPwUPEYyzBcNXkFWKywMiZ9eSd96dQxhBQd371foiH16Y7gFgLBj9G5"
--
-- λ> import Cardano.Codec.Cbor
-- λ> import Crypto.Hash.Algorithms (Blake2b_224, SHA3_256)
-- λ> import Crypto.Hash (hash)
-- λ> let blake2b224 = hash @_ @Blake2b_224
-- λ> let sha3256 = hash @_ @SHA3_256
-- λ> import qualified Codec.CBOR.Encoding as CBOR
-- λ> let encodeXPub = CBOR.encodeBytes (xpubToBytes . Icarus.getKey $ icarusAddrKPub)
-- λ> let encodeSpendingData = CBOR.encodeListLen 2 <> CBOR.encodeWord8 0 <> encodeXPub
-- λ> let encodeAttrs = CBOR.encodeMapLen 0
-- λ> import qualified Data.ByteArray as BA
-- λ> let rootAddr = BA.convert $ blake2b224 $ sha3256 $ CBOR.toStrictByteString $ mempty <> CBOR.encodeListLen 3 <> CBOR.encodeWord8 0 <> encodeSpendingData <> encodeAttrs
-- λ> encode EBase16 rootAddr
-- "1fdde02c9e087474aa7ab0a46ae2f6d316a92cd0fa2d4e8b1c2eebdf"
--
-- $ echo Ae2tdPwUPEYyzBcNXkFWKywMiZ9eSd96dQxhBQd371foiH16Y7gFgLBj9G5 | cardano-address address inspect
-- {
--    "stake_reference": "none",
--    "address_style": "Icarus",
--    "address_root": "1fdde02c9e087474aa7ab0a46ae2f6d316a92cd0fa2d4e8b1c2eebdf",
--    "network_tag": null,
--    "address_type": 8
--}
-- @since 2.0.0
inspectAddress :: MonadThrow m => Address -> m Json.Value
inspectAddress = either throwM (pure . toJSON) . eitherInspectAddress

-- | Determines whether an 'Address' is an Icarus address.
--
-- Returns either details about the 'Address', or 'ErrInspectAddress' if it's
-- not a valid icarus address.
--
-- @since 3.4.0
eitherInspectAddress :: Address -> Either ErrInspectAddress AddressInfo
eitherInspectAddress addr = do
    payload <- first DeserialiseError $
        CBOR.deserialiseCbor CBOR.decodeAddressPayload $
        unAddress addr
    ntwrk <- bimap DeserialiseError (fmap NetworkTag) $
        CBOR.deserialiseCbor CBOR.decodeProtocolMagicAttr payload
    (root, attrs) <- first DeserialiseError $
        CBOR.deserialiseCbor decodePayload payload
    if (elem 1 $ fst <$> attrs)
        then Left UnexpectedDerivationPath
        else Right AddressInfo
            { infoAddressRoot = root
            , infoNetworkTag = ntwrk
            }
  where
    decodePayload :: forall s. CBOR.Decoder s (ByteString, [(Word8, ByteString)])
    decodePayload = do
        _ <- CBOR.decodeListLenCanonicalOf 3
        root <- CBOR.decodeBytes
        (root,) <$> CBOR.decodeAllAttributes

-- | The result of 'eitherInspectAddress' for Icarus addresses.
--
-- @since 3.4.0
data AddressInfo = AddressInfo
    { infoAddressRoot :: !ByteString
    , infoNetworkTag :: !(Maybe NetworkTag)
    } deriving (Generic, Show, Eq)

instance ToJSON AddressInfo where
    toJSON AddressInfo{..} = Json.object
        [ "network_tag" .= maybe Json.Null toJSON infoNetworkTag
        , "address_root" .= T.decodeUtf8 (encode EBase16 infoAddressRoot)
        , "address_type" .= toJSON @Word8 8
        ]

instance Internal.PaymentAddress Icarus where
    paymentAddress discrimination k = unsafeMkAddress
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k) attrs
      where
        NetworkTag magic = networkTag @Icarus discrimination
        attrs = case addressDiscrimination @Icarus discrimination of
            RequiresNetworkTag ->
                [ CBOR.encodeProtocolMagicAttr magic
                ]
            RequiresNoTag ->
                []

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key to a payment 'Address' valid for the given
-- network discrimination.
--
-- @since 1.0.0
paymentAddress
    :: NetworkDiscriminant Icarus
    -> Icarus 'PaymentK XPub
    -> Address
paymentAddress =
    Internal.paymentAddress

--
-- Network Discrimination
--

instance HasNetworkDiscriminant Icarus where
    type NetworkDiscriminant Icarus = (AddressDiscrimination, NetworkTag)
    addressDiscrimination = fst
    networkTag = snd

-- | 'NetworkDiscriminant' for Cardano MainNet & 'Icarus'
--
-- @since 2.0.0
icarusMainnet :: NetworkDiscriminant Icarus
icarusMainnet = byronMainnet

-- | 'NetworkDiscriminant' for Cardano Staging & 'Icarus'
--
-- @since 2.0.0
icarusStaging :: NetworkDiscriminant Icarus
icarusStaging = byronStaging

-- | 'NetworkDiscriminant' for Cardano Testnet & 'Icarus'
--
-- @since 2.0.0
icarusTestnet :: NetworkDiscriminant Icarus
icarusTestnet = byronTestnet

-- | 'NetworkDiscriminant' for Cardano Preprod & 'Icarus'
--
-- @since 3.13.0
icarusPreprod :: NetworkDiscriminant Icarus
icarusPreprod = byronPreprod

-- | 'NetworkDiscriminant' for Cardano Preview & 'Icarus'
--
-- @since 3.13.0
icarusPreview :: NetworkDiscriminant Icarus
icarusPreview = byronPreview

--
-- Unsafe
--

-- | Unsafe backdoor for constructing an 'Icarus' key from a raw 'XPrv'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 1.0.0
liftXPrv :: XPrv -> Icarus depth XPrv
liftXPrv = Icarus

-- | Unsafe backdoor for constructing an 'Icarus' key from a raw 'XPub'. this is
-- unsafe because it lets the caller choose the actually derivation 'depth'.
--
-- This can be useful however when serializing / deserializing such a type, or to
-- speed up test code (and avoid having to do needless derivations from a master
-- key down to an address key for instance).
--
-- @since 2.0.0
liftXPub :: XPub -> Icarus depth XPub
liftXPub = Icarus

--
-- Internal
--

-- Purpose is a constant set to 44' (or 0x8000002C) following the original
-- BIP-44 specification.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000002C

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

-- The minimum seed length for 'generateKeyFromMnemonic' and 'unsafeGenerateKeyFromMnemonic'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- Hardware Ledger devices generates keys from mnemonic using a different
-- approach (different from the rest of Cardano).
--
-- It is a combination of:
--
-- - [SLIP 0010](https://github.com/satoshilabs/slips/blob/master/slip-0010.md)
-- - [BIP 0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
-- - [BIP 0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
-- - [RFC 8032](https://tools.ietf.org/html/rfc8032#section-5.1.5)
-- - What seems to be arbitrary changes from Ledger regarding the calculation of
--   the initial chain code and generation of the root private key.
unsafeGenerateKeyFromHardwareLedger
    :: SomeMnemonic
        -- ^ The root mnemonic
    -> Icarus 'RootK XPrv
unsafeGenerateKeyFromHardwareLedger (SomeMnemonic mw) = unsafeFromRight $ do
    let seed = pbkdf2HmacSha512
            $ T.encodeUtf8
            $ T.intercalate " "
            $ mnemonicToText mw

    -- NOTE
    -- SLIP-0010 refers to `iR` as the chain code. Here however, the chain code
    -- is obtained as a hash of the initial seed whereas iR is used to make part
    -- of the root private key itself.
    let cc = hmacSha256 (BS.pack [1] <> seed)
    let (iL, iR) = first pruneBuffer $ hashRepeatedly seed

    prv <- maybe (Left "invalid xprv") pure $ xprvFromBytes $ iL <> iR <> cc
    pure $ Icarus prv
  where
    -- Errors yielded in the body of 'unsafeGenerateKeyFromHardwareLedger' are
    -- programmer errors (out-of-range byte buffer access or, invalid length for
    -- cryptographic operations). Therefore, we throw badly if we encounter any.
    unsafeFromRight :: Either String a -> a
    unsafeFromRight = either error id

    -- This is the algorithm described in SLIP 0010 for master key generation
    -- with an extra step to discard _some_ of the potential private keys. Why
    -- this extra step remains a mystery as of today.
    --
    --      1. Generate a seed byte sequence S of 512 bits according to BIP-0039.
    --         (done in a previous step, passed as argument).
    --
    --      2. Calculate I = HMAC-SHA512(Key = "ed25519 seed", Data = S)
    --
    --      3. Split I into two 32-byte sequences, IL and IR.
    --
    -- extra *******************************************************************
    -- *                                                                       *
    -- *    3.5 If the third highest bit of the last byte of IL is not zero    *
    -- *        S = I and go back to step 2.                                   *
    -- *                                                                       *
    -- *************************************************************************
    --
    --      4. Use parse256(IL) as master secret key, and IR as master chain code.
    hashRepeatedly :: ByteString -> (ByteString, ByteString)
    hashRepeatedly bytes = case BS.splitAt 32 (hmacSha512 bytes) of
        (iL, iR) | isInvalidKey iL -> hashRepeatedly (iL <> iR)
        (iL, iR) -> (iL, iR)
      where
        isInvalidKey k = testBit (k `BS.index` 31) 5

    -- - Clear the lowest 3 bits of the first byte
    -- - Clear the highest bit of the last byte
    -- - Set the second highest bit of the last byte
    --
    -- As described in [RFC 8032 - 5.1.5](https://tools.ietf.org/html/rfc8032#section-5.1.5)
    pruneBuffer :: ByteString -> ByteString
    pruneBuffer bytes =
        let
            (firstByte, rest) = fromMaybe (error "pruneBuffer: no first byte") $
                BS.uncons bytes

            (rest', lastByte) = fromMaybe (error "pruneBuffer: no last byte") $
                BS.unsnoc rest

            firstPruned = firstByte
                & (`clearBit` 0)
                & (`clearBit` 1)
                & (`clearBit` 2)

            lastPruned = lastByte
                & (`setBit` 6)
                & (`clearBit` 7)
        in
            (firstPruned `BS.cons` BS.snoc rest' lastPruned)

    -- As described in [BIP 0039 - From Mnemonic to Seed](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#from-mnemonic-to-seed)
    pbkdf2HmacSha512 :: ByteString -> ByteString
    pbkdf2HmacSha512 bytes = PBKDF2.generate
        (PBKDF2.prfHMAC SHA512)
        (PBKDF2.Parameters 2048 64)
        bytes
        ("mnemonic" :: ByteString)

    hmacSha256 :: ByteString -> ByteString
    hmacSha256 =
        BA.convert @(HMAC SHA256) . hmac salt

    -- As described in [SLIP 0010 - Master Key Generation](https://github.com/satoshilabs/slips/blob/master/slip-0010.md#master-key-generation)
    hmacSha512 :: ByteString -> ByteString
    hmacSha512 =
        BA.convert @(HMAC SHA512) . hmac salt

    salt :: ByteString
    salt = "ed25519 seed"
