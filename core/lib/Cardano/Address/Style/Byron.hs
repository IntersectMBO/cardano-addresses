{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.Address.Style.Byron
    ( -- $overview

      -- * Byron
      Byron
    , DerivationPath
    , payloadPassphrase
    , derivationPath
    , getKey

      -- * Key Derivation
      -- $keyDerivation
    , genMasterKeyFromXPrv
    , genMasterKeyFromMnemonic
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey

      -- * Addresses
      -- $addresses
    , AddressInfo (..)
    , eitherInspectAddress
    , inspectAddress
    , inspectByronAddress
    , paymentAddress
    , ErrInspectAddress (..)
    , prettyErrInspectAddress

      -- * Network Discrimination
    , byronMainnet
    , byronStaging
    , byronTestnet

      -- * Unsafe
    , liftXPrv
    , liftXPub

      -- Internals
    , minSeedLengthBytes
    ) where

import Prelude

import Cardano.Address
    ( Address
    , AddressDiscrimination (..)
    , HasNetworkDiscriminant (..)
    , NetworkTag (..)
    , unAddress
    , unsafeMkAddress
    )
import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationScheme (DerivationScheme1)
    , DerivationType (..)
    , Index (..)
    , XPrv
    , XPub
    , deriveXPrv
    , generate
    , toXPub
    , xpubToBytes
    )
import Cardano.Address.Internal
    ( DeserialiseFailure, WithErrorMessage (..) )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
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
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256, SHA512 (..) )
import Data.Aeson
    ( ToJSON (..), (.=) )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Kind
    ( Type )
import Data.List
    ( find )
import Data.Word
    ( Word32, Word8 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Address as Internal
import qualified Cardano.Address.Derivation as Internal
import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.Aeson as Json
import qualified Data.ByteArray as BA
import qualified Data.Text.Encoding as T

-- $overview
--
-- This module provides an implementation of:
--
-- - 'Cardano.Address.Derivation.GenMasterKey': for generating Byron master keys from mnemonic sentences
-- - 'Cardano.Address.Derivation.HardDerivation': for hierarchical derivation of parent to child keys
-- - 'Cardano.Address.PaymentAddress': for constructing addresses from a public key
--
-- We call 'Byron' addresses the old address type used by Daedalus in the early
-- days of Cardano. Using this type of addresses and underlying key scheme is
-- now considered __deprecated__ because of some security implications.
--
-- The internals of the 'Byron' does not matter for the reader, but basically
-- contains what is necessary to perform key derivation and generate addresses
-- from a 'Byron' type.
--
-- == Deprecation Notice
--
-- Unless you have good reason to do so (like writing backward-compatible code
-- with an existing piece), any new implementation __should use__ the
-- 'Cardano.Address.Style.Icarus.Icarus' style for key and addresses.


-- | Material for deriving HD random scheme keys, which can be used for making
-- addresses.
--
-- @since 1.0.0
data Byron (depth :: Depth) key = Byron
    { getKey :: key
    -- ^ The raw private or public key.
    --
    -- @since 1.0.0
    , derivationPath :: DerivationPath depth
    -- ^ The address derivation indices for the level of this key.
    --
    -- @since 1.0.0
    , payloadPassphrase :: ScrubbedBytes
    -- ^ Used for encryption of the derivation path payload within an address.
    --
    -- @since 1.0.0
    } deriving stock (Generic)
{-# DEPRECATED Byron "see 'Cardano.Address.Style.Icarus.Icarus'" #-}
{-# DEPRECATED getKey "see 'Cardano.Address.Style.Icarus.Icarus'" #-}
{-# DEPRECATED derivationPath "see 'Cardano.Address.Style.Icarus.Icarus'" #-}
{-# DEPRECATED payloadPassphrase "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

instance (NFData key, NFData (DerivationPath depth)) => NFData (Byron depth key)
deriving instance (Show key, Show (DerivationPath depth)) => Show (Byron depth key)
deriving instance (Eq key, Eq (DerivationPath depth)) => Eq (Byron depth key)
deriving instance (Functor (Byron depth))

-- | The hierarchical derivation indices for a given level/depth.
--
-- @since 1.0.0
type family DerivationPath (depth :: Depth) :: Type where
    -- The root key is generated from the seed.
    DerivationPath 'RootK =
        ()
    -- The account key is generated from the root key and account index.
    DerivationPath 'AccountK =
        Index 'WholeDomain 'AccountK
    -- The address key is generated from the account key and address index.
    DerivationPath 'PaymentK =
        (Index 'WholeDomain 'AccountK, Index 'WholeDomain 'PaymentK)
{-# DEPRECATED DerivationPath "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

--
-- Key Derivation
--
-- === Generating a root key from 'SomeMnemonic'
-- > :set -XOverloadedStrings
-- > :set -XTypeApplications
-- > :set -XDataKinds
-- > import Cardano.Mnemonic ( mkSomeMnemonic )
-- >
-- > let (Right mw) = mkSomeMnemonic @'[12] ["moon","fox","ostrich","quick","cactus","raven","wasp","intact","first","ring","crumble","error"]
-- > let rootK = genMasterKeyFromMnemonic mw :: Byron 'RootK XPrv
--
-- === Deriving child keys
--
-- > let Just accIx = fromWord32 0x80000000
-- > let acctK = deriveAccountPrivateKey rootK accIx
-- >
-- > let Just addIx = fromWord32 0x80000014
-- > let addrK = deriveAddressPrivateKey acctK addIx

instance Internal.GenMasterKey Byron where
    type SecondFactor Byron = ()

    genMasterKeyFromXPrv xprv =
        liftXPrv (toXPub xprv) () xprv
    genMasterKeyFromMnemonic (SomeMnemonic mw) () =
        liftXPrv (toXPub xprv) () xprv
      where
        xprv = generate (hashSeed seedValidated)
        seed  = entropyToBytes $ mnemonicToEntropy mw
        seedValidated = assert
            (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
            seed

instance Internal.HardDerivation Byron where
    type AddressIndexDerivationType Byron = 'WholeDomain
    type AccountIndexDerivationType Byron = 'WholeDomain
    type WithRole Byron = ()

    deriveAccountPrivateKey rootXPrv accIx = Byron
        { getKey = deriveXPrv DerivationScheme1 (getKey rootXPrv) accIx
        , derivationPath = accIx
        , payloadPassphrase = payloadPassphrase rootXPrv
        }

    deriveAddressPrivateKey accXPrv () addrIx = Byron
        { getKey = deriveXPrv DerivationScheme1 (getKey accXPrv) addrIx
        , derivationPath = (derivationPath accXPrv, addrIx)
        , payloadPassphrase = payloadPassphrase accXPrv
        }

-- | Generate a root key from a corresponding mnemonic.
--
-- @since 1.0.0
genMasterKeyFromMnemonic
    :: SomeMnemonic
        -- ^ Some valid mnemonic sentence.
    -> Byron 'RootK XPrv
genMasterKeyFromMnemonic =
    flip Internal.genMasterKeyFromMnemonic ()
{-# DEPRECATED genMasterKeyFromMnemonic "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

-- | Generate a root key from a corresponding root 'XPrv'
--
-- @since 1.0.0
genMasterKeyFromXPrv
    :: XPrv
    -> Byron 'RootK XPrv
genMasterKeyFromXPrv =
    Internal.genMasterKeyFromXPrv
{-# DEPRECATED genMasterKeyFromXPrv "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an account private key from the given root private key.
--
-- @since 1.0.0
deriveAccountPrivateKey
    :: Byron 'RootK XPrv
    -> Index 'WholeDomain 'AccountK
    -> Byron 'AccountK XPrv
deriveAccountPrivateKey =
    Internal.deriveAccountPrivateKey
{-# DEPRECATED deriveAccountPrivateKey "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

-- Re-export from 'Cardano.Address.Derivation' to have it documented specialized in Haddock.
--
-- | Derives an address private key from the given account private key.
--
-- @since 1.0.0
deriveAddressPrivateKey
    :: Byron 'AccountK XPrv
    -> Index 'WholeDomain 'PaymentK
    -> Byron 'PaymentK XPrv
deriveAddressPrivateKey acctK =
    Internal.deriveAddressPrivateKey acctK ()
{-# DEPRECATED deriveAddressPrivateKey "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

--
-- Addresses
--
-- $addresses
-- === Generating a 'PaymentAddress'
--
-- > import Cardano.Address ( base58 )
-- > import Cardano.Address.Derivation ( toXPub(..) )
-- >
-- > base58 $ paymentAddress byronMainnet (toXPub <$> addrK)
-- > "DdzFFzCqrhsq3KjLtT51mESbZ4RepiHPzLqEhamexVFTJpGbCXmh7qSxnHvaL88QmtVTD1E1sjx8Z1ZNDhYmcBV38ZjDST9kYVxSkhcw"

-- | Possible errors from inspecting a Byron address
--
-- @since 3.0.0
data ErrInspectAddress
    = MissingExpectedDerivationPath
    | DeserialiseError DeserialiseFailure
    | FailedToDecryptPath
    deriving (Generic, Show, Eq)
    deriving ToJSON via WithErrorMessage ErrInspectAddress

instance Exception ErrInspectAddress where
  displayException = prettyErrInspectAddress

-- | Pretty-print an 'ErrInspectAddress'
--
-- @since 3.0.0
prettyErrInspectAddress :: ErrInspectAddress -> String
prettyErrInspectAddress = \case
    MissingExpectedDerivationPath ->
        "Missing expected derivation path"
    DeserialiseError e ->
        displayException e
    FailedToDecryptPath ->
        "Failed to decrypt derivation path"

-- Determines whether an 'Address' is a Byron address.
--
-- Returns a JSON object with information about the address, or throws
-- 'ErrInspectAddress' if the address isn't a byron address.
--
-- @since 2.0.0
inspectByronAddress :: forall m. MonadThrow m => Maybe XPub -> Address -> m Json.Value
inspectByronAddress = inspectAddress
{-# DEPRECATED inspectByronAddress "use qualified 'inspectAddress' instead." #-}

-- | Determines whether an 'Address' is a Byron address.
--
-- Returns a JSON object with information about the address, or throws
-- 'ErrInspectAddress' if the address isn't a byron address.
--
-- @since 3.0.0
inspectAddress :: forall m. MonadThrow m => Maybe XPub -> Address -> m Json.Value
inspectAddress mRootPub addr = either throwM (pure . toJSON) $
    eitherInspectAddress mRootPub addr

-- | Determines whether an 'Address' is a Byron address.
--
-- Returns either details about the 'Address', or 'ErrInspectAddress' if it's
-- not a valid address.
--
-- @since 3.4.0
eitherInspectAddress :: Maybe XPub -> Address -> Either ErrInspectAddress AddressInfo
eitherInspectAddress mRootPub addr = do
    payload <- first DeserialiseError $
        CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes

    (root, attrs) <- first DeserialiseError $
        CBOR.deserialiseCbor decodePayload payload

    path <- do
        attr <- maybe (Left MissingExpectedDerivationPath) Right $
            find ((== 1) . fst) attrs
        case mRootPub of
            Nothing -> Right $ EncryptedDerivationPath $ snd attr
            Just rootPub -> decryptPath attr rootPub

    ntwrk <- bimap DeserialiseError (fmap NetworkTag) $
        CBOR.deserialiseCbor CBOR.decodeProtocolMagicAttr payload

    pure AddressInfo
        { infoAddressRoot = root
        , infoPayload = path
        , infoNetworkTag = ntwrk
        }
  where
    bytes :: ByteString
    bytes = unAddress addr

    decodePayload :: forall s. CBOR.Decoder s (ByteString, [(Word8, ByteString)])
    decodePayload = do
        _ <- CBOR.decodeListLenCanonicalOf 3
        root <- CBOR.decodeBytes
        (root,) <$> CBOR.decodeAllAttributes

    decryptPath :: (Word8, ByteString) -> XPub -> Either ErrInspectAddress PayloadInfo
    decryptPath attr rootPub = do
        let pwd = hdPassphrase rootPub
        path <- first (const FailedToDecryptPath) $
            CBOR.deserialiseCbor (CBOR.decodeDerivationPathAttr pwd [attr]) mempty
        case path of
            Nothing -> Left FailedToDecryptPath
            Just (accountIndex, addressIndex) -> Right PayloadDerivationPath{..}

-- | The result of 'eitherInspectAddress' for Byron addresses.
--
-- @since 3.4.0
data AddressInfo = AddressInfo
    { infoAddressRoot :: !ByteString
    , infoPayload :: !PayloadInfo
    , infoNetworkTag :: !(Maybe NetworkTag)
    } deriving (Generic, Show, Eq)

-- | The derivation path in a Byron address payload.
--
-- @since 3.4.0
data PayloadInfo
    = PayloadDerivationPath
        { accountIndex :: !Word32
        , addressIndex :: !Word32
        }
    | EncryptedDerivationPath
        { encryptedDerivationPath :: !ByteString
        }
    deriving (Generic, Show, Eq)

instance ToJSON AddressInfo where
    toJSON AddressInfo{..} = Json.object
        [ "address_root"    .= T.decodeUtf8 (encode EBase16 infoAddressRoot)
        , "derivation_path" .= infoPayload
        , "network_tag"     .= maybe Json.Null toJSON infoNetworkTag
        ]

instance ToJSON PayloadInfo where
    toJSON PayloadDerivationPath{..} = Json.object
        [ "account_index" .= prettyIndex accountIndex
        , "address_index" .= prettyIndex addressIndex
        ]
      where
        prettyIndex :: Word32 -> String
        prettyIndex ix
            | ix >= firstHardened = show (ix - firstHardened) <> "H"
            | otherwise = show ix
          where
            firstHardened = 0x80000000
    toJSON EncryptedDerivationPath{..} = Json.String $
        T.decodeUtf8 $ encode EBase16 encryptedDerivationPath

instance Internal.PaymentAddress Byron where
    paymentAddress discrimination k = unsafeMkAddress
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k) attrs
      where
        (acctIx, addrIx) = bimap indexToWord32 indexToWord32 $ derivationPath k
        pwd = payloadPassphrase k
        NetworkTag magic = networkTag @Byron discrimination
        attrs = case addressDiscrimination @Byron discrimination of
            RequiresNetworkTag ->
                [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx
                , CBOR.encodeProtocolMagicAttr magic
                ]
            RequiresNoTag ->
                [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx
                ]

-- Re-export from 'Cardano.Address' to have it documented specialized in Haddock.
--
-- | Convert a public key to a payment 'Address' valid for the given
-- network discrimination.
--
-- @since 1.0.0
paymentAddress
    :: NetworkDiscriminant Byron
    -> Byron 'PaymentK XPub
    -> Address
paymentAddress =
    Internal.paymentAddress

--
-- Network Discrimination
--

instance HasNetworkDiscriminant Byron where
    type NetworkDiscriminant Byron = (AddressDiscrimination, NetworkTag)
    addressDiscrimination = fst
    networkTag = snd

-- | 'NetworkDiscriminant' for Cardano MainNet & Byron
--
-- @since 2.0.0
byronMainnet :: NetworkDiscriminant Byron
byronMainnet = (RequiresNoTag, NetworkTag 764824073)

-- | 'NetworkDiscriminant' for Cardano Staging & Byron
--
-- @since 2.0.0
byronStaging :: NetworkDiscriminant Byron
byronStaging = (RequiresNetworkTag, NetworkTag 633343913)

-- | 'NetworkDiscriminant' for Cardano TestNet & Byron
--
-- @since 2.0.0
byronTestnet :: NetworkDiscriminant Byron
byronTestnet = (RequiresNetworkTag, NetworkTag 1097911063)

--
-- Unsafe
--

-- | Backdoor for generating a new key from a raw 'XPrv'.
--
-- Note that the @depth@ is left open so that the caller gets to decide what type
-- of key this is. This is mostly for testing, in practice, seeds are used to
-- represent root keys, and one should 'genMasterKeyFromXPrv'
--
-- The first argument is a type-family 'DerivationPath' and its type depends on
-- the 'depth' of the key.
--
-- __examples:__
--
-- >>> liftXPrv rootPrv () prv
-- _ :: Byron RootK XPrv
--
-- >>> liftXPrv rootPrv minBound prv
-- _ :: Byron AccountK XPrv
--
-- >>> liftXPrv rootPrv (minBound, minBound) prv
-- _ :: Byron PaymentK XPrv
--
-- @since 2.0.0
liftXPrv
    :: XPub -- ^ A root public key
    -> DerivationPath depth
    -> XPrv
    -> Byron depth XPrv
liftXPrv rootPub derivationPath getKey = Byron
    { getKey
    , derivationPath
    , payloadPassphrase = hdPassphrase rootPub
    }
{-# DEPRECATED liftXPrv "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

-- | Backdoor for generating a new key from a raw 'XPub'.
--
-- Note that the @depth@ is left open so that the caller gets to decide what type
-- of key this is. This is mostly for testing, in practice, seeds are used to
-- represent root keys, and one should 'genMasterKeyFromXPrv'
--
-- see also 'liftXPrv'
--
-- @since 2.0.0
liftXPub
    :: XPub -- ^ A root public key
    -> DerivationPath depth
    -> XPub
    -> Byron depth XPub
liftXPub rootPub derivationPath getKey = Byron
    { getKey
    , derivationPath
    , payloadPassphrase = hdPassphrase rootPub
    }
{-# DEPRECATED liftXPub "see 'Cardano.Address.Style.Icarus.Icarus'" #-}

--
-- Internal
--

-- The amount of entropy carried by a BIP-39 12-word mnemonic is 16 bytes.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- Hash the seed entropy (generated from mnemonic) used to initiate a HD
-- wallet. This increases the key length to 34 bytes, selectKey is greater than the
-- minimum for 'generate' (32 bytes).
--
-- Note that our current implementation deviates from BIP-39 because we use a
-- hash function (Blake2b) rather than key stretching with PBKDF2.
--
-- There are two methods of hashing the seed entropy, for different use cases.
--
-- 1. Normal random derivation wallet seeds. The seed entropy is hashed using
--    Blake2b_256, inside a double CBOR serialization sandwich.
--
-- 2. Seeds for redeeming paper wallets. The seed entropy is hashed using
--    Blake2b_256, without any serialization.
hashSeed :: ScrubbedBytes -> ScrubbedBytes
hashSeed = serialize . blake2b256 . serialize
  where
    serialize = BA.convert . cbor . BA.convert
    cbor = CBOR.toStrictByteString . CBOR.encodeBytes

-- Hash a byte string through blake2b 256
blake2b256 :: ScrubbedBytes -> ScrubbedBytes
blake2b256 = BA.convert . hash @ScrubbedBytes @Blake2b_256

-- Derive a symmetric key for encrypting and authenticating the address
-- derivation path. PBKDF2 encryption using HMAC with the hash algorithm SHA512
-- is employed.
hdPassphrase :: XPub -> ScrubbedBytes
hdPassphrase masterKey =
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    (xpubToBytes masterKey)
    ("address-hashing" :: ByteString)
