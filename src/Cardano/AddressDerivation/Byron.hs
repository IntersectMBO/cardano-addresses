{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for the random scheme, as
-- implemented by the legacy Cardano wallets.
--
-- For full documentation of the key derivation schemes,
-- see the "Cardano.Crypto.Wallet" module, and the implementation in
-- <https://github.com/input-output-hk/cardano-crypto/blob/4590efa638397e952a51a8994b5543e4ea3c1ecd/cbits/encrypted_sign.c cardano-crypto>.

module Cardano.AddressDerivation.Byron
    ( -- * Types
      Byron(..)

      -- * Generation
    , unsafeGenerateKeyFromSeed
    , minSeedLengthBytes
    , mkByronKeyFromMasterKey
    , unsafeMkByronKeyFromMasterKey

    ) where

import Prelude

import Cardano.AddressDerivation
    ( Address (..)
    , Depth (..)
    , DerivationType (..)
    , GenMasterKey (..)
    , HardDerivation (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , ProtocolMagic (..)
    , testnetMagic
    )
import Cardano.Crypto.Wallet
    ( ChainCode (..)
    , DerivationScheme (DerivationScheme1)
    , XPrv
    , XPub (..)
    , deriveXPrv
    , generate
    , toXPub
    , unXPub
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Control.DeepSeq
    ( NFData )
import Control.Exception.Base
    ( assert )
import Crypto.Error
    ( CryptoFailable (..) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224, Blake2b_256, SHA3_256, SHA512 (..) )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Digest.CRC32
    ( crc32 )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}

-- | Material for deriving HD random scheme keys, which can be used for making
-- addresses.
data Byron (depth :: Depth) key = Byron
    { getKey :: key
    -- ^ The raw private or public key.
    , derivationPath :: DerivationPath depth
    -- ^ The address derivation indices for the level of this key.
    , payloadPassphrase :: ScrubbedBytes
    -- ^ Used for encryption of payload containing address derivation path.
    } deriving stock (Generic)

instance (NFData key, NFData (DerivationPath depth)) => NFData (Byron depth key)
deriving instance (Show key, Show (DerivationPath depth)) => Show (Byron depth key)
deriving instance (Eq key, Eq (DerivationPath depth)) => Eq (Byron depth key)

-- | The hierarchical derivation indices for a given level/depth.
type family DerivationPath (depth :: Depth) :: * where
    -- The root key is generated from the seed.
    DerivationPath 'RootK =
        ()
    -- The account key is generated from the root key and account index.
    DerivationPath 'AccountK =
        Index 'WholeDomain 'AccountK
    -- The address key is generated from the account key and address index.
    DerivationPath 'AddressK =
        (Index 'WholeDomain 'AccountK, Index 'WholeDomain 'AddressK)

instance GenMasterKey Byron where
    type GenMasterKeyFrom Byron = SomeMnemonic

    genMasterKey = generateKeyFromSeed

instance HardDerivation Byron where
    type AddressIndexDerivationType Byron = 'WholeDomain
    type AccountIndexDerivationType Byron = 'WholeDomain

    deriveAccountPrivateKey pwd rootXPrv idx@(Index accIx) = Byron
        { getKey = deriveXPrv DerivationScheme1 pwd (getKey rootXPrv) accIx
        , derivationPath = idx
        , payloadPassphrase = payloadPassphrase rootXPrv
        }

    deriveAddressPrivateKey pwd accXPrv _accStyle idx@(Index addrIx) = Byron
        { getKey = deriveXPrv DerivationScheme1 pwd (getKey accXPrv) addrIx
        , derivationPath = (derivationPath accXPrv, idx)
        , payloadPassphrase = payloadPassphrase accXPrv
        }

instance KnownNat pm => PaymentAddress ('Testnet pm) Byron where
    paymentAddress k = Address
        $ CBOR.toStrictByteString
        $ encodeAddress (getKey k)
            [ encodeDerivationPathAttr pwd acctIx addrIx
            , encodeProtocolMagicAttr (testnetMagic @pm)
            ]
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k

instance PaymentAddress 'Mainnet Byron where
    paymentAddress k = Address
        $ CBOR.toStrictByteString
        $ encodeAddress (getKey k)
            [ encodeDerivationPathAttr pwd acctIx addrIx ]
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | The amount of entropy carried by a BIP-39 12-word mnemonic is 16 bytes.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: SomeMnemonic
    -> ScrubbedBytes
    -> Byron 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed ()

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: DerivationPath depth
    -> SomeMnemonic
    -> ScrubbedBytes
    -> Byron depth XPrv
unsafeGenerateKeyFromSeed derivationPath (SomeMnemonic mw) pwd = Byron
    { getKey = masterKey
    , derivationPath
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }
  where
    masterKey = generate (hashSeed seedValidated) pwd
    seed  = entropyToBytes $ mnemonicToEntropy mw
    seedValidated = assert
        (BA.length seed >= minSeedLengthBytes && BA.length seed <= 255)
        seed

-- | Hash the seed entropy (generated from mnemonic) used to initiate a HD
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

blake2b256 :: ScrubbedBytes -> ScrubbedBytes
blake2b256 = BA.convert . hash @ScrubbedBytes @Blake2b_256

-- | Derive a symmetric key for encrypting and authenticating the address
-- derivation path. PBKDF2 encryption using HMAC with the hash algorithm SHA512
-- is employed.
hdPassphrase :: XPub -> ScrubbedBytes
hdPassphrase masterKey =
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    (unXPub masterKey)
    ("address-hashing" :: ByteString)

mkByronKeyFromMasterKey
    :: XPrv
    -> Byron 'RootK XPrv
mkByronKeyFromMasterKey = unsafeMkByronKeyFromMasterKey ()

unsafeMkByronKeyFromMasterKey
    :: DerivationPath depth
    -> XPrv
    -> Byron depth XPrv
unsafeMkByronKeyFromMasterKey derivationPath masterKey = Byron
    { getKey = masterKey
    , derivationPath
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }

{-------------------------------------------------------------------------------
                                 CBOR
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
                       Byron Address Binary Format

In the composition of a Cardano address, the following functions concern the
"Derivation Path" box.

+-------------------------------------------------------------------------------+
|                                                                               |
|                        CBOR-Serialized Object with CRC¹                       |
|                                                                               |
+-------------------------------------------------------------------------------+
                                        |
                                        |
                                        v
+-------------------------------------------------------------------------------+
|     Address Root    |     Address Attributes    |           AddrType          |
|                     |                           |                             |
|   Hash (224 bits)   |  Der. Path² + Stake + NM  |  PubKey | (Script) | Redeem |
|                     |    (open for extension)   |     (open for extension)    |
+-------------------------------------------------------------------------------+
             |                 |
             |                 |     +----------------------------------+
             v                 |     |        Derivation Path           |
+---------------------------+  |---->|                                  |
| SHA3-256                  |  |     | ChaChaPoly⁴ AccountIx/AddressIx  |
|   |> Blake2b 224          |  |     +----------------------------------+
|   |> CBOR                 |  |
|                           |  |
|  -AddrType                |  |     +----------------------------------+
|  -ASD³ (~AddrType+PubKey) |  |     |       Stake Distribution         |
|  -Address Attributes      |  |     |                                  |
+---------------------------+  |---->|  BootstrapEra | (Single | Multi) |
                               |     +----------------------------------+
                               |
                               |
                               |     +----------------------------------+
                               |     |          Network Magic           |
                               |---->|                                  |
                                     | Addr Discr: MainNet vs TestNet   |
                                     +----------------------------------+

-------------------------------------------------------------------------------}

-- * Encoding

-- | Encode a public key to a corresponding Cardano Address. The encoding of the
-- attributes part of an address is left out to the caller; This allows for
-- distinguishing between Sequential and Random addresses (the former doesn't
-- have any attributes to encode).
--
-- @
-- -- Old / Random Addresses
-- let encodeAddrAttributes = mempty
--      <> CBOR.encodeMapLen 1
--      <> CBOR.encodeWord8 1
--      <> encodeDerivationPath (hdPassphrase rootXPub) accIx addrIx
-- let addr = encodeAddress xpub encodeAddrAttributes
--
-- -- New / Sequential Addresses
-- let encodeAddrAttributes = mempty <> CBOR.encodeMapLen 0
-- let addr = encodeAddress xpub encodeAddrAttributes
-- @
--
-- Note that we are passing the behavior to encode attributes as a parameter
-- here and do not handle multiple cases in 'encodeAddress' itself for multiple
-- reasons:
--
-- - Inversion of control gives us a nicer implementation overall
--
-- - Encoding attributes for Random addresses requires more context than just
--   the public key (like the wallet root id and some extra logic for encoding
--   passphrases). This is just scheme-specific and is better left out of this
--   particular function
encodeAddress :: XPub -> [CBOR.Encoding] -> CBOR.Encoding
encodeAddress (XPub pub (ChainCode cc)) attrs =
    encodeAddressPayload payload
  where
    blake2b224 = hash @_ @Blake2b_224
    sha3256 = hash @_ @SHA3_256
    payload = CBOR.toStrictByteString $ mempty
        <> CBOR.encodeListLen 3
        <> CBOR.encodeBytes root
        <> encodeAttributes attrs
        <> CBOR.encodeWord8 0 -- Address Type, 0 = Public Key
    root = BA.convert $ blake2b224 $ sha3256 $ CBOR.toStrictByteString $ mempty
        <> CBOR.encodeListLen 3
        <> CBOR.encodeWord8 0 -- Address Type, 0 = Public Key
        <> encodeSpendingData
        <> encodeAttributes attrs
    encodeXPub =
        CBOR.encodeBytes (pub <> cc)
    encodeSpendingData = CBOR.encodeListLen 2
        <> CBOR.encodeWord8 0
        <> encodeXPub

encodeAddressPayload :: ByteString -> CBOR.Encoding
encodeAddressPayload payload = mempty
    <> CBOR.encodeListLen 2
    <> CBOR.encodeTag 24 -- Hard-Coded Tag value in cardano-sl
    <> CBOR.encodeBytes payload
    <> CBOR.encodeWord32 (crc32 payload)

encodeAttributes :: [CBOR.Encoding] -> CBOR.Encoding
encodeAttributes attrs = CBOR.encodeMapLen l <> mconcat attrs
  where
    l = fromIntegral (length attrs)

encodeProtocolMagicAttr :: ProtocolMagic -> CBOR.Encoding
encodeProtocolMagicAttr pm = mempty
    <> CBOR.encodeWord 2 -- Tag for 'ProtocolMagic' attribute
    <> CBOR.encodeBytes (CBOR.toStrictByteString $ encodeProtocolMagic pm)

encodeProtocolMagic :: ProtocolMagic -> CBOR.Encoding
encodeProtocolMagic (ProtocolMagic i) = CBOR.encodeInt32 i

-- This is the opposite of 'decodeDerivationPathAttr'.
--
-- NOTE: The caller must ensure that the passphrase length is 32 bytes.
encodeDerivationPathAttr
    :: ScrubbedBytes
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> CBOR.Encoding
encodeDerivationPathAttr pwd acctIx addrIx = mempty
    <> CBOR.encodeWord8 1 -- Tag for 'DerivationPath' attribute
    <> CBOR.encodeBytes (encryptDerivationPath pwd path)
  where
    path = encodeDerivationPath acctIx addrIx

encodeDerivationPath
    :: Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> CBOR.Encoding
encodeDerivationPath (Index acctIx) (Index addrIx) = mempty
    <> CBOR.encodeListLenIndef
    <> CBOR.encodeWord32 acctIx
    <> CBOR.encodeWord32 addrIx
    <> CBOR.encodeBreak

-- | ChaCha20/Poly1305 encrypting and signing the HD payload of addresses.
--
-- NOTE: The caller must ensure that the passphrase length is 32 bytes.
encryptDerivationPath
    :: ScrubbedBytes
       -- ^ Symmetric key / passphrase, 32-byte long
    -> CBOR.Encoding
        -- ^ Payload to be encrypted
    -> ByteString
        -- ^ Ciphertext with a 128-bit crypto-tag appended.
encryptDerivationPath pwd payload = unsafeSerialize $ do
    nonce <- Poly.nonce12 cardanoNonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize pwd nonce
    let (out, st2) = Poly.encrypt (CBOR.toStrictByteString payload) st1
    return $ out <> BA.convert (Poly.finalize st2)
  where
    unsafeSerialize :: CryptoFailable ByteString -> ByteString
    unsafeSerialize =
        CBOR.toStrictByteString . CBOR.encodeBytes . useInvariant

    -- Encryption will fail if the key is the wrong size, but that won't happen
    -- if the key was created with 'generateKeyFromSeed'.
    useInvariant = \case
        CryptoPassed res -> res
        CryptoFailed err -> error $ "encodeAddressKey: " ++ show err

    -- | Hard-coded nonce from the legacy code-base.
    cardanoNonce :: ByteString
    cardanoNonce = "serokellfore"
