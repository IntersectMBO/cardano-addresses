{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- These are (partial) CBOR decoders for Byron binary types. Note that we
-- ignore most of the block's and header's content and only retrieve the pieces
-- of information relevant to us, wallet (we do assume a trusted node and
-- therefore, we needn't to care about verifying signatures and blocks
-- themselves).

module Cardano.AddressDerivation.Cbor
    ( -- * Encoders
      encodeAddress
    , encodeDerivationPathAttr
    , encodeProtocolMagicAttr

     -- * Reexports from CBOR
    , CBOR.encodeBytes
    , CBOR.toStrictByteString
    ) where

import Prelude

import Cardano.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..), ProtocolMagic (..) )
import Cardano.Crypto.Wallet
    ( ChainCode (..), XPub (..) )
import Crypto.Error
    ( CryptoFailable (..) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224, SHA3_256 )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Digest.CRC32
    ( crc32 )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Data.ByteArray as BA

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
