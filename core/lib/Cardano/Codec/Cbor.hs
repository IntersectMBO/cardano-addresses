{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- These are (partial) CBOR decoders for Byron binary types. Note that we
-- ignore most of the block's and header's content and only retrieve the pieces
-- of information relevant to us, wallet (we do assume a trusted node and
-- therefore, we needn't to care about verifying signatures and blocks
-- themselves).

module Cardano.Codec.Cbor
    ( -- * Encoders
      encodeAddress
    , encodeAttributes
    , encodeDerivationPathAttr
    , encodeProtocolMagicAttr

     -- * Decoders
    , decodeAddress
    , decodeAddressDerivationPath
    , decodeAddressPayload
    , decodeAllAttributes
    , decodeDerivationPathAttr
    , decodeProtocolMagicAttr
    , deserialiseCbor
    , unsafeDeserialiseCbor

     -- * Reexports from CBOR
    , CBOR.encodeBytes
    , CBOR.toStrictByteString
    , CBOR.toLazyByteString
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( ChainCode (..), XPub (..) )
import Control.Monad
    ( replicateM, when )
import Control.Monad.Catch
    ( MonadThrow, throwM )
import Control.Monad.Fail
    ( MonadFail )
import Crypto.Error
    ( CryptoError (..), CryptoFailable (..) )
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
import Data.List
    ( find )
import Data.Word
    ( Word32, Word8 )
import GHC.Stack
    ( HasCallStack )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

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

encodeProtocolMagicAttr :: Word32 -> CBOR.Encoding
encodeProtocolMagicAttr pm = mempty
    <> CBOR.encodeWord 2 -- Tag for 'ProtocolMagic' attribute
    <> CBOR.encodeBytes (CBOR.toStrictByteString $ CBOR.encodeWord32 pm)

-- This is the opposite of 'decodeDerivationPathAttr'.
--
-- NOTE: The caller must ensure that the passphrase length is 32 bytes.
encodeDerivationPathAttr
    :: ScrubbedBytes
    -> Word32
    -> Word32
    -> CBOR.Encoding
encodeDerivationPathAttr pwd acctIx addrIx = mempty
    <> CBOR.encodeWord8 1 -- Tag for 'DerivationPath' attribute
    <> CBOR.encodeBytes (encryptDerivationPath pwd path)
  where
    path = encodeDerivationPath acctIx addrIx

encodeDerivationPath
    :: Word32
    -> Word32
    -> CBOR.Encoding
encodeDerivationPath acctIx addrIx = mempty
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

decodeAddress :: CBOR.Decoder s ByteString
decodeAddress = do
    _ <- CBOR.decodeListLenCanonicalOf 2
        -- CRC Protection Wrapper
    tag <- CBOR.decodeTag
        -- Mysterious hard-coded tag cardano-sl seems to so much like
    bytes <- CBOR.decodeBytes
        -- Addr Root + Attributes + Type
    crc <- CBOR.decodeWord32 -- CRC
    -- NOTE 1:
    -- Treating addresses as a blob here, so we just re-encode them as such
    -- Ultimately for us, addresses are nothing more than a bunch of bytes that
    -- we display in a Base58 format when we have to.
    --
    -- NOTE 2:
    -- We may want to check the CRC at this level as-well... maybe not.
    return $ CBOR.toStrictByteString $ mempty
        <> CBOR.encodeListLen 2
        <> CBOR.encodeTag tag
        <> CBOR.encodeBytes bytes
        <> CBOR.encodeWord32 crc

decodeAddressPayload :: CBOR.Decoder s ByteString
decodeAddressPayload = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- CBOR.decodeTag
    bytes <- CBOR.decodeBytes
    _ <- CBOR.decodeWord32 -- CRC
    return bytes

decodeAddressDerivationPath
    :: ScrubbedBytes
    -> CBOR.Decoder s (Maybe (Word32, Word32))
decodeAddressDerivationPath pwd = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    _ <- CBOR.decodeBytes
    path <- decodeAllAttributes >>= decodeDerivationPathAttr pwd
    addrType <- CBOR.decodeWord8 -- Type
    when (addrType /= 0) $
        fail $ mconcat
            [ "decodeAddressDerivationPath: type is not 0 (public key), it is "
            , show addrType
            ]
    pure path

decodeProtocolMagicAttr
    :: CBOR.Decoder s (Maybe Word32)
decodeProtocolMagicAttr = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    _ <- CBOR.decodeBytes
    attrs <- decodeAllAttributes
    case find ((== 2) . fst) attrs of
        Nothing -> pure Nothing
        Just (_, bytes) -> case deserialiseCbor CBOR.decodeWord32 bytes of
            Nothing -> fail "unable to decode attribute into protocol magic"
            Just pm -> pure (Just pm)

-- | The attributes are pairs of numeric tags and bytes, where the bytes will be
-- CBOR-encoded stuff. This decoder does not enforce "canonicity" of entries.
decodeAllAttributes
    :: CBOR.Decoder s [(Word8, ByteString)]
decodeAllAttributes = do
    n <- CBOR.decodeMapLenCanonical -- Address Attributes length
    replicateM n decodeAttr
  where
    decodeAttr = (,) <$> CBOR.decodeWord8 <*> CBOR.decodeBytes

decodeDerivationPathAttr
    :: ScrubbedBytes
    -> [(Word8, ByteString)]
    -> CBOR.Decoder s (Maybe (Word32, Word32))
decodeDerivationPathAttr pwd attrs = do
    case lookup derPathTag attrs of
        Just payload -> do
            decodeNestedBytes decoder payload
        Nothing -> fail $ mconcat
            [ "decodeDerivationPathAttr: Missing attribute "
            , show derPathTag
            ]
  where
    derPathTag = 1
    decoder :: CBOR.Decoder s (Maybe (Word32, Word32))
    decoder = do
        bytes <- CBOR.decodeBytes
        case decryptDerivationPath pwd bytes of
            CryptoPassed plaintext ->
                Just <$> decodeNestedBytes decodeDerivationPath plaintext
            CryptoFailed _ ->
                pure Nothing

-- | ChaCha20/Poly1305 decrypting and authenticating the HD payload of
-- addresses.
decryptDerivationPath
    :: ScrubbedBytes
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString
        -- ^ Payload to be decrypted
    -> CryptoFailable ByteString
decryptDerivationPath pwd bytes = do
    let (payload, tag) = BS.splitAt (BS.length bytes - 16) bytes
    nonce <- Poly.nonce12 cardanoNonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize pwd nonce
    let (out, st2) = Poly.decrypt payload st1
    when (BA.convert (Poly.finalize st2) /= tag) $
        CryptoFailed CryptoError_MacKeyInvalid
    return out

-- Opposite of 'encodeDerivationPath'.
decodeDerivationPath
    :: CBOR.Decoder s (Word32, Word32)
decodeDerivationPath = do
    ixs <- decodeListIndef CBOR.decodeWord32
    case ixs of
        [acctIx, addrIx] ->
            pure (acctIx, addrIx)
        _ ->
            fail $ mconcat
                [ "decodeDerivationPath: invalid derivation path payload: "
                , "expected two indexes but got: "
                , show ixs
                ]
-- | Decode an arbitrary long list. CBOR introduce a "break" character to
-- mark the end of the list, so we simply decode each item until we encounter
-- a break character.
--
-- @
--     myDecoder :: CBOR.Decoder s [MyType]
--     myDecoder = decodeListIndef decodeOne
--       where
--         decodeOne :: CBOR.Decoder s MyType
-- @
decodeListIndef :: forall s a. CBOR.Decoder s a -> CBOR.Decoder s [a]
decodeListIndef decodeOne = do
    _ <- CBOR.decodeListLenIndef
    CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeOne

-- | Byron CBOR encodings often have CBOR nested in CBOR. This helps decoding
-- a particular 'ByteString' that represents a CBOR object.
decodeNestedBytes
    :: MonadFail m
    => (forall s. CBOR.Decoder s r)
    -> ByteString
    -> m r
decodeNestedBytes dec bytes =
    case CBOR.deserialiseFromBytes dec (BL.fromStrict bytes) of
        Right ("", res) ->
            pure res
        Right _ ->
            fail "Leftovers when decoding nested bytes"
        _ ->
            fail "Could not decode nested bytes"

-- | Shortcut for deserialising a strict 'Bytestring' with the given decoder.
deserialiseCbor
    :: MonadThrow m
    => (forall s. CBOR.Decoder s a)
    -> ByteString
    -> m a
deserialiseCbor dec =
  fmap snd . either throwM pure . CBOR.deserialiseFromBytes dec . BL.fromStrict

-- | CBOR deserialise without error handling - handy for prototypes or testing.
unsafeDeserialiseCbor
    :: HasCallStack
    => (forall s. CBOR.Decoder s a)
    -> BL.ByteString
    -> a
unsafeDeserialiseCbor decoder bytes = either
    (\e -> error $ "unsafeSerializeCbor: " <> show e)
    snd
    (CBOR.deserialiseFromBytes decoder bytes)

