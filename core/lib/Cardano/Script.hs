{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Script
    (
    -- * Script
      Script (..)
    , validateScript
    , ErrValidateScript (..)
    , prettyErrValidateScript
    , bech32

    -- * Hashing
    , serialize

    , ScriptHash (..)
    , scriptHashFromBytes
    , toScriptHash

    , KeyHash (..)
    , keyHashFromBytes
    , keyHashFromText

    -- * Internal
    , hashSize
    ) where

import Prelude

import Codec.Binary.Encoding
    ( AbstractEncoding (..), detectEncoding, encode, fromBase16, fromBase58 )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( when )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.=)
    )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Foldable
    ( foldl', traverse_ )
import Data.Maybe
    ( isNothing )
import Data.Text
    ( Text )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.Aeson.Types as Json
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- | A 'Script' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data Script
    = RequireSignatureOf !KeyHash
    | RequireAllOf ![Script]
    | RequireAnyOf ![Script]
    | RequireMOf Word8 ![Script]
    deriving stock (Generic, Show, Eq)
instance NFData Script

-- | Validate 'Script'
--
-- @since 3.0.0
validateScript :: Script -> Either ErrValidateScript ()
validateScript = \case
    RequireSignatureOf (KeyHash bytes) -> do
        when (isNothing (keyHashFromBytes bytes)) $ Left WrongKeyHash

    RequireAllOf script -> do
        when (L.null script) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script

    RequireAnyOf script -> do
        when (L.null script) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script

    RequireMOf m script -> do
        when (m == 0) $ Left MZero
        when (length script < fromIntegral m) $ Left ListTooSmall
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script
  where
    hasDuplicate xs = do
        length sigs /= length (L.nub sigs)
      where
        sigs = [ sig | RequireSignatureOf sig <- xs ]

-- | Possible validation errors when validating a script
data ErrValidateScript
    = EmptyList
    | ListTooSmall
    | MZero
    | DuplicateSignatures
    | WrongKeyHash
    deriving (Eq, Show)

prettyErrValidateScript
    :: ErrValidateScript
    -> String
prettyErrValidateScript = \case
    EmptyList ->
        "The list inside a script is empty."
    MZero ->
        "The M in at_least cannot be 0."
    ListTooSmall ->
        "The list inside at_least cannot be less than M."
    DuplicateSignatures ->
        "The list inside a script has duplicate keys."
    WrongKeyHash ->
        "The hash of verification key is expected to have "<>show hashSize<>" bytes."

-- | This function realizes what cardano-node's `Api.serialiseToCBOR script` realizes
-- This is basically doing the symbolically following:
-- toCBOR [0,multisigScript]
serialize :: Script -> ByteString
serialize script =
    multisigTag <> CBOR.toStrictByteString (toCBOR script)
  where
    -- | Magic number representing the tag of the native multi-signature script
    -- language. For each script language included, a new tag is chosen.
    multisigTag :: ByteString
    multisigTag = "\00"

    toCBOR :: Script -> CBOR.Encoding
    toCBOR = \case
        RequireSignatureOf (KeyHash verKeyHash) ->
            encodeMultiscriptCtr 0 2 <> CBOR.encodeBytes verKeyHash
        RequireAllOf contents ->
            encodeMultiscriptCtr 1 2 <> encodeFoldable toCBOR contents
        RequireAnyOf contents ->
            encodeMultiscriptCtr 2 2 <> encodeFoldable toCBOR contents
        RequireMOf m contents -> mconcat
            [ encodeMultiscriptCtr 3 3
            , CBOR.encodeInt (fromInteger $ toInteger m)
            , encodeFoldable toCBOR contents
            ]

    encodeMultiscriptCtr :: Word -> Word -> CBOR.Encoding
    encodeMultiscriptCtr ctrIndex listLen =
        CBOR.encodeListLen listLen <> CBOR.encodeWord ctrIndex

    encodeFoldable :: (Foldable f) => (a -> CBOR.Encoding) -> f a -> CBOR.Encoding
    encodeFoldable encode' xs = wrapArray len contents
      where
        (len, contents) = foldl' go (0, mempty) xs
        go (!l, !enc) next = (l + 1, enc <> encode' next)

        wrapArray :: Word -> CBOR.Encoding -> CBOR.Encoding
        wrapArray len' contents'
            | len' <= 23 = CBOR.encodeListLen len' <> contents'
            | otherwise  = CBOR.encodeListLenIndef <> contents' <> CBOR.encodeBreak

-- | Computes the hash of a given script, by first serializing it to CBOR.
toScriptHash :: Script -> ScriptHash
toScriptHash = ScriptHash . blake2b224 . serialize

-- | A 'ScriptHash' type represents script hash. The hash is expected to have size of
-- 28-byte.
--
-- @since 3.0.0
newtype ScriptHash = ScriptHash ByteString
    deriving (Generic, Show, Eq)
instance NFData ScriptHash

-- | Construct an 'ScriptHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
scriptHashFromBytes :: ByteString -> Maybe ScriptHash
scriptHashFromBytes bytes
    | BS.length bytes /= hashSize = Nothing
    | otherwise = Just $ ScriptHash bytes

-- | A 'KeyHash' type represents verification key hash that participate in building
-- multi-signature script. The hash is expected to have size of 28-byte.
--
-- @since 3.0.0
newtype KeyHash = KeyHash ByteString
    deriving (Generic, Show, Eq)
instance NFData KeyHash

-- | Construct an 'KeyHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
keyHashFromBytes :: ByteString -> Maybe KeyHash
keyHashFromBytes bytes
    | BS.length bytes /= hashSize = Nothing
    | otherwise = Just $ KeyHash bytes

data ErrorKeyHashFromText =
      ErrorKeyHashFromTextInvalidString (AbstractEncoding ())
    | ErrorKeyHashFromTextWrongEncoding
    | ErrorKeyHashFromTextWrongPayload
    | ErrorKeyHashFromTextWrongHrp
    | ErrorKeyHashFromTextWrongDataPart
    deriving (Show, Eq)

showErr :: ErrorKeyHashFromText -> String
showErr (ErrorKeyHashFromTextInvalidString EBase16) =
    "Invalid Base16-encoded string."
showErr (ErrorKeyHashFromTextInvalidString EBech32{}) =
    "Invalid Bech32-encoded string."
showErr (ErrorKeyHashFromTextInvalidString EBase58) =
    "Invalid Base58-encoded string."
showErr ErrorKeyHashFromTextWrongEncoding =
    "Verification key hash must be must be encoded as \
    \base16, bech32 or base58."
showErr ErrorKeyHashFromTextWrongPayload =
    "Verification key hash must contain exactly 28 bytes."
showErr ErrorKeyHashFromTextWrongHrp =
    "Verification key hash must have 'script_vkh' hrp when Bech32-encoded."
showErr ErrorKeyHashFromTextWrongDataPart =
    "Verification key hash is Bech32-encoded but has wrong data part."

-- | Construct a 'KeyHash' from 'Text'. Either hex encoded text or
-- Bech32 encoded text with `script_vkh` hrp is expected. Also
-- binary payload is expected to be composed of 28 bytes.
--
-- @since 3.0.0
keyHashFromText :: Text -> Either ErrorKeyHashFromText KeyHash
keyHashFromText txt =  case detectEncoding str of
        Just EBase16 -> case fromBase16 (toBytes str) of
            Left _ -> Left $ ErrorKeyHashFromTextInvalidString EBase16
            Right bytes -> checkPayload bytes
        Just EBech32{} -> fromBech32
        Just EBase58 -> case fromBase58 (toBytes str) of
            Left _ -> Left $ ErrorKeyHashFromTextInvalidString EBase58
            Right bytes -> checkPayload bytes
        Nothing -> Left ErrorKeyHashFromTextWrongEncoding
 where
    str = T.unpack txt
    toBytes = T.encodeUtf8 . T.pack
    checkPayload bytes =
        maybeToRight ErrorKeyHashFromTextWrongPayload (keyHashFromBytes bytes)
    fromBech32 = do
        (hrp, dp) <- either
            (const $ Left $ ErrorKeyHashFromTextInvalidString $ EBech32 ())
            Right (Bech32.decodeLenient txt)
        case Bech32.humanReadablePartToText hrp of
            "script_vkh" -> do
                bytes <- maybeToRight
                    ErrorKeyHashFromTextWrongDataPart (Bech32.dataPartToBytes dp)
                checkPayload bytes
            _ -> Left ErrorKeyHashFromTextWrongHrp

--
-- Internal
--

-- Hash a public key
blake2b224 :: ByteString -> ByteString
blake2b224 =
    BA.convert . hash @_ @Blake2b_224

-- Size, in bytes, of a hash of public key (without the corresponding chain code)
hashSize :: Int
hashSize = hashDigestSize Blake2b_224

-- | Encode a 'KeyHash' to bech32 'Text', using @script_vkh@ as a human readable prefix.
--
-- @since 3.0.0
bech32 :: KeyHash -> Text
bech32 (KeyHash keyHash) = T.decodeUtf8 $ encode (EBech32 hrp) keyHash
  where
    hrp = [Bech32.humanReadablePart|script_vkh|]

-- Examples of Script jsons:
--"e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--{ "all" : [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--          , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735b"
--          ]
--}
--{ "all" : [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--          , {"any": [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735b"
--                    , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735c"
--                    ]
--            }
--          ]
--}
--{ "all" : [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--          , {"at_least": { "from" :[ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735b"
--                                   , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735c"
--                                   , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735d"
--                                   ]
--                         , "m" : 2
--                         }
--            }
--          ]
--}

instance ToJSON Script where
    toJSON (RequireSignatureOf keyHash) = String $ bech32 keyHash
    toJSON (RequireAllOf content) =
        object ["all" .= fmap toJSON content]
    toJSON (RequireAnyOf content) =
        object ["any" .= fmap toJSON content]
    toJSON (RequireMOf m content) =
        let inside = Object ("from" .= fmap toJSON content <> "m" .= toJSON m)
        in object ["at_least" .= inside]

instance FromJSON Script where
    parseJSON v = parseKey v <|> parseAnyOf v  <|> parseAllOf v <|> parseAtLeast v
      where
          parseKey = withText "Script KeyHash" $
              either (fail . showErr) (pure . RequireSignatureOf) . keyHashFromText
          parseAnyOf =
              withObject "Script AnyOf" $ \o -> RequireAnyOf <$> (o .: "any" :: Json.Parser [Script])
          parseAllOf =
              withObject "Script AllOf" $ \o -> RequireAllOf <$> (o .: "all" :: Json.Parser [Script])
          parseAtLeast = withObject "Script MOf" $ \o -> do
              obj <- o .: "at_least"
              content <- obj .: "from"
              m <- obj .: "m"
              RequireMOf m <$> parseJSON content
