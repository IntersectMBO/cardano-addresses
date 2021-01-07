{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address.Script
    (
    -- * Script
      Script (..)
    , serializeScript
    , foldScript

    -- * Script template
    , ScriptTemplate (..)
    , Cosigner (..)

    -- * Validation
    , validateScript
    , ErrValidateScript (..)
    , prettyErrValidateScript

    -- * Hashing
    , ScriptHash (..)
    , toScriptHash
    , scriptHashFromBytes

    , KeyHash (..)
    , keyHashFromBytes
    , keyHashFromText
    , keyHashToText
    , ErrKeyHashFromText
    , prettyErrKeyHashFromText
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, credentialHashSize, hashCredential )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
import Control.Applicative
    ( (<|>) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( foldM, when )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Aeson.Types
    ( Parser )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Foldable
    ( asum, foldl', traverse_ )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Cardano.Codec.Cbor as CBOR
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text.Encoding as T


-- | A 'Script' type represents multi signature script. The script embodies conditions
-- that need to be satisfied to make it valid.
--
-- @since 3.0.0
data Script (elem :: *)
    = RequireSignatureOf !elem
    | RequireAllOf ![Script elem]
    | RequireAnyOf ![Script elem]
    | RequireSomeOf Word8 ![Script elem]
    | ActiveFromSlot Natural
    | ActiveUntilSlot Natural
    deriving stock (Generic, Show, Eq)
instance NFData elem => NFData (Script elem)

-- | This function realizes what cardano-node's `Api.serialiseToCBOR script` realizes
-- This is basically doing the symbolically following:
-- toCBOR [0,multisigScript]
--
-- @since 3.0.0
serializeScript :: Script KeyHash -> ByteString
serializeScript script =
    multisigTag <> CBOR.toStrictByteString (toCBOR script)
  where
    -- | Magic number representing the tag of the native multi-signature script
    -- language. For each script language included, a new tag is chosen.
    multisigTag :: ByteString
    multisigTag = "\00"

    toCBOR :: Script KeyHash -> CBOR.Encoding
    toCBOR = \case
        RequireSignatureOf (KeyHash verKeyHash) ->
            encodeMultiscriptCtr 0 2 <> CBOR.encodeBytes verKeyHash
        RequireAllOf contents ->
            encodeMultiscriptCtr 1 2 <> encodeFoldable toCBOR contents
        RequireAnyOf contents ->
            encodeMultiscriptCtr 2 2 <> encodeFoldable toCBOR contents
        RequireSomeOf m contents -> mconcat
            [ encodeMultiscriptCtr 3 3
            , CBOR.encodeInt (fromInteger $ toInteger m)
            , encodeFoldable toCBOR contents
            ]
        ActiveFromSlot slotNum ->
            encodeMultiscriptCtr 4 2 <> CBOR.encodeWord64 (fromInteger $ toInteger slotNum)
        ActiveUntilSlot slotNum ->
            encodeMultiscriptCtr 5 2 <> CBOR.encodeWord64 (fromInteger $ toInteger slotNum)

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

-- | Represents the cosigner of the script, ie., party that co-shares the script.
--
-- @since 3.2.0
data Cosigner = Cosigner Word8
    deriving (Generic, Show, Ord, Eq)
instance NFData Cosigner

-- | Represents the script template that show the structure of the script and determines
-- the expected place of verification keys corresponding to given cosigners.
--
-- @since 3.2.0
data ScriptTemplate = ScriptTemplate
    { cosigners :: [(Cosigner, XPub)]
    , template :: Script Cosigner
    } deriving (Generic, Show, Eq)
instance NFData ScriptTemplate

-- | Computes the hash of a given script, by first serializing it to CBOR.
--
-- @since 3.0.0
toScriptHash :: Script KeyHash -> ScriptHash
toScriptHash = ScriptHash . hashCredential . serializeScript

-- | A 'ScriptHash' type represents script hash. The hash is expected to have size of
-- 28-byte.
--
-- @since 3.0.0
newtype ScriptHash = ScriptHash { unScriptHash :: ByteString }
    deriving (Generic, Show, Ord, Eq)
instance NFData ScriptHash

-- | Construct an 'ScriptHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
scriptHashFromBytes :: ByteString -> Maybe ScriptHash
scriptHashFromBytes bytes
    | BS.length bytes /= credentialHashSize = Nothing
    | otherwise = Just $ ScriptHash bytes

-- | A 'KeyHash' type represents verification key hash that participate in building
-- multi-signature script. The hash is expected to have size of 28-byte.
--
-- @since 3.0.0
newtype KeyHash = KeyHash { unKeyHash :: ByteString }
    deriving (Generic, Show, Ord, Eq)
instance NFData KeyHash

-- | Construct an 'KeyHash' from raw 'ByteString' (28 bytes).
--
-- @since 3.0.0
keyHashFromBytes :: ByteString -> Maybe KeyHash
keyHashFromBytes bytes
    | BS.length bytes /= credentialHashSize = Nothing
    | otherwise = Just $ KeyHash bytes

-- | Encode a 'KeyHash' to bech32 'Text', using @script_vkh@ as a human readable prefix.
--
-- @since 3.0.0
keyHashToText :: KeyHash -> Text
keyHashToText (KeyHash keyHash) =
    T.decodeUtf8 $ encode (EBech32 CIP5.script_vkh) keyHash

-- | Construct a 'KeyHash' from 'Text'. Either hex encoded text or
-- Bech32 encoded text with `script_vkh`, `script_vk` or `script_xvk` hrp are
-- expected. Raw keys will be hashed on the fly, whereas hash that are directly
-- provided will remain as such.
--
-- @since 3.1.0
keyHashFromText :: Text -> Either ErrKeyHashFromText KeyHash
keyHashFromText txt = do
    (hrp, dp) <- either
        (const $ Left ErrKeyHashFromTextInvalidString)
        Right
        (Bech32.decodeLenient txt)

    maybeToRight ErrKeyHashFromTextWrongDataPart (Bech32.dataPartToBytes dp)
        >>= convertBytes hrp
        >>= maybeToRight ErrKeyHashFromTextWrongPayload . keyHashFromBytes
 where
    convertBytes hrp bytes
        | hrp == CIP5.script_vkh = Right bytes
        | hrp == CIP5.script_vk  = Right $ hashCredential bytes
        | hrp == CIP5.script_xvk = Right $ hashCredential $ BS.take 32 bytes
        | otherwise = Left ErrKeyHashFromTextWrongHrp

-- Possible errors when deserializing a key hash from text.
--
-- @since 3.0.0
data ErrKeyHashFromText
    = ErrKeyHashFromTextInvalidString
    | ErrKeyHashFromTextWrongPayload
    | ErrKeyHashFromTextWrongHrp
    | ErrKeyHashFromTextWrongDataPart
    deriving (Show, Eq)

-- Possible errors when deserializing a key hash from text.
--
-- @since 3.0.0
prettyErrKeyHashFromText :: ErrKeyHashFromText -> String
prettyErrKeyHashFromText = \case
    ErrKeyHashFromTextInvalidString ->
        "Invalid encoded string: must be bech32-encoded."
    ErrKeyHashFromTextWrongPayload ->
        "Verification key hash must contain exactly 28 bytes."
    ErrKeyHashFromTextWrongHrp ->
        "Invalid human-readable prefix: must be 'script_vkh'."
    ErrKeyHashFromTextWrongDataPart ->
        "Verification key hash is Bech32-encoded but has an invalid data part."

--
-- Script folding
--

-- | 'Script' folding
--
-- @since 3.2.0
foldScript :: (KeyHash -> b -> b) -> b -> Script KeyHash -> b
foldScript fn zero = \case
    RequireSignatureOf k -> fn k zero
    RequireAllOf xs      -> foldMScripts xs
    RequireAnyOf xs      -> foldMScripts xs
    RequireSomeOf _ xs   -> foldMScripts xs
    ActiveFromSlot _     -> zero
    ActiveUntilSlot _    -> zero
  where
    foldMScripts =
        runIdentity . foldM (\acc -> Identity . foldScript fn acc) zero


--
-- Script validation
--

-- | Validate a 'Script', semantically
--
-- @since 3.0.0
validateScript :: Script KeyHash -> Either ErrValidateScript ()
validateScript = \case
    RequireSignatureOf (KeyHash bytes) -> do
        when (BS.length bytes /= credentialHashSize) $ Left WrongKeyHash

    RequireAllOf script -> do
        when (L.null script) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script

    RequireAnyOf script -> do
        when (L.null script) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script

    RequireSomeOf m script -> do
        when (m == 0) $ Left MZero
        when (length script < fromIntegral m) $ Left ListTooSmall
        when (hasDuplicate script) $ Left DuplicateSignatures
        traverse_ validateScript script

    ActiveFromSlot _ -> pure ()

    ActiveUntilSlot _ -> pure ()
  where
    hasDuplicate xs = do
        length sigs /= length (L.nub sigs)
      where
        sigs = [ sig | RequireSignatureOf sig <- xs ]

-- | Possible validation errors when validating a script
--
-- @since 3.0.0
data ErrValidateScript
    = EmptyList
    | ListTooSmall
    | MZero
    | DuplicateSignatures
    | WrongKeyHash
    | Malformed
    deriving (Eq, Show)

-- | Pretty-print a validation error.
--
-- @since 3.0.0
prettyErrValidateScript
    :: ErrValidateScript
    -> String
prettyErrValidateScript = \case
    EmptyList ->
        "The list inside a script is empty."
    MZero ->
        "At least must be at least 1."
    ListTooSmall ->
        "At least must not be larger than the list of keys."
    DuplicateSignatures ->
        "The list inside a script has duplicate keys."
    WrongKeyHash ->
        "The hash of verification key is expected to have "<>show credentialHashSize<>" bytes."
    Malformed ->
        "Parsing of the script failed. The script should be composed of nested \
        \lists, and the verification keys should be either encoded as bech32."

--
-- Internal
--

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
--          , {"some": { "from" :[ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735b"
--                               , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735c"
--                               , "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735d"
--                               ]
--                     , "at_least" : 2
--                     }
--            }
--          ]
--}
--{ "all" : [ "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
--          , {"active_from": 120 }
--          ]
--}

instance ToJSON (Script KeyHash) where
    toJSON (RequireSignatureOf keyHash) =
        String $ keyHashToText keyHash
    toJSON (RequireAllOf content) =
        object ["all" .= fmap toJSON content]
    toJSON (RequireAnyOf content) =
        object ["any" .= fmap toJSON content]
    toJSON (RequireSomeOf count scripts) =
        object ["some" .= object ["at_least" .= count, "from" .= scripts]]
    toJSON (ActiveFromSlot slot) =
        object ["active_from" .= slot]
    toJSON (ActiveUntilSlot slot) =
        object ["active_until" .= slot]

instance FromJSON (Script KeyHash) where
    parseJSON v = do
        script <- asum
            [ parseKey v
            , parseAnyOf v
            , parseAllOf v
            , parseAtLeast v
            , parseActiveFrom v
            , parseActiveUntil v
            ] <|> backtrack v

        either (fail . prettyErrValidateScript) pure
            (validateScript script)

        return script
      where
        parseKey = withText "Script KeyHash" $
            either
                (fail . prettyErrKeyHashFromText)
                (pure . RequireSignatureOf)
                . keyHashFromText

        parseAnyOf = withObject "Script AnyOf" $ \o ->
            RequireAnyOf <$> o .: "any"

        parseAllOf = withObject "Script AllOf" $ \o ->
            RequireAllOf <$> o .: "all"

        parseAtLeast = withObject "Script SomeOf" $ \o -> do
            some <- o .: "some"
            RequireSomeOf <$> some .: "at_least" <*> some .: "from"

        parseActiveFrom = withObject "Script ActiveFrom" $ \o ->
            ActiveFromSlot <$> o .: "active_from"

        parseActiveUntil = withObject "Script ActiveUntil" $ \o ->
            ActiveUntilSlot <$> o .: "active_until"

        -- NOTE: Because we use an alternative sum to define all parsers, in
        -- case all parser fails, only the last error is returned which can be
        -- very misleading. For example, sending {"any": []} yields an error
        -- telling us that the key `"some"` is missing.
        --
        -- To cope with this, we add a last parser 'backtrack' which always
        -- fail but with a more helpful error which tries its best at
        -- identifying the right constructor.
        backtrack = \case
            Object o -> do
                mAny  <- o .:? "any"  :: Parser (Maybe Value)
                mAll  <- o .:? "all"  :: Parser (Maybe Value)
                mSome <- o .:? "some" :: Parser (Maybe Value)
                case (mAny, mAll, mSome) of
                    (Just{}, Nothing, Nothing)  -> parseAnyOf v
                    (Nothing, Just{}, Nothing)  -> parseAllOf v
                    (Nothing, Nothing, Just{})  -> parseAtLeast v
                    (Nothing, Nothing, Nothing) -> fail
                        "Found object with no known key 'any', 'all' or 'some'"
                    (      _,       _,      _)  -> fail
                        "Found multiple keys 'any', 'all' and/or 'some' at the same level"

            String{} ->
                parseKey v

            _ ->
                Json.typeMismatch "Object or String" v
