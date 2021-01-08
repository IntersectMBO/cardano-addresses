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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
    ( XPub, credentialHashSize, hashCredential, xpubFromBytes, xpubToBytes )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode, fromBase16 )
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
import Data.Map.Strict
    ( Map )
import Data.Text
    ( Text )
import Data.Traversable
    ( for )
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
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T


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
newtype Cosigner = Cosigner Word8
    deriving (Generic, Show, Ord, Eq)
instance NFData Cosigner

-- | Represents the script template that show the structure of the script and determines
-- the expected place of verification keys corresponding to given cosigners.
--
-- @since 3.2.0
data ScriptTemplate = ScriptTemplate
    { cosigners :: Map Cosigner XPub
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
        when (invalidTimelocks script) $ Left InvalidTimelocks
        traverse_ validateScript script

    RequireAnyOf script -> do
        when (L.null script) $ Left EmptyList
        when (hasDuplicate script) $ Left DuplicateSignatures
        when (invalidTimelocks script) $ Left InvalidTimelocks
        traverse_ validateScript script

    RequireSomeOf m script -> do
        when (m == 0) $ Left MZero
        when (length script < fromIntegral m) $ Left ListTooSmall
        when (hasDuplicate script) $ Left DuplicateSignatures
        when (invalidTimelocks script) $ Left InvalidTimelocks
        traverse_ validateScript script

    ActiveFromSlot _ -> pure ()

    ActiveUntilSlot _ -> pure ()
  where
    hasDuplicate xs =
        length sigs /= length (L.nub sigs)
      where
        sigs = [ sig | RequireSignatureOf sig <- xs ]
    invalidTimelocks xs =
        let hasTimelocks = \case
                ActiveFromSlot _ -> True
                ActiveUntilSlot _ -> True
                _ -> False
        in case filter hasTimelocks xs of
            [] -> False
            [ActiveFromSlot s1, ActiveUntilSlot s2] -> s2 <= s1
            [ActiveUntilSlot s2, ActiveFromSlot s1] -> s2 <= s1
            [ActiveFromSlot _] -> False
            [ActiveUntilSlot _] -> False
            _ -> True

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
    | InvalidTimelocks
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
    InvalidTimelocks ->
        "The list inside a script must contain at most two timelock conditions \
        \and they cannot be contridactory"
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

instance ToJSON elem => ToJSON (Script elem) where
    toJSON (RequireSignatureOf content) = toJSON content
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

instance ToJSON KeyHash where
    toJSON = String . keyHashToText

instance FromJSON (Script KeyHash) where
    parseJSON v = do
        script <- fromScriptJson parseKey backtrack v

        either (fail . prettyErrValidateScript) pure
            (validateScript script)

        return script
      where
        parseKey = withText "Script KeyHash" $
            either
                (fail . prettyErrKeyHashFromText)
                (pure . RequireSignatureOf)
                . keyHashFromText

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

fromScriptJson
    :: FromJSON (Script elem)
    => (Value -> Parser (Script elem))
    -> (Value -> Parser (Script elem))
    -> Value
    -> Parser (Script elem)
fromScriptJson parseElem backtrack v =
    asum
        [ parseElem v
        , parseAnyOf v
        , parseAllOf v
        , parseAtLeast v
        , parseActiveFrom v
        , parseActiveUntil v
        ] <|> backtrack v

parseAnyOf
    :: FromJSON (Script elem)
    => Value
    -> Parser (Script elem)
parseAnyOf = withObject "Script AnyOf" $ \o ->
    RequireAnyOf <$> o .: "any"

parseAllOf
    :: FromJSON (Script elem)
    => Value
    -> Parser (Script elem)
parseAllOf = withObject "Script AllOf" $ \o ->
    RequireAllOf <$> o .: "all"

parseAtLeast
    :: FromJSON (Script elem)
    => Value
    -> Parser (Script elem)
parseAtLeast = withObject "Script SomeOf" $ \o -> do
    some <- o .: "some"
    RequireSomeOf <$> some .: "at_least" <*> some .: "from"

parseActiveFrom
    :: Value
    -> Parser (Script elem)
parseActiveFrom = withObject "Script ActiveFrom" $ \o ->
    ActiveFromSlot <$> o .: "active_from"

parseActiveUntil
    :: Value
    -> Parser (Script elem)
parseActiveUntil = withObject "Script ActiveUntil" $ \o ->
    ActiveUntilSlot <$> o .: "active_until"

instance ToJSON Cosigner where
    toJSON (Cosigner ix) = object ["cosigner" .= toJSON ix]

instance FromJSON Cosigner where
    parseJSON = withObject "Cosigner" $ \o ->
        Cosigner <$> o .: "cosigner"

instance ToJSON XPub where
    toJSON = String . T.decodeUtf8 . encode EBase16 . xpubToBytes

instance FromJSON XPub where
    parseJSON = withText "XPub" $ \txt ->
        case fromBase16 (T.encodeUtf8 txt) of
            Left err -> fail err
            Right hex -> case xpubFromBytes hex of
                Nothing -> fail "Extended public key cannot be retrieved from a given hex bytestring"
                Just validXPub -> pure validXPub

instance ToJSON ScriptTemplate where
    toJSON (ScriptTemplate cosigners' template') =
        object [ "cosigners" .= object (fmap toPair (Map.toList cosigners'))
               , "template" .= toJSON template']
      where
        toPair (Cosigner ix, xpub) =
            ( T.pack (show ix)
            , toJSON xpub )

instance FromJSON (Script Cosigner) where
    parseJSON v = fromScriptJson parserCosigner backtrack v
      where
        parserCosigner o = do
            cosigner <- parseJSON @Cosigner o
            pure $ RequireSignatureOf cosigner
        backtrack = \case
            Object o -> do
                mAny  <- o .:? "any"  :: Parser (Maybe Value)
                mAll  <- o .:? "all"  :: Parser (Maybe Value)
                mSome <- o .:? "some" :: Parser (Maybe Value)
                mCos <- o .:? "cosigner" :: Parser (Maybe Value)
                case (mAny, mAll, mSome, mCos) of
                    (Just{}, Nothing, Nothing, Nothing)  -> parseAnyOf v
                    (Nothing, Just{}, Nothing, Nothing)  -> parseAllOf v
                    (Nothing, Nothing, Just{}, Nothing)  -> parseAtLeast v
                    (Nothing, Nothing, Nothing, Just{})  -> parserCosigner v
                    (Nothing, Nothing, Nothing, Nothing) -> fail
                        "Found object with no known key 'any', 'all', 'some' or 'cosigner'"
                    (      _,       _,      _,       _)  -> fail
                        "Found multiple keys 'any', 'all', 'cosigner' and/or 'some' at the same level"
            _ ->
                Json.typeMismatch "Object only" v

instance FromJSON ScriptTemplate where
    parseJSON = withObject "ScriptTemplate" $ \o -> do
        template' <- parseJSON <$> o .: "template"
        cosigners' <- parseCosignerPairs <$> o .: "cosigners"
        ScriptTemplate <$> (Map.fromList <$> cosigners') <*> template'
      where
        parseCosignerPairs = withObject "Cosigner pairs" $ \o ->
            case HM.toList o of
                [] -> fail "Cosigners object array should not be empty"
                cs -> for (reverse cs) $ \(numTxt, str) -> do
                    case T.decimal numTxt of
                        Right (num,"") -> do
                            when (num < minBound @Word8 || num > maxBound @Word8) $
                                fail "Cosigner object field should be between '0' and '255'"
                            xpub <- parseJSON str
                            pure (Cosigner num, xpub)
                        _ -> fail "Cosigner object field should be number and value string"
