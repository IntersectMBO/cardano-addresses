{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Cardano.Script.Parser
    (
    -- ** Script Parser
      scriptFromString
    , scriptParser

    -- * Internal
    , requireSignatureOfParser
    , requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser

    ) where

import Prelude

import Cardano.Script
    ( KeyHash (..), Script (..) )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), detectEncoding, fromBase16, fromBase58 )
import Data.Char
    ( isDigit, isLetter )
import Data.Word
    ( Word8 )
import Text.ParserCombinators.ReadP
    ( ReadP, readP_to_S, (<++) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.ParserCombinators.ReadP as P

-- | Run 'scriptParser' on string input.
scriptFromString :: String -> Maybe Script
scriptFromString str =
    case readP_to_S scriptParser str of
         [(multisig, "")] -> Just multisig
         _ -> Nothing

-- | The script embodies combination of signing keys that need to be met to make
-- it valid. We assume here that the script could
-- delivered from standard input. The examples below are self-explanatory:
--
-- 1. requiring signature
-- 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe
--
-- 2. 'any' for signature required
-- any [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]
--
-- 3. 'all' signatures required
-- all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]
--
-- 4. 'at_least' 1 signature required
-- at_least 1 [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]
--
-- 5. Nested script are supported
-- at_least 1 [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]]
--
-- Parser is insensitive to whitespaces.
--
-- @since 3.0.0
scriptParser :: ReadP Script
scriptParser =
    requireAllOfParser <++
    requireAnyOfParser <++
    requireAtLeastOfParser <++
    requireSignatureOfParser

requireSignatureOfParser :: ReadP Script
requireSignatureOfParser = do
    P.skipSpaces
    verKeyStr <- P.munch1 (\c -> isDigit c || isLetter c || c == '_')
    case detectEncoding verKeyStr of
        Just EBase16 -> case fromBase16 (toBytes verKeyStr) of
            Left _ -> fail "Invalid Base16-encoded string."
            Right keyHash -> return $ toSignature keyHash
        Just EBech32{} -> case fromBech32 (T.pack verKeyStr) of
            Nothing -> fail "Invalid Bech32-encoded string.."
            Just keyHash -> return $ toSignature keyHash
        Just EBase58 -> case fromBase58 (toBytes verKeyStr) of
            Left err -> fail err
            Right keyHash -> return $ toSignature keyHash
        Nothing ->
            fail "Verification key hash must be must be encoded as \
                   \base16, bech32 or base58."
 where
    toBytes = T.encodeUtf8 . T.pack
    fromBech32 txt = do
        (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient txt)
        Bech32.dataPartToBytes dp
    toSignature = RequireSignatureOf . KeyHash

requireAllOfParser :: ReadP Script
requireAllOfParser = do
    P.skipSpaces
    _identifier <- P.string "all"
    RequireAllOf <$> commonPart

requireAnyOfParser :: ReadP Script
requireAnyOfParser = do
    P.skipSpaces
    _identifier <- P.string "any"
    RequireAnyOf <$> commonPart

requireAtLeastOfParser :: ReadP Script
requireAtLeastOfParser = do
    P.skipSpaces
    _identifier <- P.string "at_least"
    RequireMOf <$> naturalParser <*> commonPart

naturalParser :: ReadP Word8
naturalParser = do
    P.skipSpaces
    fromInteger . read <$> P.munch1 isDigit

commonPart :: ReadP [Script]
commonPart = do
    P.skipSpaces
    _open <- P.string "["
    P.skipSpaces
    content <- P.sepBy scriptParser (P.string ",")
    P.skipSpaces
    _close <- P.string "]"
    P.skipSpaces
    return content
