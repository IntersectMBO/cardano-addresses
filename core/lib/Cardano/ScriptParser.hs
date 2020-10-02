{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Cardano.ScriptParser
    (
    -- ** Script Parser
      scriptParser

    -- * Internal
    , requireSignatureOfParser
    , requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser

    ) where

import Prelude

import Cardano.Script
    ( Script (..), keyHashFromBytes )
import Codec.Binary.Encoding
    ( fromBase16 )
import Data.Char
    ( isDigit, isLetter )
import Data.Word
    ( Word8 )
import Text.ParserCombinators.ReadP
    ( ReadP, (<++) )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.ParserCombinators.ReadP as P


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
    verKeyH <- P.munch1 (\c -> isDigit c || isLetter c)
    case length verKeyH of
        56 -> do
            let (Right bytes) = fromBase16 $ T.encodeUtf8 $ T.pack verKeyH
            let (Just keyHash) = keyHashFromBytes bytes
            return $ RequireSignatureOf keyHash
        len ->
            fail $ "Verification key hash should be 28 bytes, but received "
            <> show len <> " bytes."

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
    content <- P.sepBy1 scriptParser (P.string ",")
    P.skipSpaces
    _close <- P.string "]"
    P.skipSpaces
    return content
