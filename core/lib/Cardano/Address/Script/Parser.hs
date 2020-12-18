{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK prune #-}

module Cardano.Address.Script.Parser
    (
    -- ** Script Parser
      scriptFromString
    , scriptParser

    -- ** Script Validator
    , validateScript
    , ErrValidateScript (..)
    , prettyErrValidateScript

    -- Internal
    , requireSignatureOfParser
    , requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser
    ) where

import Prelude

import Cardano.Address.Script
    ( ErrValidateScript (..)
    , Script (..)
    , keyHashFromText
    , prettyErrKeyHashFromText
    , prettyErrValidateScript
    , validateScript
    )
import Data.Char
    ( isDigit, isLetter )
import Data.Functor
    ( ($>) )
import Data.Word
    ( Word8 )
import Numeric.Natural
    ( Natural )
import Text.ParserCombinators.ReadP
    ( ReadP, readP_to_S, (<++) )

import qualified Data.Text as T
import qualified Text.ParserCombinators.ReadP as P

-- | Run 'scriptParser' on string input.
--
-- @since 3.0.0
scriptFromString :: String -> Either ErrValidateScript Script
scriptFromString str =
    case readP_to_S scriptParser str of
         [(script, "")] -> validateScript script $> script
         _ -> Left Malformed

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
-- 6. 1 signature required after slot number 120
-- all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, active_from 120]
-- 7. 1 signature required until slot number 150
-- all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, active_until 150]
-- 8. 1 signature required in slot interval <145, 150)
-- all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, active_from 145, active_until 150]
--
-- Parser is insensitive to whitespaces.
--
-- @since 3.0.0
scriptParser :: ReadP Script
scriptParser =
    requireAllOfParser <++
    requireAnyOfParser <++
    requireAtLeastOfParser <++
    requireSignatureOfParser <++
    activeFromSlotParser <++
    activeUntilSlotParser

requireSignatureOfParser :: ReadP Script
requireSignatureOfParser = do
    P.skipSpaces
    verKeyStr <- P.munch1 (\c -> isDigit c || isLetter c || c == '_')
    case keyHashFromText (T.pack verKeyStr) of
        Left e  -> fail (prettyErrKeyHashFromText e)
        Right h -> pure (RequireSignatureOf h)

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
    RequireSomeOf <$> naturalParser <*> commonPart

activeFromSlotParser :: ReadP Script
activeFromSlotParser = do
    P.skipSpaces
    _identifier <- P.string "active_from"
    ActiveFromSlot <$> slotParser

activeUntilSlotParser :: ReadP Script
activeUntilSlotParser = do
    P.skipSpaces
    _identifier <- P.string "active_until"
    ActiveUntilSlot <$> slotParser

naturalParser :: ReadP Word8
naturalParser = do
    P.skipSpaces
    fromInteger . read <$> P.munch1 isDigit

slotParser :: ReadP Natural
slotParser = do
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
