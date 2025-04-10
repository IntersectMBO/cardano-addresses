{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Cardano.Address.Script.Parser
    (
    -- ** Script Parser
      scriptFromString
    , scriptToText
    , scriptParser

    -- Internal
    , requireSignatureOfParser
    , requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser
    , requireCosignerOfParser
    ) where

import Prelude

import Cardano.Address.KeyHash
    ( KeyHash, keyHashFromText, prettyErrKeyHashFromText )
import Cardano.Address.Script
    ( Cosigner (..), ErrValidateScript (..), Script (..) )
import Data.Char
    ( isDigit, isLetter )
import Data.Text
    ( Text )
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
scriptFromString
    :: ReadP (Script a)
    -> String
    -> Either ErrValidateScript (Script a)
scriptFromString parser str =
    case readP_to_S (scriptParser parser) str of
         [(script, "")] -> pure script
         _ -> Left Malformed

-- | Defines canonical string output for script that is
-- consistent with 'scriptFromString'.
--
-- @since 3.10.0
scriptToText
    :: Show a
    => Script a
    -> Text
scriptToText (RequireSignatureOf object) = T.pack $ show object
scriptToText (RequireAllOf contents) =
    "all [" <>  T.intercalate "," (map scriptToText contents) <> "]"
scriptToText (RequireAnyOf contents) =
    "any [" <>  T.intercalate "," (map scriptToText contents) <> "]"
scriptToText (RequireSomeOf m contents) =
    "at_least "<> T.pack (show m) <>
    " [" <>  T.intercalate "," (map scriptToText contents) <> "]"
scriptToText (ActiveFromSlot s) =
    "active_from " <> T.pack (show s)
scriptToText (ActiveUntilSlot s) =
    "active_until " <> T.pack (show s)


-- | The script embodies combination of signing keys that need to be met to make
-- it valid. We assume here that the script could
-- delivered from standard input. The examples below are self-explanatory:
--
-- ==== __Example__:
--
-- 1. requiring signature
--    @
--    3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe
--    @
--
-- 2. 'any' for signature required
--    @
--    any [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]
--    @
--
-- 3. 'all' signatures required
-- @all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]@
--
-- 4. 'at_least' 1 signature required
-- @at_least 1 [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]@
--
-- 5. Nested script are supported
-- @at_least 1 [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1, 3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3f1]]@
-- 6. 1 signature required after slot number 120
-- @all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, active_from 120]@
-- 7. 1 signature required until slot number 150
-- @all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, active_until 150]@
-- 8. 1 signature required in slot interval <145, 150)
-- @all [3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe, active_from 145, active_until 150]@
--
-- Parser is insensitive to whitespaces.
--
-- @since 3.0.0
scriptParser :: ReadP (Script a) -> ReadP (Script a)
scriptParser parser =
    requireAllOfParser parser <++
    requireAnyOfParser parser <++
    requireAtLeastOfParser parser <++
    parser <++
    activeFromSlotParser <++
    activeUntilSlotParser

requireSignatureOfParser :: ReadP (Script KeyHash)
requireSignatureOfParser = do
    P.skipSpaces
    verKeyStr <- P.munch1 (\c -> isDigit c || isLetter c || c == '_')
    case keyHashFromText (T.pack verKeyStr) of
        Left e  -> fail (prettyErrKeyHashFromText e)
        Right h -> pure (RequireSignatureOf h)

requireCosignerOfParser :: ReadP (Script Cosigner)
requireCosignerOfParser = do
    P.skipSpaces
    _identifier <- P.string "cosigner#"
    cosignerid <- fromInteger . read <$> P.munch1 isDigit
    pure $ RequireSignatureOf $ Cosigner cosignerid

requireAllOfParser :: ReadP (Script a) -> ReadP (Script a)
requireAllOfParser parser = do
    P.skipSpaces
    _identifier <- P.string "all"
    RequireAllOf <$> commonPart parser

requireAnyOfParser :: ReadP (Script a) -> ReadP (Script a)
requireAnyOfParser parser = do
    P.skipSpaces
    _identifier <- P.string "any"
    RequireAnyOf <$> commonPart parser

requireAtLeastOfParser :: ReadP (Script a) -> ReadP (Script a)
requireAtLeastOfParser parser = do
    P.skipSpaces
    _identifier <- P.string "at_least"
    RequireSomeOf <$> naturalParser <*> commonPart parser

activeFromSlotParser :: ReadP (Script a)
activeFromSlotParser = do
    P.skipSpaces
    _identifier <- P.string "active_from"
    ActiveFromSlot <$> slotParser

activeUntilSlotParser :: ReadP (Script a)
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

commonPart :: ReadP (Script a) -> ReadP [Script a]
commonPart parser = do
    P.skipSpaces
    _open <- P.string "["
    P.skipSpaces
    content <- P.sepBy (scriptParser parser) (P.string ",")
    P.skipSpaces
    _close <- P.string "]"
    P.skipSpaces
    return content
