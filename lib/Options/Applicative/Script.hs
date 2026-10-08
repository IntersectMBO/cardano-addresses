{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Options.Applicative.Script
    (
    -- ** Applicative Parser
      scriptArg
    , scriptReader
    , scriptHashArg
    , scriptHashReader
    , levelOpt
    , scriptTemplateReader
    , scriptTemplateSpendingArg
    , scriptTemplateStakingArg
    ) where

import Prelude

import Cardano.Address.KeyHash
    ( KeyHash )
import Cardano.Address.Script
    ( Cosigner (..)
    , Script (..)
    , ScriptHash
    , ValidationLevel (..)
    , prettyErrScriptHashFromText
    , prettyErrValidateScript
    , scriptHashFromText
    )
import Cardano.Address.Script.Parser
    ( requireCosignerOfParser, requireSignatureOfParser, scriptFromString )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Data.Bifunctor
    ( first )
import Options.Applicative
    ( Parser, argument, eitherReader, flag', help, long, metavar, option )

import qualified Data.Text as T

--
-- Applicative Parsers
--

scriptArg :: Parser (Script KeyHash)
scriptArg = argument (eitherReader scriptReader) $ mempty
    <> metavar "SCRIPT"
    <> help "Script string in the simple script syntax (e.g. 'all [vk1..., vk2...]' or 'any [vk..., at_least 1 [...]]' or 'at_least N [...]' with optional 'active_from'/'active_until' timelocks). See script subcommands for examples."

scriptReader :: String -> Either String (Script KeyHash)
scriptReader =
    left prettyErrValidateScript . (scriptFromString requireSignatureOfParser)

scriptHashArg :: String -> Parser ScriptHash
scriptHashArg helpDoc =
    argument (eitherReader scriptHashReader) $ mempty
        <> metavar "SCRIPT HASH"
        <> help helpDoc

scriptHashReader :: String -> Either String ScriptHash
scriptHashReader str =
    first prettyErrScriptHashFromText (scriptHashFromText . T.pack $ str)

levelOpt :: Parser ValidationLevel
levelOpt = required <|> recommended
  where
    required = flag' RequiredValidation (long "required")
    recommended = flag' RecommendedValidation (long "recommended")

scriptTemplateReader :: String -> Either String (Script Cosigner)
scriptTemplateReader =
    left prettyErrValidateScript . (scriptFromString requireCosignerOfParser)

scriptTemplateSpendingArg :: Parser (Script Cosigner)
scriptTemplateSpendingArg = option (eitherReader scriptTemplateReader) $ mempty
    <> long "spending"
    <> metavar "SPENDING SCRIPT TEMPLATE"
    <> help "Spending script template string."

scriptTemplateStakingArg :: Parser (Script Cosigner)
scriptTemplateStakingArg = option (eitherReader scriptTemplateReader) $ mempty
    <> long "staking"
    <> metavar "STAKING SCRIPT TEMPLATE"
    <> help "Staking script template string."
