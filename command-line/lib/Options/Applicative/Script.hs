{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

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
    -- TODO: Provides a bigger help text explaining how to construct a script
    -- address.
    <> help "Script string."

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
