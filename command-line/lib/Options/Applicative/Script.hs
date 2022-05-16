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
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash
    , Script (..)
    , ScriptHash
    , ValidationLevel (..)
    , prettyErrValidateScript
    , scriptHashFromBytes
    )
import Cardano.Address.Script.Parser
    ( requireSignatureOfParser, scriptFromString )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Options.Applicative
    ( Parser, argument, eitherReader, flag', help, long, metavar )
import Options.Applicative.Derivation
    ( bech32Reader )


import qualified Cardano.Codec.Bech32.Prefixes as CIP5

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
scriptHashReader str = do
    (_hrp, bytes) <- bech32Reader allowedPrefixes str
    case scriptHashFromBytes bytes of
        Just scriptHash -> pure scriptHash
        Nothing -> Left "Failed to convert bytes into a valid script hash."
  where
    allowedPrefixes =
        [ CIP5.script
        ]

levelOpt :: Parser ValidationLevel
levelOpt = required <|> recommended
  where
    required = flag' RequiredValidation (long "required")
    recommended = flag' RecommendedValidation (long "recommended")
