{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Script
    (
    -- ** Applicative Parser
      scriptArg
    ) where

import Prelude

import Cardano.Script
    ( Script (..) )
import Cardano.ScriptParser
    ( scriptParser )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )
import Text.ParserCombinators.ReadP
    ( readP_to_S )


scriptFromString :: String -> Either String Script
scriptFromString str =
    case readP_to_S scriptParser str of
         [(multisig,_rest)] ->
                 Right multisig
         _ ->
             Left "Parsing of the script failed."

scriptArg :: Parser Script
scriptArg = argument (eitherReader scriptFromString) $ mempty
    <> metavar "SCRIPT"
    <> help
        "Script string."
