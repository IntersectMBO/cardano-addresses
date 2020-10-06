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
    ( Script (..), ScriptError (..) )
import Cardano.ScriptParser
    ( scriptFromString )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )

scriptArg :: Parser Script
scriptArg = argument (eitherReader scriptFromString') $ mempty
    <> metavar "SCRIPT"
    <> help
        "Script string."

scriptFromString' :: String -> Either String Script
scriptFromString' = maybe (Left errMsg) Right . scriptFromString
  where
    errMsg = show MalformedScript
