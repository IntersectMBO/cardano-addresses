{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Script
    (
    -- ** Applicative Parser
      scriptArg
    , stakeCredentialArg
    , Credential (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..), liftXPub )
import Cardano.Script
    ( Script (..)
    , ScriptError (..)
    , ScriptHash
    , scriptErrorToMsg
    , scriptHashFromBytes
    )
import Cardano.ScriptParser
    ( scriptFromString )
import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, argument, eitherReader, help, long, metavar, option )
import Options.Applicative.Derivation
    ( encodingReader, xpubOpt )

scriptArg :: Parser Script
scriptArg = argument (eitherReader scriptFromString') $ mempty
    <> metavar "SCRIPT"
    <> help
        "Script string."

scriptFromString' :: String -> Either String Script
scriptFromString' = maybe (Left errMsg) Right . scriptFromString
  where
    errMsg = scriptErrorToMsg MalformedScript

scriptHashReader :: String -> Either String ScriptHash
scriptHashReader str = do
    bytes <- encodingReader str
    case scriptHashFromBytes bytes of
        Just scriptHash -> pure scriptHash
        Nothing   -> Left
            "Failed to convert bytes into a valid script hash."

scriptHashArg :: String -> Parser ScriptHash
scriptHashArg helpDoc =
    option (eitherReader scriptHashReader) $ mempty
        <> long "from-script"
        <> metavar "SCRIPT_HASH"
        <> help helpDoc

stakeCredentialArg  :: String -> Parser (Credential 'StakingK)
stakeCredentialArg str =
    ((FromKey . liftXPub ) <$> xpubOpt "from-key" str) <|> (FromScript <$> scriptHashArg str)
