{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Script
    (
    -- ** Data-Types
      ScriptError (..)
    , prettyScriptError

    -- ** Applicative Parser
    , scriptArg
    , scriptHashArg
    , stakeCredentialArg
    ) where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..) )
import Cardano.Address.Style.Shelley
    ( Credential (..), liftXPub )
import Cardano.Script
    ( ErrValidateScript
    , Script (..)
    , ScriptHash
    , prettyErrValidateScript
    , scriptHashFromBytes
    , validateScript
    )
import Cardano.Script.Parser
    ( scriptFromString )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Data.Bifunctor
    ( bimap )
import Options.Applicative
    ( Parser, argument, eitherReader, help, long, metavar, option )
import Options.Applicative.Derivation
    ( encodingReader, xpubOpt )

--
-- Data-Types
--

data ScriptError
    = MalformedScript
    | InvalidScript ErrValidateScript
    deriving (Eq, Show)

prettyScriptError :: ScriptError -> String
prettyScriptError = \case
    MalformedScript ->
        "Parsing of the script failed. The script should be composed of nested \
        \lists, and the verification keys should be either encoded as base16, \
        \bech32 or base58."
    InvalidScript e ->
        prettyErrValidateScript e

--
-- Applicative Parsers
--

scriptArg :: Parser Script
scriptArg = argument (eitherReader reader) $ mempty
    <> metavar "SCRIPT"
    <> help
        "Script string."
  where
    reader :: String -> Either String Script
    reader
        = left prettyScriptError
        . maybe (Left malformedScript) (\s -> bimap InvalidScript (const s) $ validateScript s)
        . scriptFromString
      where
        malformedScript = MalformedScript

stakeCredentialArg  :: String -> Parser (Credential 'DelegationK)
stakeCredentialArg str =
    (DelegationFromKey . liftXPub <$> xpubOpt "from-key" str)
    <|>
    (DelegationFromScript <$> scriptHashArg str)

scriptHashArg :: String -> Parser ScriptHash
scriptHashArg helpDoc =
    option (eitherReader reader) $ mempty
        <> long "from-script"
        <> metavar "SCRIPT_HASH"
        <> help helpDoc
  where
    reader :: String -> Either String ScriptHash
    reader str = do
        bytes <- encodingReader str
        case scriptHashFromBytes bytes of
            Just scriptHash -> pure scriptHash
            Nothing -> Left "Failed to convert bytes into a valid script hash."
