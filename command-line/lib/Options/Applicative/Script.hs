{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.Script
    (
    -- ** Data-Types
      ScriptError (..)
    , prettyScriptError

    -- ** Applicative Parser
    , scriptArg
    , scriptReader
    , scriptHashArg
    , scriptHashReader
    ) where

import Prelude

import Cardano.Address.Script
    ( ErrValidateScript
    , Script (..)
    , ScriptHash
    , prettyErrValidateScript
    , scriptHashFromBytes
    , validateScript
    )
import Cardano.Address.Script.Parser
    ( scriptFromString )
import Control.Arrow
    ( left )
import Data.Bifunctor
    ( bimap )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )
import Options.Applicative.Derivation
    ( bech32Reader )

import qualified Cardano.Codec.Bech32.Prefixes as CIP5

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
scriptArg = argument (eitherReader scriptReader) $ mempty
    <> metavar "SCRIPT"
    -- TODO: Provides a bigger help text explaining how to construct a script
    -- address.
    <> help "Script string."

scriptReader :: String -> Either String Script
scriptReader
    = left prettyScriptError
    . maybe (Left malformedScript) (\s -> bimap InvalidScript (const s) $ validateScript s)
    . scriptFromString
  where
    malformedScript = MalformedScript

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
