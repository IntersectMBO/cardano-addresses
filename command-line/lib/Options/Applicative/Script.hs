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
    , StakeCredential (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Script
    ( Script (..)
    , ScriptError (..)
    , ScriptHash
    , scriptErrorToMsg
    , scriptHashFromBytes
    )
import Cardano.ScriptParser
    ( scriptFromString )
import Codec.Binary.Encoding
    ( AbstractEncoding (..)
    , detectEncoding
    , fromBase16
    , fromBase58
    , fromBech32
    )
import Control.Applicative
    ( (<|>) )
import Options.Applicative
    ( Parser, argument, eitherReader, help, metavar )
import Options.Applicative.Derivation
    ( xpubArg )
import System.IO.Extra
    ( markCharsRedAtIndices )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
    bytes <- case detectEncoding str of
        Just EBase16   -> fromBase16 (toBytes str)
        Just EBech32{} -> fromBech32 markCharsRedAtIndices (toBytes str)
        Just EBase58   -> fromBase58 (toBytes str)
        Nothing        -> Left
            "Couldn't detect input encoding? The key must be encoded as \
            \base16, bech32 or base58."
    case scriptHashFromBytes bytes of
        Just scriptHash -> pure scriptHash
        Nothing   -> Left
            "Failed to convert bytes into a valid script hash."
  where
    toBytes = T.encodeUtf8 . T.pack

scriptHashArg :: String -> Parser ScriptHash
scriptHashArg helpDoc =
    argument (eitherReader scriptHashReader) $ mempty
        <> metavar "SCRIPT_HASH"
        <> help helpDoc

data StakeCredential = FromKey XPub | FromScriptHash ScriptHash
    deriving (Show, Eq)

stakeCredentialArg  :: String -> Parser StakeCredential
stakeCredentialArg str = (FromScriptHash <$> scriptHashArg str) <|> (FromKey <$> xpubArg str)
