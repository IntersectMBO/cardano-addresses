{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Options.Applicative.MnemonicLanguage
    (
    -- * Type
      MnemonicLanguage(..)
    , mnemonicLanguageToString
    , mnemonicLanguageFromString

    -- * Applicative Parser
    , mnemonicLanguageOpt
    ) where

import Prelude

import Data.Char
    ( toLower )
import Data.List
    ( intercalate )
import Data.List.Extra
    ( enumerate )
import GHC.Generics
    ( Generic )
import Options.Applicative
    ( Parser
    , completer
    , eitherReader
    , help
    , listCompleter
    , long
    , metavar
    , option
    , showDefaultWith
    , value
    )

--
-- Type
--

data MnemonicLanguage
    = English
    deriving (Generic, Show, Bounded, Enum, Eq)

mnemonicLanguageToString :: MnemonicLanguage -> String
mnemonicLanguageToString = map toLower . show

mnemonicLanguageFromString :: String -> Either String MnemonicLanguage
mnemonicLanguageFromString = \case
    "english" -> Right English
    _ -> Left $ mempty
           <> "Invalid mnemonic language. At this moment only 'english' available."

languageStrs :: [String]
languageStrs = mnemonicLanguageToString <$> enumerate

--
-- Applicative Parser
--

mnemonicLanguageOpt :: Parser MnemonicLanguage
mnemonicLanguageOpt = option (eitherReader mnemonicLanguageFromString) $ mempty
    <> long "language"
    <> metavar "STR"
    <> help ( "Language of mnemonic words to generate. Must be one of: "
             <> intercalate ", " languageStrs
             <> ".")
    <> value English
    <> showDefaultWith mnemonicLanguageToString
    <> completer (listCompleter languageStrs)
