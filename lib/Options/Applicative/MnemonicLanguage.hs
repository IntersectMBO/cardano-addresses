{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2025 Intersect
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

-- | MnemonicLanguage displays according to two-letter [ISO 639-1 Code](https://www.loc.gov/standards/iso639-2/php/code_list.php)
mnemonicLanguageToString :: MnemonicLanguage -> String
mnemonicLanguageToString English = "en"

mnemonicLanguageFromString :: String -> Either String MnemonicLanguage
mnemonicLanguageFromString = \case
    "en" -> Right English
    _ -> Left $ mempty
           <> "Invalid mnemonic language. At this moment only 'en' available."

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
