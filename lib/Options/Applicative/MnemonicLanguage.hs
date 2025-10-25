{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2025 Intersect
-- License: Apache-2.0

module Options.Applicative.MnemonicLanguage
    (
    -- * Applicative Parser
      mnemonicLanguageOpt

    -- * Helpers
    , supportedDictionaryToString
    , supportedDictionaryFromString

    ) where

import Prelude

import Cardano.Dictionary
    ( SupportedDictionary (..) )
import Data.List
    ( intercalate )
import Data.List.Extra
    ( enumerate )
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


-- | SupportedDictionary displays according to two-letter [ISO 639-1 Code](https://www.loc.gov/standards/iso639-2/php/code_list.php)
supportedDictionaryToString :: SupportedDictionary -> String
supportedDictionaryToString English = "en"
supportedDictionaryToString Italian = "it"

supportedDictionaryFromString :: String -> Either String SupportedDictionary
supportedDictionaryFromString = \case
    "en" -> Right English
    "it" -> Right Italian
    _ -> Left $ mempty
           <> "Invalid supported dictionary. At this moment only 'en' and 'it' available."

languageStrs :: [String]
languageStrs = supportedDictionaryToString <$> enumerate

--
-- Applicative Parser
--

mnemonicLanguageOpt :: Parser SupportedDictionary
mnemonicLanguageOpt = option (eitherReader supportedDictionaryFromString) $ mempty
    <> long "language"
    <> metavar "STR"
    <> help ( "Language of mnemonic words to generate. Must be one of: "
             <> intercalate ", " languageStrs
             <> ".")
    <> value English
    <> showDefaultWith supportedDictionaryToString
    <> completer (listCompleter languageStrs)
