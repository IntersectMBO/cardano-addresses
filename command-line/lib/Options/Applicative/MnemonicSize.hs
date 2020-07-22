{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK hide #-}

module Options.Applicative.MnemonicSize
    (
    -- * Type
      MnemonicSize(..)
    , mnemonicSizeToString
    , mnemonicSizeFromString

    -- * Applicative Parser
    , mnemonicSizeOpt
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

data MnemonicSize
    = MS_9 | MS_12 | MS_15 | MS_18 | MS_21 | MS_24
    deriving (Generic, Show, Bounded, Enum, Eq)

mnemonicSizeToString :: MnemonicSize -> String
mnemonicSizeToString = drop 3 . show

mnemonicSizeFromString :: String -> Either String MnemonicSize
mnemonicSizeFromString str =
    case lookup str sizeMap of
        Just ms -> Right ms
        Nothing -> Left $ mempty
            <> "Invalid mnemonic size. Expected one of: "
            <> intercalate ", " sizeStrs
            <> "."
  where
    sizeMap  = sizeStrs `zip` enumerate

sizeStrs :: [String]
sizeStrs = mnemonicSizeToString <$> enumerate

--
-- Applicative Parser
--

mnemonicSizeOpt :: Parser MnemonicSize
mnemonicSizeOpt = option (eitherReader mnemonicSizeFromString) $ mempty
    <> long "size"
    <> metavar "INT"
    <> help "Number of mnemonic words to generate. Must be a multiple of 3."
    <> value MS_15
    <> showDefaultWith mnemonicSizeToString
    <> completer (listCompleter sizeStrs)
