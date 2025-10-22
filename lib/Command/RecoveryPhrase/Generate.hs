{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.RecoveryPhrase.Generate
    ( Cmd (..)
    , mod
    , run
    ) where

import Prelude hiding
    ( mod )

import Cardano.Mnemonic
    ( english, entropyToMnemonic, genEntropy, mnemonicToTextWithDict )
import Options.Applicative
    ( CommandFields, Mod, command, helper, info, progDesc )
import Options.Applicative.MnemonicLanguage
    ( MnemonicLanguage (..), mnemonicLanguageOpt )
import Options.Applicative.MnemonicSize
    ( MnemonicSize (..), mnemonicSizeOpt )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data Cmd = Generate
    { size :: MnemonicSize
    , language :: MnemonicLanguage
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "generate" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Generate a recovery phrase for a specified mnemonic size and language."
  where
    parser = Generate
        <$> mnemonicSizeOpt
        <*> mnemonicLanguageOpt

run :: Cmd -> IO ()
run Generate{size,language} = do
    m <- case size of
        MS_9  -> mnemonicToTextWithDict @9  . entropyToMnemonic <$> genEntropy
        MS_12 -> mnemonicToTextWithDict @12 . entropyToMnemonic <$> genEntropy
        MS_15 -> mnemonicToTextWithDict @15 . entropyToMnemonic <$> genEntropy
        MS_18 -> mnemonicToTextWithDict @18 . entropyToMnemonic <$> genEntropy
        MS_21 -> mnemonicToTextWithDict @21 . entropyToMnemonic <$> genEntropy
        MS_24 -> mnemonicToTextWithDict @24 . entropyToMnemonic <$> genEntropy
    B8.putStrLn $ T.encodeUtf8 $ T.unwords $ m $ dictionaryFromLanguage language
  where
    dictionaryFromLanguage = \case
        English -> english
