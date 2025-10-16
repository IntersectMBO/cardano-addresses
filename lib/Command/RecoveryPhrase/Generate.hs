{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ( english, entropyToMnemonic, genEntropy, mnemonicToText )
import Options.Applicative
    ( CommandFields, Mod, command, helper, info, progDesc )
import Options.Applicative.MnemonicSize
    ( MnemonicSize (..), mnemonicSizeOpt )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


newtype Cmd = Generate
    { size :: MnemonicSize
    } deriving (Show)

mod :: (Cmd -> parent) -> Mod CommandFields parent
mod liftCmd = command "generate" $
    info (helper <*> fmap liftCmd parser) $ mempty
        <> progDesc "Generate an English recovery phrase"
  where
    parser = Generate
        <$> mnemonicSizeOpt

run :: Cmd -> IO ()
run Generate{size} = do
    m <- case size of
        MS_9  -> mnemonicToText @9  . entropyToMnemonic <$> genEntropy
        MS_12 -> mnemonicToText @12 . entropyToMnemonic <$> genEntropy
        MS_15 -> mnemonicToText @15 . entropyToMnemonic <$> genEntropy
        MS_18 -> mnemonicToText @18 . entropyToMnemonic <$> genEntropy
        MS_21 -> mnemonicToText @21 . entropyToMnemonic <$> genEntropy
        MS_24 -> mnemonicToText @24 . entropyToMnemonic <$> genEntropy
    B8.putStrLn $ T.encodeUtf8 $ T.unwords $ m english
