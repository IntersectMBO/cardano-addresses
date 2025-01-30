{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK hide #-}

-- |
-- Copyright: 2020 Input Output (Hong Kong) Ltd., 2021-2022 Input Output Global Inc. (IOG), 2023-2025 Intersect
-- License: Apache-2.0

module Command.Version
    ( opt
    , run
    ) where

import Prelude

import Data.Version
    ( showVersion )
import Options.Applicative
    ( Parser
    , command
    , flag'
    , help
    , hidden
    , info
    , long
    , progDesc
    , short
    , subparser
    , (<|>)
    )
import Paths_cardano_addresses_cli
    ( version )
import System.Git.TH
    ( gitRevParseHEAD )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

opt :: a -> Parser a
opt a =
    flag' a (mconcat
        [ long "version"
        , short 'v'
        , help helpText
        ])
  <|>
    subparser (mconcat
        [ hidden
        , command "version" $ info (pure a) (progDesc helpText)
        ])
  where
    helpText = "Show the software current version and build revision."

run :: IO ()
run = do
    B8.putStrLn $ T.encodeUtf8 $ T.pack $
        showVersion version <> " @ " <> $(gitRevParseHEAD)
