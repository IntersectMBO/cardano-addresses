{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude

import Cardano.Address.Compat
    ( ghcjsBuildSupport )
import Main.Utf8
    ( withUtf8 )

import qualified Command as CLI

main :: IO ()
main = do
    ghcjsBuildSupport
    withUtf8 (CLI.setup >> CLI.parse >>= CLI.run)
