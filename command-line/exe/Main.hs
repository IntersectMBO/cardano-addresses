{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude

import Cardano.Address.Compat
    ( ghcjsBuildSupport )
import Command
    ( withUtf8Encoding )

import qualified Command as CLI

main :: IO ()
main = do
    ghcjsBuildSupport
    withUtf8Encoding (CLI.setup >> CLI.parse >>= CLI.run)
