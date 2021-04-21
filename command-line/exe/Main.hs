{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude

import Cardano.Address.Jsbits
    ( addJsbitsDependency )
import Command
    ( withUtf8Encoding )

import qualified Command as CLI

main :: IO ()
main = do
    addJsbitsDependency
    withUtf8Encoding (CLI.setup >> CLI.parse >>= CLI.run)
