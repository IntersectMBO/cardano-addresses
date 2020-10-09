{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude

import Command
    ( withUtf8Encoding )

import qualified Command as CLI

main :: IO ()
main = withUtf8Encoding (CLI.setup >> CLI.parse >>= CLI.run)
