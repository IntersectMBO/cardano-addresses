module Main where

import Prelude

import qualified Command as CLI

main :: IO ()
main = CLI.setup >> CLI.parse >>= CLI.run
