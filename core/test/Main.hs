module Main where

import Prelude

import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover

main :: IO ()
main = do
    hspecWith defaultConfig AutoDiscover.spec
