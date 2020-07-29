module Main where

import Prelude

import Command
    ( withUtf8Encoding )
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover

main :: IO ()
main = withUtf8Encoding $ hspecWith defaultConfig AutoDiscover.spec
