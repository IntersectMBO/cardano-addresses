module Main where

import Prelude

import Cardano.Address.Compat
    ( ghcjsBuildSupport )
import Command
    ( withUtf8Encoding )
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover

main :: IO ()
main = do
    ghcjsBuildSupport
    withUtf8Encoding $ hspecWith defaultConfig AutoDiscover.spec
