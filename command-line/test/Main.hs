module Main where

import Prelude

import Cardano.Address.Compat
    ( ghcjsBuildSupport )
import Main.Utf8
    ( withUtf8 )
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover

main :: IO ()
main = do
    ghcjsBuildSupport
    withUtf8 $ hspecWith defaultConfig AutoDiscover.spec
