module Main where

import Prelude

import Cardano.Address.Jsbits
    ( addJsbitsDependency )
import Command
    ( withUtf8Encoding )
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover


main :: IO ()
main = do
    addJsbitsDependency
    withUtf8Encoding $ hspecWith defaultConfig AutoDiscover.spec
