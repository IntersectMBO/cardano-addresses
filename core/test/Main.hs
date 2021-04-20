module Main where

import Prelude

import Cardano.Address.Jsbits
    ( addJsbitsDependency )
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover

main :: IO ()
main = do
    addJsbitsDependency
    hspecWith defaultConfig AutoDiscover.spec
