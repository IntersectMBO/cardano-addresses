{-# LANGUAGE CPP #-}

module Main where

import Prelude

import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover

#ifdef ghcjs_HOST_OS
import qualified Cardano.Address.Jsbits as Jsbits
    ( addJsbitsDependency )
#endif

main :: IO ()
main =
#ifdef ghcjs_HOST_OS
  do
    Jsbits.addJsbitsDependency
#endif
    hspecWith defaultConfig AutoDiscover.spec
