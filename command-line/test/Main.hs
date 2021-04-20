{-# LANGUAGE CPP #-}

module Main where

import Prelude

import Command
    ( withUtf8Encoding )
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
    withUtf8Encoding $ hspecWith defaultConfig AutoDiscover.spec
