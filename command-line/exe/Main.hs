{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude

import Command
    ( withUtf8Encoding )

import qualified Command as CLI

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
    withUtf8Encoding (CLI.setup >> CLI.parse >>= CLI.run)
