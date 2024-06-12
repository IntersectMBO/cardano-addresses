{-# LANGUAGE CPP #-}
module Main where

import Prelude

import Cardano.Address.Compat
    ( ghcjsBuildSupport )
import Main.Utf8
    ( withUtf8 )
import Test.Hspec.Runner
    ( defaultConfig, hspecWith )

import qualified AutoDiscover

-- This is effectively from the now defunct foundation/basement from
--  https://github.com/haskell-foundation/foundation/blob/5e28e3ea1e2fe9a98c157df463bd32d3f92e7f80/basement/Basement/Terminal.hs#L14-L26
#ifdef mingw32_HOST_OS
import System.IO (hSetEncoding, utf8, hPutStrLn, stderr, stdin, stdout)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP, getConsoleCP, getConsoleOutputCP)
#endif

initialize :: IO ()
initialize = do
#ifdef mingw32_HOST_OS
    query getConsoleOutputCP (\e -> setConsoleOutputCP e >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8) utf8Code
    query getConsoleCP (\e -> setConsoleCP e >> hSetEncoding stdin utf8) utf8Code
  where
    utf8Code = 65001
    query get set expected = do
        v <- get
        if v == expected then pure () else set expected
#else
    pure ()
#endif

main :: IO ()
main = do
    ghcjsBuildSupport
    initialize
    withUtf8 $ hspecWith defaultConfig AutoDiscover.spec
