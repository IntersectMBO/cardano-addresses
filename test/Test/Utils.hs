{-# LANGUAGE FlexibleInstances #-}

module Test.Utils
    ( cli
    ) where

import Prelude

import System.Process
    ( readProcess, readProcessWithExitCode )

exe :: String
exe = "cardano-address"

class CommandLine output where
    cli :: [String]
           -- ^ arguments
        -> String
            -- ^ stdin
        -> IO output
            -- ^ output, either stdout or (stdout, stderr)

instance CommandLine String where
    cli = readProcess exe

instance CommandLine (String, String) where
    cli args =
        fmap dropFirst . readProcessWithExitCode exe args
      where
        dropFirst (_,b,c) = (b, c)
