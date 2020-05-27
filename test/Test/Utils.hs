{-# LANGUAGE FlexibleInstances #-}

module Test.Utils
    ( cli
    , describeCmd
    ) where

import Prelude

import System.Process
    ( readProcess, readProcessWithExitCode )

import Test.Hspec
    ( Spec, SpecWith, describe, runIO )

--
-- cli
--

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

exe :: String
exe = "cardano-address"

--
-- describeCmd
--

-- | Wrap HSpec 'describe' into a friendly command description. So that, we get
-- a very satisfying result visually from running the tests, and can inspect
-- what each command help text looks like.
describeCmd :: [String] -> SpecWith () -> Spec
describeCmd cmd spec = do
    title <- runIO $ cli (cmd ++ ["--help"]) ""
    describe title spec
