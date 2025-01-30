{-# LANGUAGE FlexibleContexts #-}

module CommandSpec
    ( spec
    ) where

import Prelude

import Data.Version
    ( showVersion )
import Paths_cardano_addresses_cli
    ( version )
import Test.Hspec
    ( Spec, it, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [] $ do
    it "Show version when --version is provided" $ do
        out <- cli ["--version"] ""
        out `shouldContain` showVersion version

    it "Show version when -v is provided" $ do
        out <- cli ["-v"] ""
        out `shouldContain` showVersion version

    it "Show version when version is provided" $ do
        out <- cli ["version"] ""
        out `shouldContain` showVersion version
