{-# LANGUAGE FlexibleContexts #-}

module Command.RecoveryPhraseSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, it )
import Test.Utils
    ( describeCmd )

spec :: Spec
spec = describeCmd ["recovery-phrase"] $ do
    it "N/A" (pure () :: IO ())
