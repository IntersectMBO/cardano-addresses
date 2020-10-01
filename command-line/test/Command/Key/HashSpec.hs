{-# LANGUAGE FlexibleContexts #-}

module Command.Key.HashSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "hash" ] $ do
    specKeyNotPublic
    specKeyPublic

specKeyNotPublic :: SpecWith ()
specKeyNotPublic = it "fail if key isn't public" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "hash" ]

    out `shouldBe` ""
    err `shouldContain` "Couldn't convert bytes into extended public key."

specKeyPublic :: SpecWith ()
specKeyPublic = it "succeds if key is public" $ do
    out <- cli [ "recovery-phrase", "generate" ] ""
           >>= cli [ "key", "from-recovery-phrase", "icarus" ]
           >>= cli [ "key", "public" ]
           >>= cli [ "key", "hash" ]
    out `shouldContain` "xpub1"
