module Command.Key.PublicSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "public" ] $ do
    specKeyNotPrivate

specKeyNotPrivate :: SpecWith ()
specKeyNotPrivate = it "fail if key isn't private" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "public" ]
              >>= cli [ "key", "public" ]

    out `shouldBe` ""
    err `shouldContain` "Couldn't convert bytes into extended private key."
