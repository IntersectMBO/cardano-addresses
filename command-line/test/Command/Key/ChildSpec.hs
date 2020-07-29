{-# LANGUAGE FlexibleContexts #-}

module Command.Key.ChildSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "child" ] $ do
    specPublicDerivationUsesSoft

specPublicDerivationUsesSoft :: SpecWith ()
specPublicDerivationUsesSoft = it "Public derivation requires soft indexes" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "public" ]
              >>= cli [ "key", "child", "14H" ]

    out `shouldBe` ""
    err `shouldContain`
        "Couldn't derive child key. If you're trying to derive children on a \
        \PUBLIC key, you must use soft indexes only."
