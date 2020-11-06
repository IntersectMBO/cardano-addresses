{-# LANGUAGE FlexibleContexts #-}

module Command.Key.HashSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain, shouldStartWith )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "hash" ] $ do
    specKeyNotPublic
    specKeyPublic "addr_vkh"   "1852H/1815H/0H/0/0"
    specKeyPublic "addr_vkh"   "1852H/1815H/0H/1/0"
    specKeyPublic "stake_vkh"  "1852H/1815H/0H/2/0"
    specKeyPublic "script_vkh" "1852H/1815H/0H/3/0"
    specKeyPublic "script_vkh" "1852H/1815H/0H/4/0"

specKeyNotPublic :: SpecWith ()
specKeyNotPublic = it "fail if key isn't public" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "hash" ]

    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."

specKeyPublic :: String -> String -> SpecWith ()
specKeyPublic hrp path = it "succeds if key is public" $ do
    out <- cli [ "recovery-phrase", "generate" ] ""
       >>= cli [ "key", "from-recovery-phrase", "icarus" ]
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--without-chain-code" ]
       >>= cli [ "key", "hash" ]
    out `shouldStartWith` hrp
