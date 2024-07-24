{-# LANGUAGE FlexibleContexts #-}

module Command.Key.PrivateSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain, shouldStartWith )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "private" ] $ do
    specKeyNotPrivate
    specKeyPrivate "icarus" "addr_sk" "1852H/1815H/0H/0/0" "--signing-key"

specKeyNotPrivate :: SpecWith ()
specKeyNotPrivate = it "fail if key isn't private" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "public", "--with-chain-code" ]
              >>= cli [ "key", "private", "--chain-code" ]

    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."

specKeyPrivate :: String -> String -> String -> String -> SpecWith ()
specKeyPrivate style hrp path cc = it "succeeds if key is private" $ do
    out <- cli [ "recovery-phrase", "generate" ] ""
       >>= cli [ "key", "from-recovery-phrase", style ]
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "private", cc ]
    out `shouldStartWith` hrp
