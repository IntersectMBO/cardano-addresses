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

    specKeyPrivate "icarus" "acct_sk" "1852H/1815H/0H" "--signing-key"
    specKeyPrivate "icarus" "addr_sk" "1852H/1815H/0H/0/0" "--signing-key"

    specKeyPrivate "shelley" "acct_sk" "1852H/1815H/0H" "--signing-key"
    specKeyPrivate "shelley" "addr_sk" "1852H/1815H/0H/0/0" "--signing-key"
    specKeyPrivate "shelley" "stake_sk" "1852H/1815H/0H/2/0" "--signing-key"
    specKeyPrivate "shelley" "policy_sk" "1855H/1815H/0H" "--signing-key"

    specKeyPrivate "shared" "acct_shared_sk" "1854H/1815H/0H" "--signing-key"
    specKeyPrivate "shared" "addr_shared_sk" "1854H/1815H/0H/0/0" "--signing-key"
    specKeyPrivate "shared" "stake_shared_sk" "1854H/1815H/0H/2/0" "--signing-key"

    specKeyPrivate "shelley" "drep_sk" "1852H/1815H/0H/3/0" "--signing-key"
    specKeyPrivate "shelley" "cc_cold_sk" "1852H/1815H/0H/4/0" "--signing-key"
    specKeyPrivate "shelley" "cc_hot_sk" "1852H/1815H/0H/5/0" "--signing-key"

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
