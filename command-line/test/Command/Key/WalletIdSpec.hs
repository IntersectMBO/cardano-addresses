{-# LANGUAGE FlexibleContexts #-}

module Command.Key.WalletIdSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "walletid" ] $ do
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/0/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/0/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/1/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/1/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/2/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shelley" "1852H/1815H/0H/2/0" "--with-chain-code"

    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/0/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/0/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/1/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/1/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/2/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "icarus" "1852H/1815H/0H/2/0" "--with-chain-code"

    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/0/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/0/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/1/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/1/0" "--with-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/2/0" "--without-chain-code"
    specKeyNeitherRootNorAcct "shared" "1854H/1815H/0H/2/0" "--with-chain-code"

specKeyNeitherRootNorAcct :: String -> String -> String -> SpecWith ()
specKeyNeitherRootNorAcct style path cc = it "fails if key is nether root nor account" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", style ]
              >>= cli [ "key", "child", path ]
              >>= cli [ "key", "public", cc ]
              >>= cli [ "key", "walletid"]
    out `shouldBe` ""
    err `shouldContain` "Invalid human-readable prefix."
