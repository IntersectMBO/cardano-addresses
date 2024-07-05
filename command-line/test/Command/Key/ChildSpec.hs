{-# LANGUAGE FlexibleContexts #-}

module Command.Key.ChildSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( foldM )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain, shouldStartWith )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "child" ] $ do
    specChildValidPath "acct_xsk" ["1852H/1815H/0H"]
    specChildValidPath "acct_shared_xsk" ["1854H/1815H/0H"]
    specChildValidPath "addr_xsk" ["1852H/1815H/0H/0/0"]
    specChildValidPath "addr_xsk" ["1852H/1815H/0H", "0/0"]
    specChildValidPath "addr_shared_xsk" ["1854H/1815H/0H/0/0"]
    specChildValidPath "addr_shared_xsk" ["1854H/1815H/0H", "0/0"]
    specChildValidPath "addr_xsk" ["14H/42H"]
    specChildValidPath "policy_xsk" ["1855H/1815H/0H"]
    specChildValidPath "drep_xsk" ["1852H/1815H/0H/3/0"]
    specChildValidPath "drep_xsk" ["1852H/1815H/0H", "3/0"]
    specChildValidPath "drep_xsk" ["1852H/1815H/100H/3/0"]
    specChildValidPath "cc_cold_xsk" ["1852H/1815H/0H/4/0"]
    specChildValidPath "cc_cold_xsk" ["1852H/1815H/0H", "4/0"]
    specChildValidPath "cc_cold_xsk" ["1852H/1815H/100H", "4/0"]
    specChildValidPath "cc_hot_xsk" ["1852H/1815H/0H/5/0"]
    specChildValidPath "cc_hot_xsk" ["1852H/1815H/101H/5/0"]
    specChildValidPath "cc_hot_xsk" ["1852H/1815H/0H", "5/0"]

    specChildInvalidPath "from a parent root key" ["0H"]
    specChildInvalidPath "from a parent account key" ["1852H/1815H/0H", "0H"]

    specPublicDerivationUsesSoft

    specGovernanceWrongPaths "1852H/1815H/0H/3/1"
    specGovernanceWrongPaths "1857H/1815H/0H/3/1"
    specGovernanceWrongPaths "1852H/1815H/0H/4/10"
    specGovernanceWrongPaths "1858H/1815H/0H/4/10"
    specGovernanceWrongPaths "1852H/1815H/1H/5/101"
    specGovernanceWrongPaths "1859H/1815H/1H/5/101"

specChildValidPath :: String -> [String] -> SpecWith ()
specChildValidPath hrp paths = it ("Can derive given path(s) from root: " <> show paths) $ do
    out <- cli [ "recovery-phrase", "generate" ] ""
       >>= cli [ "key", "from-recovery-phrase", "shelley" ]
       >>= flip (foldM (\stdin path -> cli ["key", "child", path] stdin)) paths
    out `shouldStartWith` hrp

specChildInvalidPath :: String -> [String] -> SpecWith ()
specChildInvalidPath msg paths = it ("Cannot derive given path(s) from root: " <> show paths) $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= flip (foldM (\(stdin,_) path -> cli ["key", "child", path] stdin)) paths
    out `shouldBe` ""
    err `shouldContain` msg

specPublicDerivationUsesSoft :: SpecWith ()
specPublicDerivationUsesSoft = it "Public derivation requires soft indexes" $ do
    (out, err) <- cli [ "recovery-phrase", "generate" ] ""
              >>= cli [ "key", "from-recovery-phrase", "icarus" ]
              >>= cli [ "key", "child", "1852H/1815H/0H" ]
              >>= cli [ "key", "public", "--with-chain-code" ]
              >>= cli [ "key", "child", "14H/42H" ]
    out `shouldBe` ""
    err `shouldContain`
        "Couldn't derive child key. If you're trying to derive children on a \
        \PUBLIC key, you must use soft indexes only."

specGovernanceWrongPaths :: String -> SpecWith ()
specGovernanceWrongPaths segments = it "Derivation requires addr_ix=0 and 1852H" $ do
    out <- cli [ "recovery-phrase", "generate" ] ""
        >>= cli [ "key", "from-recovery-phrase", "shelley" ]
        >>= cli [ "key", "child", segments ]
    out `shouldContain` "addr_xsk1"
