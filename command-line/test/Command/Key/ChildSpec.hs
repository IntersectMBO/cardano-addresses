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
    specChildValidPath "addr_xsk" ["1852H/1815H/0H/0/0"]
    specChildValidPath "addr_xsk" ["1852H/1815H/0H", "0/0"]
    specChildValidPath "addr_xsk" ["14H/42H"]

    specChildInvalidPath "from a parent root key" ["0H"]
    specChildInvalidPath "from a parent account key" ["1852H/1815H/0H", "0H"]

    specPublicDerivationUsesSoft

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
