module Command.RecoveryPhrase.GenerateSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe, shouldContain )
import Test.Utils
    ( cli )

spec :: Spec
spec = describe "recovery-phrase generate" $ do
    specDefaultSize
    mapM_ specSpecificSize [9,12,15,18,21,24]
    mapM_ specInvalidSize ["15.5","3","6","14","abc","👌","0","~!@#%","-1000","1000"]

specDefaultSize :: SpecWith ()
specDefaultSize = it "ø; default size" $ do
    out <- cli ["recovery-phrase", "generate"] mempty
    length (words out) `shouldBe` 15

specSpecificSize :: Int -> SpecWith ()
specSpecificSize n = it ("--size=" <> show n <> "; valid size") $ do
    out <- cli ["recovery-phrase", "generate", "--size", show n] ""
    length (words out) `shouldBe` n

specInvalidSize :: String -> SpecWith ()
specInvalidSize x = it ("--size=" <> x <> "; invalid size") $ do
    (out, err) <- cli ["recovery-phrase", "generate", "--size", x] ""
    out `shouldBe` ""
    err `shouldContain` "Invalid mnemonic size. Expected one of: 9, 12, 15, 18, 21, 24."
