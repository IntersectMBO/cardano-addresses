{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Command.ScriptSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( ErrValidateScript (..), prettyErrValidateScript )
import Data.String.Interpolate
    ( iii )
import Options.Applicative.Script
    ( ScriptError (..), prettyScriptError )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = do
    describeCmd [ "script", "hash" ] $ do
        specScriptHashProper "7ab0402cd65ae95f6f1c8e7ba63d17ecb4b905d4e7635ffc83301eac"
            [iii|all [ #{verKeyH1} ]|]

        specScriptHashProper "7ab0402cd65ae95f6f1c8e7ba63d17ecb4b905d4e7635ffc83301eac"
            [iii|all [ #{verKeyH1Bech32} ]|]

        specScriptHashProper "7ab0402cd65ae95f6f1c8e7ba63d17ecb4b905d4e7635ffc83301eac"
            [iii|all [ #{verKeyH1Base58} ]|]

        specScriptHashProper "7ab0402cd65ae95f6f1c8e7ba63d17ecb4b905d4e7635ffc83301eac"
            ("all    [ " <>verKeyH1<>"  ] ")

        specScriptHashProper "5e26af6e6e0342b6f1360192f5c27a07a615af74d1146b46d15d859b"
            [iii|all [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "7787e991398b96e1171d87cd98d0148d45f8bcf80a574381a8645c05"
            [iii|any [ #{verKeyH1} ]|]

        specScriptHashProper "d25a9787f60b5c988fe5e7aad0d412682363ed0f00c045afe59e55a2"
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "d48f83b08759679b2f029d375f6586c9acccb9d86cf88ad2eb9361e0"
            [iii|at_least 1 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptHashProper "6f7ff5aee204c2d7e93cb0752ac1ead3a1370c692d9ff22c487de72d"
            [iii|at_least 1 [ #{verKeyH1}, all [ #{verKeyH2}, #{verKeyH3} ] ]|]

        specScriptParsingWrong
            [iii|wrong [ #{verKeyH1} ]|]

        specScriptParsingWrong
            [iii|any [ #{verKeyH1}, ]|]

        specScriptInvalid ListTooSmall
            [iii|at_least 4 [ #{verKeyH1}, #{verKeyH2}, #{verKeyH3} ]|]

        specScriptInvalid ListTooSmall
            [iii|at_least 1 [ #{verKeyH1}, at_least 2 [ #{verKeyH2} ] ]|]

        specScriptInvalid EmptyList
            [iii|all []|]

        specScriptInvalid EmptyList
            [iii|any [ #{verKeyH1}, all [] ]|]

        specScriptInvalid MZero
            [iii|at_least 0 [ #{verKeyH1}, #{verKeyH2} ]|]

        specScriptInvalid DuplicateSignatures
            [iii|any [ #{verKeyH1}, #{verKeyH2}, #{verKeyH1}]|]

        specScriptInvalid WrongKeyHash
            [iii|3c07030e36bfffe67e2e2ec09e5293d384637cd2|]

specScriptHashProper :: String -> String -> SpecWith ()
specScriptHashProper expected script = it (script <> " => " <> expected) $ do
    out <- cli ["script", "hash", "--base16", script] ""
    out `shouldBe` expected

specScriptParsingWrong :: String -> SpecWith ()
specScriptParsingWrong script = it "fails if a script is malformed" $ do
    (out, err) <- cli ["script", "hash", "--base16", script] ""
    out `shouldBe` ("" :: String)
    err `shouldContain` (prettyScriptError MalformedScript)

specScriptInvalid :: ErrValidateScript -> String -> SpecWith ()
specScriptInvalid errMsg script = it (script <> " => " <> show errMsg) $ do
    (out, err) <- cli ["script", "hash", "--base16", script] ""
    out `shouldBe` ("" :: String)
    err `shouldContain` (prettyErrValidateScript errMsg)

verKeyH1 :: String
verKeyH1 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe"

verKeyH1Bech32 :: String
verKeyH1Bech32 = "xpub_hash18srsxr3khll7vl3w9mqfu55n6wzxxlxj7qzr2mhnyrelufhy5t2"

verKeyH1Base58 :: String
verKeyH1Base58 = "covdomtKN9H7F5ZBTEXxZYL9u5mCTYNtrsafVf"

verKeyH2 :: String
verKeyH2 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333"

verKeyH3 :: String
verKeyH3 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344"
