{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.ScriptSpec
    ( spec
    ) where

import Prelude

import Cardano.Script
    ( InvalidScriptError (..), ScriptError (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

import qualified Data.Text as T

spec :: Spec
spec = do

    let verKeyH1 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe" :: Text
    let verKeyH2 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333" :: Text
    let verKeyH3 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344" :: Text

    let script1 = "all ["<>verKeyH1<>"]"
    let script2 = " all   [ "<>verKeyH1<>"  ] "
    let script3 = "all ["<>verKeyH1<>", "<>verKeyH2<>"]"
    let script4 = "all ["<>verKeyH1<>", "<>verKeyH2<>","<>verKeyH3<>"]"
    let script5 = "any ["<>verKeyH1<>"]"
    let script6 = " any   [ "<>verKeyH1<>"  ] "
    let script7 = "any ["<>verKeyH1<>", "<>verKeyH2<>"]"
    let script8 = "any ["<>verKeyH1<>", "<>verKeyH2<>","<>verKeyH3<>"]"
    let script9 = "any ["<>verKeyH1<>", all ["<>verKeyH2<>","<>verKeyH3<>"]]"
    let script10 = "at_least 1 ["<>verKeyH1<>", "<>verKeyH2<>","<>verKeyH3<>"]"
    let script11 = "at_least 1 ["<>verKeyH1<>", all ["<>verKeyH2<>","<>verKeyH3<>"]]"

    describeCmd [ "script", "hash" ] $ do
        specScriptHashProper (T.unpack script1)
            "7ab0402cd65ae95f6f1c8e7ba63d17ecb4b905d4e7635ffc83301eac"
        specScriptHashProper (T.unpack script2)
            "7ab0402cd65ae95f6f1c8e7ba63d17ecb4b905d4e7635ffc83301eac"
        specScriptHashProper (T.unpack script3)
            "a015ae61075e25c3d9250bdcbc35c6557272127927ecf2a2d716e29f"
        specScriptHashProper (T.unpack script4)
            "5e26af6e6e0342b6f1360192f5c27a07a615af74d1146b46d15d859b"
        specScriptHashProper (T.unpack script5)
            "7787e991398b96e1171d87cd98d0148d45f8bcf80a574381a8645c05"
        specScriptHashProper (T.unpack script6)
            "7787e991398b96e1171d87cd98d0148d45f8bcf80a574381a8645c05"
        specScriptHashProper (T.unpack script7)
            "74ca76e79e59dbe4b1ee617376e54f97ecfa6ac73dd4da2aef606bc7"
        specScriptHashProper (T.unpack script8)
            "d25a9787f60b5c988fe5e7aad0d412682363ed0f00c045afe59e55a2"
        specScriptHashProper (T.unpack script9)
            "999bc2ba46ef5f4767686a4128191217757ec97c4d1a81563c87daf5"
        specScriptHashProper (T.unpack script10)
            "d48f83b08759679b2f029d375f6586c9acccb9d86cf88ad2eb9361e0"
        specScriptHashProper (T.unpack script11)
            "6f7ff5aee204c2d7e93cb0752ac1ead3a1370c692d9ff22c487de72d"

        let verKeyWrong = "3c07030e36bfffe67e2e2ec09e5293d384637cd2" :: Text
        specScriptParsingWrong (T.unpack verKeyWrong)

        let scriptWrong1 = "wrong ["<>verKeyH1<>"]"
        specScriptParsingWrong (T.unpack scriptWrong1)

        let scriptWrong2 = " any   [ "<>verKeyH1<>",  ] "
        specScriptParsingWrong (T.unpack scriptWrong2)

        let scriptInvalid1 = "at_least 4 ["<>verKeyH1<>", "<>verKeyH2<>","<>verKeyH3<>"]"
        specScriptInvalid (T.unpack scriptInvalid1) (InvalidScript ListTooSmall)

        let scriptInvalid2 = "at_least 1 ["<>verKeyH1<>", at_least 2 ["<>verKeyH2<>"]]"
        specScriptInvalid (T.unpack scriptInvalid2) (InvalidScript ListTooSmall)

        let scriptInvalid3 = "all []"
        specScriptInvalid (T.unpack scriptInvalid3) (InvalidScript EmptyList)

        let scriptInvalid4 = "any ["<>verKeyH1<>", all [   ]]"
        specScriptInvalid (T.unpack scriptInvalid4) (InvalidScript EmptyList)

        let scriptInvalid5 = "at_least 0 ["<>verKeyH1<>"," <>verKeyH2<>"]"
        specScriptInvalid (T.unpack scriptInvalid5) (InvalidScript MZero)

        let scriptInvalid6 = "any ["<>verKeyH1<>", "<>verKeyH2<>","<>verKeyH1<>"]"
        specScriptInvalid (T.unpack scriptInvalid6) (InvalidScript DuplicateSignatures)


specScriptHashProper :: String -> String -> SpecWith ()
specScriptHashProper script expected = it "script hash working as expected" $ do
    out <- cli ["script", "hash", "--base16", script] ""
    out `shouldBe` expected

specScriptParsingWrong :: String -> SpecWith ()
specScriptParsingWrong script = it "fails if wrong hash in a script" $ do
    (out, err) <- cli ["script", "hash", "--base16", script] ""
    out `shouldBe` ("" :: String)
    err `shouldContain` ("Parsing of the script failed." :: String)

specScriptInvalid :: String -> ScriptError -> SpecWith ()
specScriptInvalid script errMsg = it "fails if a correctly parsed script is invalid" $ do
    (out, err) <- cli ["script", "hash", "--base16", script] ""
    out `shouldBe` ("" :: String)
    err `shouldContain` (show errMsg)
