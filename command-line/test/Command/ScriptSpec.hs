{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.ScriptSpec
    ( spec
    ) where

import Prelude

import Cardano.Multisig
    ( KeyHash (..), Script (..) )
import Codec.Binary.Encoding
    ( fromBase16 )
import Command.Script
    ( requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser
    , requireSignatureOfParser
    , scriptParser
    )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )
import Text.ParserCombinators.ReadP
    ( ReadP, readP_to_S )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do

    let verKeyH1 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe" :: Text
    let (Right bytes1) = fromBase16 $ T.encodeUtf8 verKeyH1
    let verKeyH2 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f333" :: Text
    let (Right bytes2) = fromBase16 $ T.encodeUtf8 verKeyH2
    let verKeyH3 = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f344" :: Text
    let (Right bytes3) = fromBase16 $ T.encodeUtf8 verKeyH3

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

    describe "requireSignatureOfParser : unit tests" $ do
        valuesParserUnitTest requireSignatureOfParser verKeyH1
            (RequireSignatureOf $ KeyHash bytes1)
        valuesParserUnitTest requireSignatureOfParser (verKeyH1 <> " ")
            (RequireSignatureOf $ KeyHash bytes1)
        valuesParserUnitTest requireSignatureOfParser (verKeyH1 <>", ")
            (RequireSignatureOf $ KeyHash bytes1)
        valuesParserUnitTest requireSignatureOfParser ("        " <> verKeyH1 <>", ")
            (RequireSignatureOf $ KeyHash bytes1)

    describe "requireAllOfParser : unit tests" $ do
        let expected1 = RequireAllOf
                [ RequireSignatureOf $ KeyHash bytes1 ]
        valuesParserUnitTest requireAllOfParser script1 expected1
        valuesParserUnitTest scriptParser script1 expected1

        valuesParserUnitTest requireAllOfParser script2 expected1
        valuesParserUnitTest scriptParser script2 expected1

        let expected2 = RequireAllOf
                [ RequireSignatureOf $ KeyHash bytes1
                , RequireSignatureOf $ KeyHash bytes2 ]
        valuesParserUnitTest requireAllOfParser script3 expected2
        valuesParserUnitTest scriptParser script3 expected2

        let expected3 = RequireAllOf
                [ RequireSignatureOf $ KeyHash bytes1
                , RequireSignatureOf $ KeyHash bytes2
                , RequireSignatureOf $ KeyHash bytes3 ]
        valuesParserUnitTest requireAllOfParser script4 expected3
        valuesParserUnitTest scriptParser script4 expected3

    describe "requireAnyOfParser : unit tests" $ do
        let expected1 = RequireAnyOf
                [ RequireSignatureOf $ KeyHash bytes1 ]
        valuesParserUnitTest requireAnyOfParser script5 expected1
        valuesParserUnitTest scriptParser script5 expected1

        valuesParserUnitTest requireAnyOfParser script6 expected1
        valuesParserUnitTest scriptParser script6 expected1

        let expected2 = RequireAnyOf
                [ RequireSignatureOf $ KeyHash bytes1
                , RequireSignatureOf $ KeyHash bytes2 ]
        valuesParserUnitTest requireAnyOfParser script7 expected2
        valuesParserUnitTest scriptParser script7 expected2

        let expected3 = RequireAnyOf
                [ RequireSignatureOf $ KeyHash bytes1
                , RequireSignatureOf $ KeyHash bytes2
                , RequireSignatureOf $ KeyHash bytes3 ]
        valuesParserUnitTest requireAnyOfParser script8 expected3
        valuesParserUnitTest scriptParser script8 expected3

        let expected4 = RequireAnyOf
                [ RequireSignatureOf $ KeyHash bytes1
                , RequireAllOf
                  [ RequireSignatureOf $ KeyHash bytes2
                  , RequireSignatureOf $ KeyHash bytes3 ]
                ]
        valuesParserUnitTest requireAnyOfParser script9 expected4
        valuesParserUnitTest scriptParser script9 expected4

    describe "requireAtLeastOfParser : unit tests" $ do
        let expected1 = RequireMOf 1
                [ RequireSignatureOf $ KeyHash bytes1
                , RequireSignatureOf $ KeyHash bytes2
                , RequireSignatureOf $ KeyHash bytes3 ]
        valuesParserUnitTest requireAtLeastOfParser script10 expected1
        valuesParserUnitTest scriptParser script10 expected1

        let expected2 = RequireMOf 1
                [ RequireSignatureOf $ KeyHash bytes1
                , RequireAllOf
                  [ RequireSignatureOf $ KeyHash bytes2
                  , RequireSignatureOf $ KeyHash bytes3 ]
                ]
        valuesParserUnitTest requireAtLeastOfParser script11 expected2
        valuesParserUnitTest scriptParser script11 expected2

    describeCmd [ "scripthash" ] $ do
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

        let scriptInvalid5 = "at_least 4 ["<>verKeyH1<>", "<>verKeyH2<>","<>verKeyH3<>"]"
        specScriptInvalid (T.unpack scriptInvalid5)

        let scriptInvalid6 = "at_least 1 ["<>verKeyH1<>", at_least 2 ["<>verKeyH2<>"]]"
        specScriptInvalid (T.unpack scriptInvalid6)


specScriptHashProper :: String -> String -> SpecWith ()
specScriptHashProper script expected = it "script hash working as expected" $ do
    out <- cli ["scripthash", "--base16"] script
    out `shouldBe` expected

specScriptParsingWrong :: String -> SpecWith ()
specScriptParsingWrong script = it "fails if wrong hash in a script" $ do
    (out, err) <- cli ["scripthash", "--base16"] script
    out `shouldBe` ("" :: String)
    err `shouldContain` ("Parsing of the script failed." :: String)

specScriptInvalid :: String -> SpecWith ()
specScriptInvalid script = it "fails if a correctly parsed script is invalid" $ do
    (out, err) <- cli ["scripthash", "--base16"] script
    out `shouldBe` ("" :: String)
    err `shouldContain` ("The script is invalid." :: String)

valuesParserUnitTest
    :: (Eq s, Show s)
    => ReadP s
    -> Text
    -> s
    -> SpecWith()
valuesParserUnitTest parser inp expected = it title $ do
    res <- case take 1 $ reverse $ readP_to_S parser (T.unpack inp) of
        [(res,_rest)] -> pure res
        err -> error $ "valuesParserUnitTest : "<>show err
    res `shouldBe` expected
    where
        title :: String
        title = mempty <> " input=" <> show inp
