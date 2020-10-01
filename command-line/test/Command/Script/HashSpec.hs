{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Script.HashSpec
    ( spec
    ) where

import Prelude

import Cardano.Multisig
    ( KeyHash (..), Script (..) )
import Codec.Binary.Encoding
    ( fromBase16 )
import Command.Script.Hash
    ( requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser
    , requireSignatureOfParser
    , scriptParser
    )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
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
