{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.ScriptSpec
    ( spec
    ) where

import Prelude

import Cardano.Multisig
    ( MultisigScript (..), VerificationKeyHash (..) )
import Codec.Binary.Encoding
    ( fromBase16 )
import Command.Script
    ( requireAllOfParser, requireSignatureOfParser )
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

    describe "requireSignatureOfParser : unit tests" $ do
        valuesParserUnitTest requireSignatureOfParser verKeyH1
            (RequireSignatureOf $ VerificationKeyHash bytes1)
        valuesParserUnitTest requireSignatureOfParser (verKeyH1 <> " ")
            (RequireSignatureOf $ VerificationKeyHash bytes1)
        valuesParserUnitTest requireSignatureOfParser (verKeyH1 <>", ")
            (RequireSignatureOf $ VerificationKeyHash bytes1)
        valuesParserUnitTest requireSignatureOfParser ("        " <> verKeyH1 <>", ")
            (RequireSignatureOf $ VerificationKeyHash bytes1)

    describe "requireAllOfParser : unit tests" $ do
        let script1 = "all ["<>verKeyH1<>"]"
        let expected1 = RequireAllOf
                [ RequireSignatureOf $ VerificationKeyHash bytes1 ]
        valuesParserUnitTest requireAllOfParser script1 expected1

        let script2 = " all   [ "<>verKeyH1<>"  ] "
        valuesParserUnitTest requireAllOfParser script2 expected1

        let script3 = "all ["<>verKeyH1<>", "<>verKeyH2<>"]"
        let expected2 = RequireAllOf
                [ RequireSignatureOf $ VerificationKeyHash bytes1
                , RequireSignatureOf $ VerificationKeyHash bytes2 ]
        valuesParserUnitTest requireAllOfParser script3 expected2

        let script4 = "all ["<>verKeyH1<>", "<>verKeyH2<>","<>verKeyH3<>"]"
        let expected3 = RequireAllOf
                [ RequireSignatureOf $ VerificationKeyHash bytes1
                , RequireSignatureOf $ VerificationKeyHash bytes2
                , RequireSignatureOf $ VerificationKeyHash bytes3 ]
        valuesParserUnitTest requireAllOfParser script4 expected3


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
