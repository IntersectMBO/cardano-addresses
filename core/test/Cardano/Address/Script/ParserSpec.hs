{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Address.Script.ParserSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner (..), KeyHash (..), KeyRole (..), Script (..) )
import Cardano.Address.Script.Parser
    ( requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser
    , requireCosignerOfParser
    , requireCosignerOfParser
    , requireSignatureOfParser
    , scriptParser
    )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Text.ParserCombinators.ReadP
    ( ReadP, readP_to_S )

import qualified Codec.Binary.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    requireOfParserTests @KeyHash requireSignatureOfParser
        (kh1,verKeyH1) "requireSignatureOfParser"
    requireOfParserTests @Cosigner requireCosignerOfParser
        (cosigner0,cosigner0Txt) "requireCosignerOfParser"

    requireAllOfParserTests @KeyHash requireSignatureOfParser
        [(kh1,verKeyH1), (kh2,verKeyH2), (kh3,verKeyH3)]
    requireAllOfParserTests @Cosigner requireCosignerOfParser
        [(cosigner0,cosigner0Txt), (cosigner1,cosigner1Txt), (cosigner2,cosigner2Txt)]

    requireAnyOfParserTests @KeyHash requireSignatureOfParser
        [(kh1,verKeyH1), (kh2,verKeyH2), (kh3,verKeyH3)]
    requireAnyOfParserTests @Cosigner requireCosignerOfParser
        [(cosigner0,cosigner0Txt), (cosigner1,cosigner1Txt), (cosigner2,cosigner2Txt)]

{--
    describe "requireAtLeastOfParser : unit tests" $ do
        let expected1 = RequireSomeOf 1
                [ RequireSignatureOf kh1
                , RequireSignatureOf kh2
                , RequireSignatureOf kh3 ]
        valuesParserUnitTest requireAtLeastOfParser script10 expected1
        valuesParserUnitTest scriptParser script10 expected1

        let expected2 = RequireSomeOf 1
                [ RequireSignatureOf kh1
                , RequireAllOf
                  [ RequireSignatureOf kh2
                  , RequireSignatureOf kh3 ]
                ]
        valuesParserUnitTest requireAtLeastOfParser script11 expected2
        valuesParserUnitTest scriptParser script11 expected2

    describe "validFromSlot unit test" $ do
        let expected = RequireAllOf
                [ RequireSignatureOf kh1
                , ActiveFromSlot 120 ]
        valuesParserUnitTest scriptParser script14 expected

    describe "validUntilSlot unit test" $ do
        let expected = RequireAllOf
                [ RequireSignatureOf kh1
                , ActiveUntilSlot 150 ]
        valuesParserUnitTest scriptParser script15 expected

    describe "validUntilSlot and validFromSlot unit test" $ do
        let expected = RequireAllOf
                [ RequireSignatureOf kh1
                , ActiveFromSlot 120
                , ActiveUntilSlot 125 ]
        valuesParserUnitTest scriptParser script16 expected
--}
  where
    verKeyH1 = "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq" :: Text
    kh1 = KeyHash Payment (unBech32 verKeyH1)
    verKeyH2 = "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp" :: Text
    kh2 = KeyHash Payment (unBech32 verKeyH2)
    verKeyH3 = "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9" :: Text
    kh3 = KeyHash Payment (unBech32 verKeyH3)

    script1 txt = "all ["<>txt<>"]"
    script2 txt = " all   [ "<>txt<>"  ] "
    script3 txt1 txt2 = "all ["<>txt1<>", "<>txt2<>"]"
    script4 txt1 txt2 txt3 = "all ["<>txt1<>", "<>txt2<>","<>txt3<>"]"
    script5 txt = "any ["<>txt<>"]"
    script6 txt = " any   [ "<>txt<>"  ] "
    script7 txt1 txt2 = "any ["<>txt1<>", "<>txt2<>"]"
    script8 txt1 txt2 txt3 = "any ["<>txt1<>", "<>txt2<>","<>txt3<>"]"
    script9 txt1 txt2 txt3 = "any ["<>txt1<>", all ["<>txt2<>","<>txt3<>"]]"
    script10 txt1 txt2 txt3 = "at_least 1 ["<>txt1<>", "<>txt2<>","<>txt3<>"]"
    script11 txt1 txt2 txt3 = "at_least 1 ["<>txt1<>", all ["<>txt2<>","<>txt3<>"]]"
    script12 = "all []"
    script13 txt = "any ["<>txt<>", all [   ]]"
    script14 txt = "all ["<>txt<>", active_from 120]"
    script15 txt = "all ["<>txt<>", active_until 150]"
    script16 txt = "all ["<>txt<>", active_from 120, active_until 125]"

    cosigner0Txt = "cosigner#0" :: Text
    cosigner0 = Cosigner 0
    cosigner1Txt = "cosigner#1" :: Text
    cosigner1 = Cosigner 1
    cosigner2Txt = "cosigner#2" :: Text
    cosigner2 = Cosigner 2

    requireOfParserTests
        :: (Eq a, Show a)
        => ReadP (Script a)
        -> (a, Text)
        -> String
        -> SpecWith ()
    requireOfParserTests parser (obj, txt) descr =
        describe (descr <> " : unit tests") $ do
            valuesParserUnitTest parser txt
                (RequireSignatureOf obj)
            valuesParserUnitTest parser (txt <> " ")
                (RequireSignatureOf obj)
            valuesParserUnitTest parser (txt <>", ")
                (RequireSignatureOf obj)
            valuesParserUnitTest parser ("        " <> txt <>", ")
                (RequireSignatureOf obj)

    requireAllOfParserTests
        :: (Eq a, Show a)
        => ReadP (Script a)
        -> [(a, Text)]
        -> SpecWith ()
    requireAllOfParserTests parser objTxts = do
        let [(obj1, txt1),(obj2, txt2),(obj3, txt3)] = objTxts
        describe "requireAllOfParser : unit tests" $ do
            let expected1 = RequireAllOf
                    [ RequireSignatureOf obj1 ]
            valuesParserUnitTest (requireAllOfParser parser) (script1 txt1) expected1
            valuesParserUnitTest (scriptParser parser) (script1 txt1) expected1

            valuesParserUnitTest (requireAllOfParser parser) (script2 txt1) expected1
            valuesParserUnitTest (scriptParser parser) (script2 txt1) expected1

            let expected2 = RequireAllOf
                    [ RequireSignatureOf obj1
                    , RequireSignatureOf obj2 ]
            valuesParserUnitTest (requireAllOfParser parser) (script3 txt1 txt2) expected2
            valuesParserUnitTest (scriptParser parser) (script3 txt1 txt2) expected2

            let expected3 = RequireAllOf
                    [ RequireSignatureOf obj1
                    , RequireSignatureOf obj2
                    , RequireSignatureOf obj3 ]
            valuesParserUnitTest (requireAllOfParser parser) (script4 txt1 txt2 txt3) expected3
            valuesParserUnitTest (scriptParser parser) (script4 txt1 txt2 txt3) expected3

            let expected4 = RequireAllOf []
            valuesParserUnitTest (requireAllOfParser parser) script12 expected4
            valuesParserUnitTest (scriptParser parser) script12 expected4

    requireAnyOfParserTests
        :: (Eq a, Show a)
        => ReadP (Script a)
        -> [(a, Text)]
        -> SpecWith ()
    requireAnyOfParserTests parser objTxts = do
        let [(obj1, txt1),(obj2, txt2),(obj3, txt3)] = objTxts
        describe "requireAnyOfParser : unit tests" $ do
            let expected1 = RequireAnyOf
                    [ RequireSignatureOf obj1 ]
            valuesParserUnitTest (requireAnyOfParser parser) (script5 txt1) expected1
            valuesParserUnitTest (scriptParser parser) (script5 txt1) expected1

            valuesParserUnitTest (requireAnyOfParser parser) (script6 txt1) expected1
            valuesParserUnitTest (scriptParser parser) (script6 txt1) expected1

            let expected2 = RequireAnyOf
                    [ RequireSignatureOf obj1
                    , RequireSignatureOf obj2 ]
            valuesParserUnitTest (requireAnyOfParser parser) (script7 txt1 txt2) expected2
            valuesParserUnitTest (scriptParser parser) (script7 txt1 txt2) expected2

            let expected3 = RequireAnyOf
                    [ RequireSignatureOf obj1
                    , RequireSignatureOf obj2
                    , RequireSignatureOf obj3 ]
            valuesParserUnitTest (requireAnyOfParser parser) (script8 txt1 txt2 txt3) expected3
            valuesParserUnitTest (scriptParser parser) (script8 txt1 txt2 txt3) expected3

            let expected4 = RequireAnyOf
                    [ RequireSignatureOf obj1
                    , RequireAllOf
                      [ RequireSignatureOf obj2
                      , RequireSignatureOf obj3 ]
                    ]
            valuesParserUnitTest (requireAnyOfParser parser) (script9 txt1 txt2 txt3) expected4
            valuesParserUnitTest (scriptParser parser) (script9 txt1 txt2 txt3) expected4

            let expected5 = RequireAnyOf
                    [ RequireSignatureOf obj1
                    , RequireAllOf []
                    ]
            valuesParserUnitTest (requireAnyOfParser parser) (script13 txt1) expected5
            valuesParserUnitTest (scriptParser parser) (script13 txt1) expected5


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

-- | Unsafe function to get bech32 data part from test data.
unBech32 :: Text -> ByteString
unBech32 = either (error "Incorrect bech32 in test data") snd
    . E.fromBech32 (const id)
    . T.encodeUtf8
