{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Address.Script.ParserSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..), Script (..) )
import Cardano.Address.Script.Parser
    ( requireAllOfParser
    , requireAnyOfParser
    , requireAtLeastOfParser
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
    let verKeyH1 = "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq" :: Text
    let kh1 = KeyHash Payment (unBech32 verKeyH1)
    let verKeyH2 = "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp" :: Text
    let kh2 = KeyHash Payment (unBech32 verKeyH2)
    let verKeyH3 = "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9" :: Text
    let kh3 = KeyHash Payment (unBech32 verKeyH3)

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
    let script12 = "all []"
    let script13 = "any ["<>verKeyH1<>", all [   ]]"
    let script14 = "all ["<>verKeyH1<>", active_from 120]"
    let script15 = "all ["<>verKeyH1<>", active_until 150]"
    let script16 = "all ["<>verKeyH1<>", active_from 120, active_until 125]"

    describe "requireSignatureOfParser : unit tests" $ do
        valuesParserUnitTest requireSignatureOfParser verKeyH1
            (RequireSignatureOf kh1)
        valuesParserUnitTest requireSignatureOfParser (verKeyH1 <> " ")
            (RequireSignatureOf kh1)
        valuesParserUnitTest requireSignatureOfParser (verKeyH1 <>", ")
            (RequireSignatureOf kh1)
        valuesParserUnitTest requireSignatureOfParser ("        " <> verKeyH1 <>", ")
            (RequireSignatureOf kh1)

    describe "requireAllOfParser : unit tests" $ do
        let expected1 = RequireAllOf
                [ RequireSignatureOf kh1 ]
        valuesParserUnitTest requireAllOfParser script1 expected1
        valuesParserUnitTest scriptParser script1 expected1

        valuesParserUnitTest requireAllOfParser script2 expected1
        valuesParserUnitTest scriptParser script2 expected1

        let expected2 = RequireAllOf
                [ RequireSignatureOf kh1
                , RequireSignatureOf kh2 ]
        valuesParserUnitTest requireAllOfParser script3 expected2
        valuesParserUnitTest scriptParser script3 expected2

        let expected3 = RequireAllOf
                [ RequireSignatureOf kh1
                , RequireSignatureOf kh2
                , RequireSignatureOf kh3 ]
        valuesParserUnitTest requireAllOfParser script4 expected3
        valuesParserUnitTest scriptParser script4 expected3

        let expected4 = RequireAllOf []
        valuesParserUnitTest requireAllOfParser script12 expected4
        valuesParserUnitTest scriptParser script12 expected4

    describe "requireAnyOfParser : unit tests" $ do
        let expected1 = RequireAnyOf
                [ RequireSignatureOf kh1 ]
        valuesParserUnitTest requireAnyOfParser script5 expected1
        valuesParserUnitTest scriptParser script5 expected1

        valuesParserUnitTest requireAnyOfParser script6 expected1
        valuesParserUnitTest scriptParser script6 expected1

        let expected2 = RequireAnyOf
                [ RequireSignatureOf kh1
                , RequireSignatureOf kh2 ]
        valuesParserUnitTest requireAnyOfParser script7 expected2
        valuesParserUnitTest scriptParser script7 expected2

        let expected3 = RequireAnyOf
                [ RequireSignatureOf kh1
                , RequireSignatureOf kh2
                , RequireSignatureOf kh3 ]
        valuesParserUnitTest requireAnyOfParser script8 expected3
        valuesParserUnitTest scriptParser script8 expected3

        let expected4 = RequireAnyOf
                [ RequireSignatureOf kh1
                , RequireAllOf
                  [ RequireSignatureOf kh2
                  , RequireSignatureOf kh3 ]
                ]
        valuesParserUnitTest requireAnyOfParser script9 expected4
        valuesParserUnitTest scriptParser script9 expected4

        let expected5 = RequireAnyOf
                [ RequireSignatureOf kh1
                , RequireAllOf []
                ]
        valuesParserUnitTest requireAnyOfParser script13 expected5
        valuesParserUnitTest scriptParser script13 expected5

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
