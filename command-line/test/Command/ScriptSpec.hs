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
    ( requireSignatureOfParser )
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
    describe "requireSignatureOfParser : unit tests" $ do
        let verKeyH = "3c07030e36bfffe67e2e2ec09e5293d384637cd2f004356ef320f3fe" :: Text
        let (Right bytes) = fromBase16 $ T.encodeUtf8 verKeyH
        valuesParserUnitTest requireSignatureOfParser verKeyH
            (RequireSignatureOf $ VerificationKeyHash bytes)
        valuesParserUnitTest requireSignatureOfParser (verKeyH <> " ")
            (RequireSignatureOf $ VerificationKeyHash bytes)
        valuesParserUnitTest requireSignatureOfParser (verKeyH <>", ")
            (RequireSignatureOf $ VerificationKeyHash bytes)


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
        title = mempty
            <> " input=" <> show inp
            <> " expected=" <> show expected
