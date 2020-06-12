{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Binary.EncodingSpec
    ( spec
    ) where

import Prelude

import Codec.Binary.Encoding
    ( AbstractEncoding (..), detectEncoding, markCharsRedAtIndices )
import Data.List
    ( nub )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , counterexample
    , forAllShrink
    , property
    , vector
    , (===)
    , (==>)
    )

import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "detectEncoding" $ do
        specDetectEncoding (EBech32 ())
            "ed25519_sk1l25926aaaf7ty55g99r285wptc7nqrzager5jghurqdswklj4pvszzp8qg"

    describe "markCharsRedAtIndices" $ do
        prop "generates strings of expected length"
            propMarkedStringsExpectedLength
        prop "all red chars correspond to indices"
            propRedCharsMatch

specDetectEncoding
    :: AbstractEncoding ()
    -> String
    -> SpecWith ()
specDetectEncoding expected str =
    it (str <> " is " <> pretty expected) $
        detectEncoding str `shouldBe` Just expected
  where
    pretty = \case
        EBech32{} -> "bech32"
        EBase16{} -> "base16"
        EBase58{} -> "base58"

propMarkedStringsExpectedLength
    :: [Word]
    -> Property
propMarkedStringsExpectedLength ixs = do
    let maxIx = fromIntegral $ foldl max 0 ixs
    let genStr = choose (maxIx, maxIx + 5) >>= vector
    forAllShrink genStr shrink $ \s -> do
        let rendered = markCharsRedAtIndices ixs s
        all ((< length s) . fromIntegral) ixs ==>
            counterexample rendered $
                length rendered === length s + ((length (nub ixs)) * 9)

propRedCharsMatch
    :: [Word]
    -> Property
propRedCharsMatch ixs = do
    let maxIx = fromIntegral $ foldl max 0 ixs
    let genStr = choose (maxIx, maxIx + 5) >>= vector
    forAllShrink genStr shrink $ \(s::String) -> do
        let rendered = markCharsRedAtIndices ixs s
        let ixs' = indicesOfRedCharacters rendered
        if length s > maxIx
        then Set.fromList ixs' === Set.fromList ixs
        else property $
            Set.fromList ixs' `Set.isSubsetOf` Set.fromList ixs

-- Returns a list of indices of charcters marked red with ANSI.
--
-- NOTE: Very primitive parser that only works with the current
-- @markCharsRedAtIndices@ which surrounds /every/ red character with ANSI, even
-- for neighboring characters.
indicesOfRedCharacters :: Integral i => String -> [i]
indicesOfRedCharacters s = go s 0
  where
    go ('\ESC':'[':'9':'1':'m':_x:'\ESC':'[':'0':'m':xs) n =
        n : (go xs (n + 1))
    go (_x:xs) n =
        go xs (n + 1)
    go [] _ = []
