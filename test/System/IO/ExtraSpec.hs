{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.IO.ExtraSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvToBytes, xpubToBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), Encoding, encode )
import Control.Exception
    ( IOException, SomeException, try )
import Data.Bifunctor
    ( bimap )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.List
    ( nub )
import Options.Applicative.Style
    ( PassphraseInfo (..), PassphraseInput (..), PassphraseInputMode (..) )
import System.IO
    ( BufferMode (..)
    , Handle
    , IOMode (..)
    , hClose
    , hSetBuffering
    , stderr
    , stdin
    , withFile
    )
import System.IO.Extra
    ( hGetBytes
    , hGetPassphraseBytes
    , hGetXP__
    , hGetXPrv
    , hGetXPub
    , hPutBytes
    , markCharsRedAtIndices
    )
import System.IO.Temp
    ( withSystemTempFile )
import Test.Hspec
    ( Spec
    , SpecWith
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldContain
    , shouldNotContain
    )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , choose
    , counterexample
    , elements
    , forAll
    , forAllShrink
    , property
    , vector
    , withNumTests
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import Test.Arbitrary
    ()

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "hGetBytes" $ do
        specRoundtrip "MyString" bech32
        specRoundtrip "MyString" base16
        specRoundtrip "MyString" base58

        specInvalidEncodedString "bech321aaaaaaaaaaaaaaaa"
            (`shouldContain` "Bech32 error: Invalid character(s) in string")
        specInvalidEncodedString "bech321f4u4xarjd9hxwrhö"
            (`shouldContain` "Couldn't detect input encoding")
        specInvalidEncodedString "\NUL\NUL"
            (`shouldContain` "Couldn't detect input encoding")

    describe "hGetPassphraseBytes (from-octets)" $ do
        let valid input expected = it ("accepts " <> show input) $ do
                result <- try $ readOctetsFromFile input
                (result :: Either IOException ByteString) `shouldBe` Right expected
        let invalid input = it ("rejects " <> show input) $ do
                result <- try $ readOctetsFromFile input
                case (result :: Either IOException ByteString) of
                    Left (e :: IOException) -> do
                        show e `shouldContain` "octet array"
                        show e `shouldNotContain` "Prelude.read"
                    Right bytes -> expectationFailure $
                        "expected " <> show input <> " to be rejected, got "
                        <> show bytes

        valid "[1,2,3]" (BS.pack [1, 2, 3])
        valid "[0,255]" (BS.pack [0, 255])
        valid "[]" BS.empty
        valid "[1, 2, 3]\n" (BS.pack [1, 2, 3])
        valid " [1, 2, 3] " (BS.pack [1, 2, 3])

        invalid "hello"
        invalid "[256]"
        invalid "[-1]"
        invalid "[1,2"
        invalid ""
        invalid "0x0102"
        invalid "[1,2]trailing"
        invalid "[1, 2, 300]"

    describe "markCharsRedAtIndices" $ do
        prop "generates strings of expected length"
            propMarkedStringsExpectedLength
        prop "all red chars correspond to indices"
            propRedCharsMatch

    describe "hGetXPrv" $ do
        prop "roundtrip: hGetXPrv vs hPutBytes" $
            withNumTests 1000 propGetPrvRoundtrip

    describe "hGetXPub" $ do
        prop "roundtrip: hGetXPub vs hPutBytes" $
            withNumTests 1000 propGetPubRoundtrip

    describe "hGetXP__" $ do
        prop "roundtrip: hGetXP__ vs hPutBytes" $
            withNumTests 1000 propGetAnyRoundtrip

specRoundtrip
    :: String
    -> (String -> ByteString)
    -> SpecWith ()
specRoundtrip str encoder = it (str <> " => " <> unbytes (encoder str)) $ do
    str' <- withHandle
        (\h -> B8.hPutStr h (encoder str))
        (fmap unbytes . hGetBytes)
    str `shouldBe` str'

specInvalidEncodedString
    :: String
    -> (String -> IO ())
    -> SpecWith ()
specInvalidEncodedString str assertion = it ("invalid encoding; " <> str) $ do
    result <- try $ withHandle (\h -> B8.hPutStr h (bytes str)) hGetBytes
    case result of
        Left (e :: SomeException) ->
            assertion (show e)
        Right{} ->
            expectationFailure "expected hGetBytes to fail but didn't"

base16 :: String -> ByteString
base16 = encode EBase16 . bytes

bech32 :: String -> ByteString
bech32 = encode (EBech32 hrp) . bytes
  where
    hrp = [humanReadablePart|bech32|]

base58 :: String -> ByteString
base58  = encode EBase58 . bytes

propGetPrvRoundtrip
    :: XPrv
    -> Property
propGetPrvRoundtrip xprv = monadicIO $ do
    result <- run $ try $ withHandle
        (\h -> hPutBytes h (xprvToBytes xprv) encoding)
        (`hGetXPrv` [hrp])
    monitor (encodingexample encoding (xprvToBytes xprv) (xprvToBytes . snd <$> result))
    assert (result == Right (hrp, xprv))
  where
    hrp = [humanReadablePart|xprv|]
    encoding = EBech32 hrp

propGetPubRoundtrip
    :: XPub
    -> Property
propGetPubRoundtrip xpub = monadicIO $ do
    result <- run $ try $ withHandle
        (\h -> hPutBytes h (xpubToBytes xpub) encoding)
        (`hGetXPub` [hrp])
    monitor (encodingexample encoding (xpubToBytes xpub) (xpubToBytes . snd <$> result))
    assert (result == Right (hrp, xpub))
  where
    hrp = [humanReadablePart|xpub|]
    encoding = EBech32 hrp

propGetAnyRoundtrip
    :: Either XPub XPrv
    -> Property
propGetAnyRoundtrip xany = forAll genEncoding $ \encoding -> monadicIO $ do
    result <- run $ try $ withHandle
        (\h -> hPutBytes h (either xpubToBytes xprvToBytes xany) encoding)
        (`hGetXP__` hrp)
    monitor (encodingexample encoding
        (either xpubToBytes xprvToBytes xany)
        (either (xpubToBytes . snd) (xprvToBytes . snd) <$> result))
    assert ((bimap snd snd <$> result) == Right xany)
    assert ((either fst fst <$> result) `elem` (Right <$> hrp))
  where
    hrp = [ [humanReadablePart|xpub|], [humanReadablePart|xprv|] ]
    genEncoding = elements (EBech32 <$> hrp)

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

--
-- Helpers
--

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

encodingexample
    :: Encoding
    -> ByteString
    -> Either IOException ByteString
    -> Property
    -> Property
encodingexample encoding sent rcvd prop_ = prop_
    & counterexample (unlines
        [ "rcvd:"
        , show $ encode encoding <$> rcvd
        ])
    & counterexample (unlines
        [ "sent:"
        , show $ encode encoding sent
        ])

withHandle
    :: (Handle -> IO ())
        -- ^ Write operation
    -> (Handle -> IO a)
        -- ^ Read Operation
    -> IO a
withHandle writeOp readOp = withSystemTempFile "System.IO.Extra" $ \file h -> do
    hSetBuffering h NoBuffering
    writeOp h *> hClose h
    withFile file ReadMode readOp

-- | Read an octet-array passphrase from a temporary file.
readOctetsFromFile :: String -> IO ByteString
readOctetsFromFile input = withSystemTempFile "System.IO.Extra" $ \path h -> do
    B8.hPutStr h (bytes input)
    hClose h
    hGetPassphraseBytes (stdin, stderr) Explicit (FromFile path) "" Octets

bytes :: String -> ByteString
bytes = T.encodeUtf8 . T.pack

unbytes :: ByteString -> String
unbytes = T.unpack . T.decodeUtf8
