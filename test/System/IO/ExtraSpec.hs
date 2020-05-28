{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.IO.ExtraSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xprvToBytes, xpubToBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Exception
    ( IOException, SomeException, try )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Function
    ( (&) )
import Data.List
    ( nub )
import Options.Applicative.Encoding
    ( AbstractEncoding (..), Encoding )
import System.IO
    ( BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering, withFile )
import System.IO.Extra
    ( hGetBytes
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
    )
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
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import Test.Arbitrary
    ()

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "hGetBytes" $ do
        specDecodeString "MyString" bech32
        specDecodeString "MyString" base16
        specDecodeString "MyString" base58

        specInvalidEncodedString "bech321aaaaaaaaaaaaaaaa"
            (`shouldContain` "Bech32 error: Invalid character(s) in string")
        specInvalidEncodedString "bech321f4u4xarjd9hxwrhö"
            (`shouldContain` "Couldn't detect input encoding")
        specInvalidEncodedString "\NUL\NUL"
            (`shouldContain` "Couldn't detect input encoding")

    describe "hGetXPrv" $ do
        prop "roundtrip: hGetXPrv vs hPutBytes" $
            withMaxSuccess 1000 propGetPrvRoundtrip

    describe "hGetXPub" $ do
        prop "roundtrip: hGetXPub vs hPutBytes" $
            withMaxSuccess 1000 propGetPubRoundtrip

    describe "hGetXP__" $ do
        prop "roundtrip: hGetXP__ vs hPutBytes" $
            withMaxSuccess 1000 propGetAnyRoundtrip

    describe "markCharsRedAtIndices" $ do
        prop "generates strings of expected length"
            propMarkedStringsExpectedLength
        prop "all red chars correspond to indices"
            propRedCharsMatch

specDecodeString
    :: String
    -> (String -> ByteString)
    -> SpecWith ()
specDecodeString str encode = it (str <> " => " <> unbytes (encode str)) $ do
    str' <- withHandle
        (\h -> B8.hPutStr h (bech32 str))
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

bech32 :: String -> ByteString
bech32 =
    T.encodeUtf8 . Bech32.encodeLenient hrp . Bech32.dataPartFromBytes . bytes
  where
    hrp = [humanReadablePart|bech32|]

base16 :: String -> ByteString
base16 = convertToBase Base16 . bytes

base58 :: String -> ByteString
base58  = encodeBase58 bitcoinAlphabet . bytes

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

propGetPrvRoundtrip
    :: XPrv
    -> Encoding
    -> Property
propGetPrvRoundtrip xprv encoding = monadicIO $ do
    xprv' <- run $ try $ withHandle
        (\h -> hPutBytes h (xprvToBytes xprv) encoding)
        hGetXPrv
    monitor (encodingexample encoding (xprvToBytes xprv) (xprvToBytes <$> xprv'))
    assert (xprv' == Right xprv)

propGetPubRoundtrip
    :: XPub
    -> Encoding
    -> Property
propGetPubRoundtrip xpub encoding = monadicIO $ do
    xpub' <- run $ try $ withHandle
        (\h -> hPutBytes h (xpubToBytes xpub) encoding)
        hGetXPub
    monitor (encodingexample encoding (xpubToBytes xpub) (xpubToBytes <$> xpub'))
    assert (xpub' == Right xpub)

propGetAnyRoundtrip
    :: Either XPub XPrv
    -> Encoding
    -> Property
propGetAnyRoundtrip xany encoding = monadicIO $ do
    xany' <- run $ try $ withHandle
        (\h -> hPutBytes h (either xpubToBytes xprvToBytes xany) encoding)
        hGetXP__
    monitor (encodingexample encoding
        (either xpubToBytes xprvToBytes xany)
        (either xpubToBytes xprvToBytes <$> xany'))
    assert (xany' == Right xany)


--
-- Helpers
--

encodingexample
    :: Encoding
    -> ByteString
    -> Either IOException ByteString
    -> Property
    -> Property
encodingexample encoding sent rcvd prop_ = case encoding of
    EBase16 -> prop_
        & counterexample (unlines
            [ "rcvd:"
            , show $ convertToBase @_ @ByteString Base16 <$> rcvd
            ])
        & counterexample (unlines
            [ "sent:"
            , show $ convertToBase @_ @ByteString Base16 sent
            ])
    EBase58 -> prop_
        & counterexample (unlines
            [ "rcvd:"
            , show $ encodeBase58 bitcoinAlphabet <$> rcvd
            ])
        & counterexample (unlines
            [ "sent:"
            , show $ encodeBase58 bitcoinAlphabet sent
            ])
    EBech32 hrp -> prop_
        & counterexample (unlines
            [ "rcvd:"
            , show $ Bech32.encodeLenient hrp . Bech32.dataPartFromBytes <$> rcvd
            ])
        & counterexample (unlines
            [ "sent:"
            , show $ Bech32.encodeLenient hrp $ Bech32.dataPartFromBytes sent
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

bytes :: String -> ByteString
bytes = T.encodeUtf8 . T.pack

unbytes :: ByteString -> String
unbytes = T.unpack . T.decodeUtf8

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
