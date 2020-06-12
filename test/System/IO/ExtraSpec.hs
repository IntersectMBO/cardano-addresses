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
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import System.IO
    ( BufferMode (..), Handle, IOMode (..), hClose, hSetBuffering, withFile )
import System.IO.Extra
    ( hGetBytes, hGetXP__, hGetXPrv, hGetXPub, hPutBytes )
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
    ( Property, counterexample, withMaxSuccess )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import Test.Arbitrary
    ()

import qualified Data.ByteString.Char8 as B8
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

    describe "hGetXPrv" $ do
        prop "roundtrip: hGetXPrv vs hPutBytes" $
            withMaxSuccess 1000 propGetPrvRoundtrip

    describe "hGetXPub" $ do
        prop "roundtrip: hGetXPub vs hPutBytes" $
            withMaxSuccess 1000 propGetPubRoundtrip

    describe "hGetXP__" $ do
        prop "roundtrip: hGetXP__ vs hPutBytes" $
            withMaxSuccess 1000 propGetAnyRoundtrip

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

bytes :: String -> ByteString
bytes = T.encodeUtf8 . T.pack

unbytes :: ByteString -> String
unbytes = T.unpack . T.decodeUtf8
