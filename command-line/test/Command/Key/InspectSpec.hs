{-# LANGUAGE FlexibleContexts #-}

module Command.Key.InspectSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xprvChainCode
    , xprvPrivateKey
    , xprvToBytes
    , xpubChainCode
    , xpubPublicKey
    , xpubToBytes
    )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Spec, SpecWith, it, shouldContain )
import Test.QuickCheck
    ( arbitrary, generate )
import Test.Utils
    ( cli, describeCmd )

import Test.Arbitrary
    ()

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describeCmd [ "key", "inspect" ] $ do
    specInspectPrivate
    specInspectPublic

specInspectPrivate :: SpecWith ()
specInspectPrivate = it "can inspect private key" $ do
    xprv <- generate arbitrary
    let cc  = base16 $ xprvChainCode xprv
    let prv = base16 $ xprvPrivateKey xprv
    out <- cli [ "key", "inspect" ] (base16 $ xprvToBytes xprv)
    out `shouldContain` "private"
    out `shouldContain` cc
    out `shouldContain` prv

specInspectPublic :: SpecWith ()
specInspectPublic = it "can inspect public key" $ do
    xpub <- generate arbitrary
    let cc  = base16 $ xpubChainCode xpub
    let pub = base16 $ xpubPublicKey xpub
    out <- cli [ "key", "inspect" ] (base16 $ xpubToBytes xpub)
    out `shouldContain` "public"
    out `shouldContain` cc
    out `shouldContain` pub

base16 :: ByteString -> String
base16 = T.unpack . T.decodeUtf8 . convertToBase Base16
