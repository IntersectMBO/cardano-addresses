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
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..), encode )
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

import qualified Cardano.Codec.Bech32.Prefixes as CIP5
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = describeCmd [ "key", "inspect" ] $ do
    specInspectPrivate CIP5.root_xsk
    specInspectPrivate CIP5.acct_xsk
    specInspectPrivate CIP5.addr_xsk
    specInspectPrivate CIP5.stake_xsk
    specInspectPrivate CIP5.root_shared_xsk
    specInspectPrivate CIP5.acct_shared_xsk
    specInspectPrivate CIP5.addr_shared_xsk
    specInspectPrivate CIP5.stake_shared_xsk
    specInspectPrivate CIP5.policy_xsk
    specInspectPrivate CIP5.drep_xsk
    specInspectPrivate CIP5.cc_cold_xsk
    specInspectPrivate CIP5.cc_hot_xsk

    specInspectPublic CIP5.root_xvk
    specInspectPublic CIP5.acct_xvk
    specInspectPublic CIP5.addr_xvk
    specInspectPublic CIP5.stake_xvk
    specInspectPublic CIP5.policy_xvk
    specInspectPublic CIP5.root_shared_xvk
    specInspectPublic CIP5.acct_shared_xvk
    specInspectPublic CIP5.addr_shared_xvk
    specInspectPublic CIP5.stake_shared_xvk
    specInspectPublic CIP5.drep_xvk
    specInspectPublic CIP5.cc_cold_xvk
    specInspectPublic CIP5.cc_hot_xvk

specInspectPrivate :: HumanReadablePart -> SpecWith ()
specInspectPrivate hrp = it "can inspect private key" $ do
    xprv <- generate arbitrary
    let cc  = base16 $ xprvChainCode xprv
    let prv = base16 $ xprvPrivateKey xprv
    out <- cli [ "key", "inspect" ] (bech32 hrp $ xprvToBytes xprv)
    out `shouldContain` "private"
    out `shouldContain` cc
    out `shouldContain` prv

specInspectPublic :: HumanReadablePart -> SpecWith ()
specInspectPublic hrp = it "can inspect public key" $ do
    xpub <- generate arbitrary
    let cc  = base16 $ xpubChainCode xpub
    let pub = base16 $ xpubPublicKey xpub
    out <- cli [ "key", "inspect" ] (bech32 hrp $ xpubToBytes xpub)
    out `shouldContain` "public"
    out `shouldContain` cc
    out `shouldContain` pub

base16 :: ByteString -> String
base16 = T.unpack . T.decodeUtf8 . encode EBase16

bech32 :: HumanReadablePart -> ByteString -> String
bech32 hrp = T.unpack . T.decodeUtf8 . encode (EBech32 hrp)
