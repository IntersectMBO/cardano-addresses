{-# LANGUAGE FlexibleContexts #-}

module Command.Address.PointerSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "address", "pointer" ] $ do
    specShelley (1,2,3) defaultAddr
        "addr1gpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5egpqgpsjej5ck"

    specShelley (24157,177,42) defaultAddr
        "addr1gpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5evph3wczvf2jyfghp"

    specMalformed ("💩","💩","💩") defaultAddr

    specInvalidAddress
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

    specInvalidAddress
        "DdzFFzCqrhsf6hiTYkK5gBAhVDwg3SiaHiEL9wZLYU3WqLUpx6DP\
        \5ZRJr4rtNRXbVNfk89FCHCDR365647os9AEJ8MKZNvG7UKTpythG"

    specInvalidAddress
        "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt\
        \70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"


specShelley :: (Int, Int, Int) -> String -> String -> SpecWith ()
specShelley (a,b,c) addr want = it ("golden shelley (pointer) " <> show (a,b,c)) $ do
    out <- cli [ "address", "pointer", show a, show b, show c ] addr
    out `shouldBe` want

specMalformed :: (String, String, String) -> String -> SpecWith ()
specMalformed (a,b,c) addr = it ("malformed pointer " <> unwords [a,b,c]) $ do
    (out, err) <- cli [ "address", "pointer", a, b, c ] addr
    out `shouldBe` ""
    err `shouldContain` "cannot parse value"
    err `shouldContain` "Usage"

specInvalidAddress :: String -> SpecWith ()
specInvalidAddress addr = it ("invalid address " <> addr) $ do
    (out, err) <- cli [ "address", "pointer", "0", "0", "0" ] addr
    out `shouldBe` ""
    err `shouldContain` "Only payment addresses can be extended"

defaultAddr :: String
defaultAddr =
    "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"
