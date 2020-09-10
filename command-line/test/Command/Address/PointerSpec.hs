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
    specShelley (1,2,3) defaultAddrMainnet
        "addr1g9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgpqgpsyefcgl"

    specShelley (1,2,3) defaultAddrTestnet
        "addr_test1gptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgpqgpsa3x7je"

    specShelley (24157,177,42) defaultAddrMainnet
        "addr1g9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qvph3wczvf2sg4yzx"

    specShelley (24157,177,42) defaultAddrTestnet
        "addr_test1gptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qvph3wczvf2lxgdw5"

    specMalformed ("💩","💩","💩") defaultAddrMainnet

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

defaultAddrMainnet :: String
defaultAddrMainnet =
    "addr1v9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgx2curq"

defaultAddrTestnet :: String
defaultAddrTestnet =
    "addr_test1vptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgazvqv9"
