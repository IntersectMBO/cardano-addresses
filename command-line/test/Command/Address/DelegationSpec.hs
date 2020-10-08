{-# LANGUAGE FlexibleContexts #-}

module Command.Address.DelegationSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "address", "delegation" ] $ do
    specShelley defaultPhrase "1852H/1815H/0H/2/0"
        defaultAddrMainnet
        "addr1q9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qwvxwdrt\
        \70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qdqhgvu"

    specShelley defaultPhrase "1852H/1815H/0H/2/0"
        defaultAddrTestnet
        "addr_test1qptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qwv\
        \xwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qwk2gqr"

    specMalformedAddress "💩"

    specMalformedAddress "\NUL"

    specInvalidAddress
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

    specInvalidAddress
        "DdzFFzCqrhsf6hiTYkK5gBAhVDwg3SiaHiEL9wZLYU3WqLUpx6DP\
        \5ZRJr4rtNRXbVNfk89FCHCDR365647os9AEJ8MKZNvG7UKTpythG"

    specInvalidAddress
        "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt\
        \70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"

    specMalformedXPub "💩"

    specInvalidXPub "\NUL"

    specInvalidXPub
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

specShelley :: [String] -> String -> String -> String -> SpecWith ()
specShelley phrase path addr want = it ("golden shelley (delegation) " <> addr) $ do
    stakeKey <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public" ]
    out <- cli [ "address", "delegation", "--from-key", stakeKey ] addr
    out `shouldBe` want

specMalformedAddress :: String -> SpecWith ()
specMalformedAddress addr = it ("malformed address " <> addr) $ do
    (out, err) <- cli [ "address", "delegation", "--from-key",defaultXPub ] addr
    out `shouldBe` ""
    err `shouldContain` "Couldn't detect input encoding?"

specInvalidAddress :: String -> SpecWith ()
specInvalidAddress addr = it ("invalid address " <> addr) $ do
    (out, err) <- cli [ "address", "delegation", "--from-key", defaultXPub ] addr
    out `shouldBe` ""
    err `shouldContain` "Only payment addresses can be extended"

specMalformedXPub :: String -> SpecWith ()
specMalformedXPub xpub = it ("malformed xpub " <> xpub) $ do
    (out, err) <- cli [ "address", "delegation", "--from-key", xpub ] defaultAddrMainnet
    out `shouldBe` ""
    err `shouldContain` "Couldn't detect input encoding?"

specInvalidXPub :: String -> SpecWith ()
specInvalidXPub xpub = it ("invalid xpub " <> xpub) $ do
    (out, err) <- cli [ "address", "delegation", "--from-key", xpub ] defaultAddrMainnet
    out `shouldBe` ""
    err `shouldContain` "Failed to convert bytes into a valid extended public key"

defaultPhrase :: [String]
defaultPhrase =
    [ "art", "forum", "devote", "street", "sure"
    , "rather", "head", "chuckle", "guard", "poverty"
    , "release", "quote", "oak", "craft", "enemy"
    ]

defaultXPub :: String
defaultXPub =
    "xpub1z0lq4d73l4xtk42s3364s2fpn4m5xtuacfkfj4dxxt9uhccvlg6p\
    \amdykgvcna3w4jf6zr3yqenuasug3gp22peqm6vduzrzw8uj6asghxwup"

defaultAddrMainnet :: String
defaultAddrMainnet =
    "addr1v9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgx2curq"

defaultAddrTestnet :: String
defaultAddrTestnet =
    "addr_test1vptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgazvqv9"
