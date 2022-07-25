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
    specFromKey defaultPhrase "1852H/1815H/0H/2/0"
        defaultAddrMainnet
        "addr1q9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qwvxwdrt\
        \70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qdqhgvu"

    specFromKey defaultPhrase "1852H/1815H/0H/2/0"
        defaultAddrTestnet
        "addr_test1qptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qwv\
        \xwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qwk2gqr"

    specFromKeyHash defaultPhrase "1852H/1815H/0H/2/0"
        defaultAddrMainnet
        "addr1q9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qwvxwdrt\
        \70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qdqhgvu"

    specFromKeyHash defaultPhrase "1852H/1815H/0H/2/0"
        defaultAddrTestnet
        "addr_test1qptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qwv\
        \xwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qwk2gqr"

    specFromScript
        defaultAddrMainnet
        "all [stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5]"
        "addr1y9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qfe5nnvf2a5vzmvdfhda0yw08qrj32kn4ytx2l7xpd08l7q0xlqfx"

    specMalformedAddress "💩"

    specMalformedAddress "\NUL"

    specMalformedAddress
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

    specMalformedAddress
        "DdzFFzCqrhsf6hiTYkK5gBAhVDwg3SiaHiEL9wZLYU3WqLUpx6DP\
        \5ZRJr4rtNRXbVNfk89FCHCDR365647os9AEJ8MKZNvG7UKTpythG"

    specInvalidAddress
        "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt\
        \70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"

    specMalformedXPub "💩"

    specInvalidXPub
        "stake_xvk1qfqcf4tp4ensj5qypqs640rt06pe5x7v2eul00c7rakzzvsakw3caelfuh6cg6nrkdv9y2ctkeu"

specFromKey :: [String] -> String -> String -> String -> SpecWith ()
specFromKey phrase path addr want = it ("delegation from key " <> want) $ do
    stakeKey <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
    out <- cli [ "address", "delegation", stakeKey ] addr
    out `shouldBe` want

specFromKeyHash :: [String] -> String -> String -> String -> SpecWith ()
specFromKeyHash phrase path addr want = it ("delegation from key " <> want) $ do
    stakeKeyHash <- cli [ "key", "from-recovery-phrase", "shelley" ] (unwords phrase)
       >>= cli [ "key", "child", path ]
       >>= cli [ "key", "public", "--with-chain-code" ]
       >>= cli [ "key", "hash" ]
    out <- cli [ "address", "delegation", stakeKeyHash ] addr
    out `shouldBe` want

specFromScript :: String -> String -> String -> SpecWith ()
specFromScript addr script want = it ("delegation from script " <> want) $ do
    scriptHash <- cli [ "script", "hash", script ] ""
    out <- cli [ "address", "delegation", scriptHash ] addr
    out `shouldBe` want

specMalformedAddress :: String -> SpecWith ()
specMalformedAddress addr = it ("malformed address " <> addr) $ do
    (out, err) <- cli [ "address", "delegation", defaultXPub ] addr
    out `shouldBe` ""
    err `shouldContain` "Bech32 error"

specInvalidAddress :: String -> SpecWith ()
specInvalidAddress addr = it ("invalid address " <> addr) $ do
    (out, err) <- cli [ "address", "delegation", defaultXPub ] addr
    out `shouldBe` ""
    err `shouldContain` "Only payment addresses can be extended"

specMalformedXPub :: String -> SpecWith ()
specMalformedXPub xpub = it ("malformed xpub " <> xpub) $ do
    (out, err) <- cli [ "address", "delegation", xpub ] defaultAddrMainnet
    out `shouldBe` ""
    err `shouldContain` "Couldn't parse delegation credentials."

specInvalidXPub :: String -> SpecWith ()
specInvalidXPub xpub = it ("invalid xpub " <> xpub) $ do
    (out, err) <- cli [ "address", "delegation", xpub ] defaultAddrMainnet
    out `shouldBe` ""
    err `shouldContain` "Couldn't parse delegation credentials."

defaultPhrase :: [String]
defaultPhrase =
    [ "art", "forum", "devote", "street", "sure"
    , "rather", "head", "chuckle", "guard", "poverty"
    , "release", "quote", "oak", "craft", "enemy"
    ]

defaultXPub :: String
defaultXPub =
    "stake_xvk1z0lq4d73l4xtk42s3364s2fpn4m5xtuacfkfj4dxxt9uhccvl\
    \g6pamdykgvcna3w4jf6zr3yqenuasug3gp22peqm6vduzrzw8uj6asu49xvf"

defaultAddrMainnet :: String
defaultAddrMainnet =
    "addr1v9therz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgx2curq"

defaultAddrTestnet :: String
defaultAddrTestnet =
    "addr_test1vptherz8fgux9ywdysrcpaclznyyvl23l2zfcery3f4m9qgazvqv9"
