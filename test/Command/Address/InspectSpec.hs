{-# LANGUAGE FlexibleContexts #-}

module Command.Address.InspectSpec
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "address", "inspect" ] $ do
    specInspectAddress ["Byron", "none"]
        "37btjrVyb4KEgoGCHJ7XFaJRLBRiVuvcrQWPpp4HeaxdTxhKwQjXHNKL4\
        \3NhXaQNa862BmxSFXZFKqPqbxRc3kCUeTRMwjJevFeCKokBG7A7num5Wh"

    specInspectAddress ["Icarus", "none"]
        "Ae2tdPwUPEYz6ExfbWubiXPB6daUuhJxikMEb4eXRp5oKZBKZwrbJ2k7EZe"

    specInspectAddress ["Jormungandr", "single"]
        "addr1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677z5t3m7d"

    specInspectAddress ["Jormungandr", "group"]
        "addr1s3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677r\
        \vjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4wkdnx06"

    specInspectAddress ["Shelley", "none"]
        "addr1vpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5eg0yu80w"

    specInspectAddress ["Shelley", "by value"]
        "addr1qdu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ew\
        \vxwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2q5ggg4z"

    specInspectAddress ["Shelley", "by pointer"]
        "addr1gw2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5ph3wczvf2x4v58t"

    specInspectMalformed
        "💩"

    specInspectInvalid
        "79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65"

    -- 28-byte long script hash
    specInspectInvalid
        "addr1y9xup4n8cyckl7zw2u33pcn9ake3xvzy3vmtw9u79rw5r8ky\
        \6pru7d6jgn4t89vy3n3d68sx5uej906uwpp83dtefn6qv4hscd"

specInspectAddress :: [String] -> String -> SpecWith ()
specInspectAddress mustHave addr = it addr $ do
    out <- cli [ "address", "inspect" ] addr
    out `shouldContain` "address_style"
    out `shouldContain` "stake_reference"
    out `shouldContain` "network_tag"
    forM_ mustHave (shouldContain out)

specInspectMalformed :: String -> SpecWith ()
specInspectMalformed str = it ("malformed: " <> str) $ do
    (out, err) <- cli [ "address", "inspect" ] str
    out `shouldBe` ""
    err `shouldContain` "Couldn't detect input encoding?"

specInspectInvalid :: String -> SpecWith ()
specInspectInvalid str = it ("invalid: " <> str) $ do
    (out, err) <- cli [ "address", "inspect" ] str
    out `shouldBe` ""
    err `shouldContain` "Unrecognized address on standard input"
