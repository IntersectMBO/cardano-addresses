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

    -- reward account: keyhash28
    specInspectAddress ["Shelley", "by value", "stake_key_hash"]
        "stake1upshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mqcjv83c"

    -- reward account: scripthash32
    specInspectAddress ["Shelley", "by value", "script_hash"]
        "stake17pshvetj09hxjcm9v9jxgunjv4ehxmr0d3hkcmmvdakx7mrgdp5xscfm7jc"

    -- cardano-cli generated --testnet-magic 42 addresses
    specInspectAddress ["Shelley", "by value", "stake_key_hash", "spending_key_hash"]
        "addr_test1qpwr8l57ceql23ylyprl6qgct239lxph8clwxy5w8r4qdz8ct9uut5a\
        \hmxqkgwy9ecn5carsv39frsgsq09u70wmqwhqjqcjqs"
    specInspectAddress ["Shelley", "by value", "stake_key_hash"]
        "stake_test1uru9j7w96wmanqty8zzuuf6vw3cxgj53cygq8j708hds8tsntl0j7"

    -- cardano-cli generated --mainnet addresses
    specInspectAddress ["Shelley", "by value", "stake_key_hash", "spending_key_hash"]
        "addr1q9777p2w2hqa3cl0ah97pdwyavjnpf0ex3muvqgttavjxhku2rp98h9drzkdf\
        \va8ea775jszmd799k59aknpvqyn6wwqwll7uw"
    specInspectAddress ["Shelley", "by value", "stake_key_hash"]
        "stake1u8w9psjnmjk33tx5kwnu7l02fgpdklzjm2z7mfskqzfa88qsjpk8l"

    specInspectMalformed
        "💩"

    specInspectInvalid "Wrong input size of 28"
        "79467c69a9ac66280174d09d62575ba955748b21dec3b483a9469a65"

    -- 28-byte long script hash
    specInspectInvalid "Unknown address type"
        "addr1y9xup4n8cyckl7zw2u33pcn9ake3xvzy3vmtw9u79rw5r8ky\
        \6pru7d6jgn4t89vy3n3d68sx5uej906uwpp83dtefn6qv4hscd"

specInspectAddress :: [String] -> String -> SpecWith ()
specInspectAddress mustHave addr = it addr $ do
    (out, err) <- cli [ "address", "inspect" ] addr
    err `shouldBe` ""
    out `shouldContain` "address_style"
    out `shouldContain` "stake_reference"
    out `shouldContain` "network_tag"
    forM_ mustHave (shouldContain out)

specInspectMalformed :: String -> SpecWith ()
specInspectMalformed str = it ("malformed: " <> str) $ do
    (out, err) <- cli [ "address", "inspect" ] str
    out `shouldBe` ""
    err `shouldContain` "Couldn't detect input encoding?"

specInspectInvalid :: String -> String -> SpecWith ()
specInspectInvalid errstr str = it ("invalid: " <> str) $ do
    (out, err) <- cli [ "address", "inspect" ] str
    out `shouldBe` ""
    err `shouldContain` errstr
