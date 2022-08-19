{-# LANGUAGE FlexibleContexts #-}

module Command.Key.FromRecoveryPhraseSpec
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

spec :: Spec
spec = describeCmd [ "key", "from-recovery-phrase" ] $ do
    specGolden "byron" defaultPhrase
        "root_xsk1vzdkjk8txce80dwm5upjddrawv9l8mq2jyv6qqx2ylzu0yua8age295\
        \s38lj9gune65g07w95ttlt5qq3qh67nx4euvj2cut7z6pente84au2qmjqqy8w9x\
        \58z6f722j5twq860nhhmlq347wf0yp6wdkvq80h42"

    specGolden "icarus" defaultPhrase
        "root_xsk1qz497hekfxq0ftrzjh7sl0m9vseep44mrnmk2dkzawczwy7ghfgd3yp\
        \maem5k7lcv782p4haa4kcwmdnks4776rkgrx9zn4h8am82dagca203x7fejp4x04\
        \ty47he9rztj2lp46fwyzz3ad2yszwadjfnv76n80u"

    specGolden "shelley" defaultPhrase
        "root_xsk1qz497hekfxq0ftrzjh7sl0m9vseep44mrnmk2dkzawczwy7ghfgd3yp\
        \maem5k7lcv782p4haa4kcwmdnks4776rkgrx9zn4h8am82dagca203x7fejp4x04\
        \ty47he9rztj2lp46fwyzz3ad2yszwadjfnv76n80u"

    specGoldenWithPassphrase "icarus" defaultPhrase (unwords sndFactorPhrase)
        rootXPrvIcarusWithPassphrase

    specGoldenWithPassphrase "icarus" defaultPhrase sndFactorPhraseHex
        rootXPrvIcarusWithPassphrase

    specGoldenWithPassphrase "icarus" defaultPhrase sndFactorPhraseBase58
        rootXPrvIcarusWithPassphrase

    specGoldenWithPassphrase "shelley" defaultPhrase (unwords sndFactorPhrase)
        rootXPrvShelleyWithPassphrase

    specGoldenWithPassphrase "shelley" defaultPhrase sndFactorPhraseHex
        rootXPrvShelleyWithPassphrase

    specGoldenWithPassphrase "shelley" defaultPhrase sndFactorPhraseBase58
        rootXPrvShelleyWithPassphrase

    specInvalidStyle "patate" defaultPhrase
    specInvalidStyle "💩" defaultPhrase

    specInvalidPhrase "shelley" invalidPhrase

specGolden :: String -> [String] -> String -> SpecWith ()
specGolden style phrase want = it ("golden " <> style) $ do
    out <-  cli [ "key", "from-recovery-phrase",  style ] (unwords phrase)
    out `shouldBe` want

specGoldenWithPassphrase :: String -> [String] -> String -> String -> SpecWith ()
specGoldenWithPassphrase style phrase sndfactor want = it ("golden " <> style) $ do
    out <-  cli [ "key", "from-recovery-phrase",  style, "--passphrase", sndfactor ] (unwords phrase)
    out `shouldBe` want

specInvalidStyle :: String -> [String] -> SpecWith ()
specInvalidStyle style phrase = it ("invalid style " <> style) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", style ] (unwords phrase)
    out `shouldBe` ""
    err `shouldContain` "Unknown style; expecting one of"
    err `shouldContain` "Usage:"

specInvalidPhrase :: String -> [String] -> SpecWith ()
specInvalidPhrase style phrase = it ("invalid phrase " <> unwords phrase) $ do
    (out, err) <- cli [ "key", "from-recovery-phrase", style ] (unwords phrase)
    out `shouldBe` ""
    err `shouldContain` "Found an unknown word not present in the pre-defined dictionary."

defaultPhrase :: [String]
defaultPhrase =
    [ "message", "mask", "aunt", "wheel", "ten"
    , "maze", "between", "tomato", "slow", "analyst"
    , "ladder", "such", "report", "capital", "produce"
    ]

sndFactorPhrase :: [String]
sndFactorPhrase =
    [ "test", "child", "burst", "immense", "armed"
    , "parrot", "company", "walk", "dog"
    ]

--λ> mkSomeMnemonic @'[ 9, 12, 15 ] [ "test", "child", "burst", "immense", "armed", "parrot", "company", "walk", "dog" ]
--Right (SomeMnemonic (Mnemonic {mnemonicToEntropy = Entropy {entropyRaw = "\223\132\252{8\192\189@\203\167\180@", entropyChecksum = Checksum 4}, mnemonicToSentence = MnemonicSentence {mnemonicSentenceToListN = [WordIndex {unWordIndex = Offset 1788},WordIndex {unWordIndex = Offset 319},WordIndex {unWordIndex = Offset 246},WordIndex {unWordIndex = Offset 908},WordIndex {unWordIndex = Offset 94},WordIndex {unWordIndex = Offset 1283},WordIndex {unWordIndex = Offset 372},WordIndex {unWordIndex = Offset 1972},WordIndex {unWordIndex = Offset 516}]}}))
--λ> let bs = BA.convert $ someMnemonicToBytes m9 :: ByteString
--λ> bs
--"\223\132\252{8\192\189@\203\167\180@"
--λ> encode EBase16 bs
--"df84fc7b38c0bd40cba7b440"
--λ> encode EBase58 bs
--"5DeNQFNAN1gAArMyh"

sndFactorPhraseHex :: String
sndFactorPhraseHex = "df84fc7b38c0bd40cba7b440"

sndFactorPhraseBase58 :: String
sndFactorPhraseBase58 = "5DeNQFNAN1gAArMyh"

rootXPrvIcarusWithPassphrase :: String
rootXPrvIcarusWithPassphrase =
    "root_xsk1xpk2wzz7xsyhfxxvwraxnq2sps45ygfayrmn8kxjep4gl9jxgfg3tffp7z7w7ltd0\
    \gw32wqhyk5c296u7m28l688n8n6v24hrp326kgzz6cgvkvvj2k0t34jkv6ze0d8vxxnavar4tz\
    \gl96th9qhfayllsl4qsqq"

rootXPrvShelleyWithPassphrase :: String
rootXPrvShelleyWithPassphrase =
    "root_xsk1xpk2wzz7xsyhfxxvwraxnq2sps45ygfayrmn8kxjep4gl9jxgfg3tffp7z7w7ltd0\
    \gw32wqhyk5c296u7m28l688n8n6v24hrp326kgzz6cgvkvvj2k0t34jkv6ze0d8vxxnavar4tz\
    \gl96th9qhfayllsl4qsqq"

invalidPhrase :: [String]
invalidPhrase =
    replicate 9 "¯\\_(ツ)_/¯"
