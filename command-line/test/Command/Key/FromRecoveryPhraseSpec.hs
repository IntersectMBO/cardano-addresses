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

    specInvalidStyle "patate" defaultPhrase
    specInvalidStyle "💩" defaultPhrase

    specInvalidPhrase "shelley" invalidPhrase

specGolden :: String -> [String] -> String -> SpecWith ()
specGolden style phrase want = it ("golden " <> style) $ do
    out <-  cli [ "key", "from-recovery-phrase",  style ] (unwords phrase)
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

invalidPhrase :: [String]
invalidPhrase =
    replicate 9 "¯\\_(ツ)_/¯"
