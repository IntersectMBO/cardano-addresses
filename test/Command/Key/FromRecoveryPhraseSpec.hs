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
    specGolden "byron" defaultPhrase "--base16"
        "609b6958eb363277b5dba70326b47d730bf3ec0a9119a000ca27c5c7939d3f51\
        \95169089ff22a393cea887f9c5a2d7f5d000882faf4cd5cf1925638bf0b41ccd\
        \793d7bc5037200087714d438b49f2952a2dc03e9f3bdf7f046be725e40e9cdb3"

    specGolden "icarus" defaultPhrase "--base16"
        "00aa5f5f364980f4ac6295fd0fbf65643390d6bb1cf76536c2ebb02713c8ba50\
        \d8903bee774b7bf8678ea0d6fded6d876db3b42bef687640cc514eb73f767537\
        \a8c754f89bc9cc83533eab257d7c94625c95f0d749710428f5aa2404eeb6499b"

    specGolden "jormungandr" defaultPhrase "--bech32"
        "xprv1qz497hekfxq0ftrzjh7sl0m9vseep44mrnmk2dkzawczwy7ghfgd3ypmaem\
        \5k7lcv782p4haa4kcwmdnks4776rkgrx9zn4h8am82dagca203x7fejp4x04ty47\
        \he9rztj2lp46fwyzz3ad2yszwadjfnv4h6y7z"

    specGolden "shelley" defaultPhrase "--bech32"
        "xprv1qz497hekfxq0ftrzjh7sl0m9vseep44mrnmk2dkzawczwy7ghfgd3ypmaem\
        \5k7lcv782p4haa4kcwmdnks4776rkgrx9zn4h8am82dagca203x7fejp4x04ty47\
        \he9rztj2lp46fwyzz3ad2yszwadjfnv4h6y7z"

    specInvalidStyle "patate" defaultPhrase
    specInvalidStyle "💩" defaultPhrase

    specInvalidPhrase "shelley" invalidPhrase

specGolden :: String -> [String] -> String -> String -> SpecWith ()
specGolden style phrase encoding want = it ("golden " <> style) $ do
    out <-  cli [ "key", "from-recovery-phrase", encoding, style ] (unwords phrase)
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
