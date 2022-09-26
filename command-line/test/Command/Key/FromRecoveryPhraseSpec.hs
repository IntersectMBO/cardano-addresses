{-# LANGUAGE FlexibleContexts #-}

module Command.Key.FromRecoveryPhraseSpec
    ( spec
    ) where

import Prelude

import System.IO.Temp
    ( withSystemTempFile, writeSystemTempFile )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe, shouldContain )
import Test.Utils
    ( cli, describeCmd )

import qualified Data.Text as T

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

    specGolden "shared" defaultPhrase
        "root_shared_xsk1qz497hekfxq0ftrzjh7sl0m9vseep44mrnmk2dkzawczwy7gh\
        \fgd3ypmaem5k7lcv782p4haa4kcwmdnks4776rkgrx9zn4h8am82dagca203x7fej\
        \p4x04ty47he9rztj2lp46fwyzz3ad2yszwadjfnvuedzh4"

    specGoldenWithMnemonicPassphrase "shelley" defaultPhrase "from-mnemonic"
        (unwords sndFactorPhrase) rootXPrvWithPassphrase

    specGoldenWithMnemonicPassphrase "shelley" defaultPhrase "from-hex"
        sndFactorPhraseHex rootXPrvWithPassphrase

    specGoldenWithMnemonicPassphrase "shelley" defaultPhrase "from-base64"
        sndFactorPhraseBase64 rootXPrvWithPassphrase

    specGoldenWithMnemonicPassphrase "shelley" defaultPhrase "from-octets"
        sndFactorPhraseOctets rootXPrvWithPassphrase

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

specGoldenWithMnemonicPassphrase
    :: String
    -> [String]
    -> String
    -> String
    -> String
    -> SpecWith ()
specGoldenWithMnemonicPassphrase style phrase method sndfactor want =
    it ("golden " <> style <> " from-file "<> method) $
    withSystemTempFile "passphrase" $ \f _ -> do
    let sep = T.pack "/"
    let tmpFile = T.unpack $ head $ reverse $ T.splitOn sep (T.pack f)
    f1 <- writeSystemTempFile tmpFile sndfactor
    out <-  cli [ "key", "from-recovery-phrase",  style, "--passphrase"
                , method, "--from-file", f1] (unwords phrase)
    out `shouldBe` want

defaultPhrase :: [String]
defaultPhrase =
    [ "message", "mask", "aunt", "wheel", "ten"
    , "maze", "between", "tomato", "slow", "analyst"
    , "ladder", "such", "report", "capital", "produce"
    ]

invalidPhrase :: [String]
invalidPhrase =
    replicate 9 "¯\\_(ツ)_/¯"

sndFactorPhrase :: [String]
sndFactorPhrase =
    [ "test", "child", "burst", "immense", "armed"
    , "parrot", "company", "walk", "dog"
    ]

--λ> let (Right m9) = mkSomeMnemonic @'[ 9, 12, 15 ] [ "test", "child", "burst", "immense", "armed", "parrot", "company", "walk", "dog" ]
--Right (SomeMnemonic (Mnemonic {mnemonicToEntropy = Entropy {entropyRaw = "\223\132\252{8\192\189@\203\167\180@", entropyChecksum = Checksum 4}, mnemonicToSentence = MnemonicSentence {mnemonicSentenceToListN = [WordIndex {unWordIndex = Offset 1788},WordIndex {unWordIndex = Offset 319},WordIndex {unWordIndex = Offset 246},WordIndex {unWordIndex = Offset 908},WordIndex {unWordIndex = Offset 94},WordIndex {unWordIndex = Offset 1283},WordIndex {unWordIndex = Offset 372},WordIndex {unWordIndex = Offset 1972},WordIndex {unWordIndex = Offset 516}]}}))
--λ> import qualified Data.ByteString as BS
--λ> import qualified Data.ByteArray as BA
--λ> let bytes = BA.convert $ someMnemonicToBytes m9 :: BS.ByteString
--λ> bytes
--"\223\132\252{8\192\189@\203\167\180@"
--λ> encode EBase16 bytes
--"df84fc7b38c0bd40cba7b440"
--λ> import qualified Data.Text.Encoding as T
--λ> import qualified Data.ByteArray.Encoding as BA
--λ> T.decodeUtf8 $ BA.convertToBase BA.Base64 bytes
--"34T8ezjAvUDLp7RA"
--λ> BS.unpack bytes
-- [223,132,252,123,56,192,189,64,203,167,180,64]

sndFactorPhraseHex :: String
sndFactorPhraseHex = "df84fc7b38c0bd40cba7b440"

sndFactorPhraseBase64 :: String
sndFactorPhraseBase64 = "34T8ezjAvUDLp7RA"

sndFactorPhraseOctets :: String
sndFactorPhraseOctets = "[223,132,252,123,56,192,189,64,203,167,180,64]"

rootXPrvWithPassphrase :: String
rootXPrvWithPassphrase =
    "root_xsk1xpk2wzz7xsyhfxxvwraxnq2sps45ygfayrmn8kxjep4gl9jxgfg3tffp7z7w7ltd0\
    \gw32wqhyk5c296u7m28l688n8n6v24hrp326kgzz6cgvkvvj2k0t34jkv6ze0d8vxxnavar4tz\
    \gl96th9qhfayllsl4qsqq"

rootXPrvSharedWithPassphrase :: String
rootXPrvSharedWithPassphrase =
    "root_shared_xsk1xpk2wzz7xsyhfxxvwraxnq2sps45ygfayrmn8kxjep4gl9jxgfg3tffp7z\
    \7w7ltd0gw32wqhyk5c296u7m28l688n8n6v24hrp326kgzz6cgvkvvj2k0t34jkv6ze0d8vxxn\
    \avar4tzgl96th9qhfayllsak74cf"
