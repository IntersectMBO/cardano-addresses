{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary
    (
    ) where

import Prelude

import Cardano.Mnemonic
    ( Entropy
    , EntropySize
    , Mnemonic
    , MnemonicException (..)
    , MnemonicWords
    , entropyToMnemonic
    , mkEntropy
    )
import Crypto.Encoding.BIP39
    ( ValidChecksumSize, ValidEntropySize, ValidMnemonicSentence )
import Data.Proxy
    ( Proxy (..) )
import GHC.TypeLits
    ( natVal )
import Test.QuickCheck
    ( Arbitrary, arbitrary, vector )

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

-- | The initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes. Note that this is good for testing or examples, but probably
-- not for generating truly random Mnemonic words.
--
-- See 'Crypto.Random.Entropy (getEntropy)'
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ natVal @n Proxy
            entropy =
                mkEntropy  @n . BA.convert . B8.pack <$> vector (size `quot` 8)
        in
            either (error . show . UnexpectedEntropyError) id <$> entropy

-- | Same remark from 'Arbitrary Entropy' applies here.
instance
    ( n ~ EntropySize mw
    , mw ~ MnemonicWords n
    , ValidChecksumSize n csz
    , ValidEntropySize n
    , ValidMnemonicSentence mw
    , Arbitrary (Entropy n)
    ) => Arbitrary (Mnemonic mw) where
    arbitrary =
        entropyToMnemonic <$> arbitrary @(Entropy n)
