{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Data.Word7Spec
    ( spec
    ) where

import Prelude

import Data.Binary.Get
    ( runGet )
import Data.Binary.Put
    ( runPut )
import Data.Word
    ( Word8 )
import Data.Word7
    ( getVariableLengthNat
    , putVariableLengthNat
    , toNatural
    , toWord7
    , toWord7s
    , toWord8
    )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, choose, forAll, (===) )

import Test.Arbitrary
    ()

spec :: Spec
spec = do
    describe "Word7 roundtrips" $ do
        prop "toWord7  . toWord8   - Word8"   prop_roundtripConversionWord8
        prop "toWord7s . toNatural - Natural" prop_roundtripConversionNatural

    describe "encode / decode roundtrip" $ do
        prop "{get,put}VariableLengthNat" prop_roundtripVariableLengthNat

prop_roundtripConversionWord8
    :: Word8
    -> Property
prop_roundtripConversionWord8 number =
    (number `mod` 128 === toWord8 (toWord7 number))

prop_roundtripConversionNatural
    :: Natural
    -> Property
prop_roundtripConversionNatural number =
    (number === toNatural (toWord7s number))

prop_roundtripVariableLengthNat
    :: Property
prop_roundtripVariableLengthNat =
    forAll genNatural $ \n ->
        runGet getVariableLengthNat (runPut $ putVariableLengthNat n) === n
  where
    genNatural = fromIntegral @Integer <$> choose (0, 500)
