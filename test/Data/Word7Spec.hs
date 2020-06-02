{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Data.Word7Spec
    ( spec
    ) where

import Prelude

import Data.Word
    ( Word8 )
import Data.Word7
    ( toNatural, toWord7, toWord7s, toWord8 )
import Numeric.Natural
    ( Natural )
import Test.Arbitrary
    ()
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, (===) )

spec :: Spec
spec = describe "Word7 roundtrips" $ do
    prop "toWord7 . toWord8 - Word8"
        prop_roundtripConversionWord8

    prop "toWord7s . toNatural - Natural"
        prop_roundtripConversionNatural

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
