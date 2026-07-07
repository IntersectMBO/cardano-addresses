module Options.Applicative.DerivationSpec
    ( spec
    ) where

import Prelude

import Control.Arrow
    ( left )
import Data.List
    ( isInfixOf )
import Options.Applicative.Derivation
    ( DerivationIndex
    , DerivationPath
    , derivationIndexFromString
    , derivationIndexToString
    , derivationPathFromString
    , derivationPathToString
    )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Property, (===) )

import Test.Arbitrary
    ()

spec :: Spec
spec = do
    describe "DerivationIndex" $ do
        specInvalidSoftIndex
        specInvalidHardIndex
        specNegativeSoftIndex
        specNegativeHardIndex
        prop "toString . fromString @ DerivationIndex"
            prop_roundtripStringDerivationIndex

    describe "DerivationPath" $ do
        specEmptyPath
        prop "toString . fromString @ DerivationPath"
            prop_roundtripStringDerivationPath

specInvalidSoftIndex
    :: SpecWith ()
specInvalidSoftIndex = it "invalid soft derivation index" $ do
    left (isInfixOf "Unable to parse soft index") (derivationIndexFromString "💩")
        `shouldBe` (Left True)

specInvalidHardIndex
    :: SpecWith ()
specInvalidHardIndex = it "invalid hard derivation index" $ do
    left (isInfixOf "Unable to parse hardened index") (derivationIndexFromString "💩H")
        `shouldBe` (Left True)

specNegativeSoftIndex
    :: SpecWith ()
specNegativeSoftIndex = it "negative soft derivation index" $ do
    left (isInfixOf "too low") (derivationIndexFromString "-1")
        `shouldBe` (Left True)

specNegativeHardIndex
    :: SpecWith ()
specNegativeHardIndex = it "negative hard derivation index" $ do
    left (isInfixOf "too low") (derivationIndexFromString "-1H")
        `shouldBe` (Left True)


prop_roundtripStringDerivationIndex
    :: DerivationIndex
    -> Property
prop_roundtripStringDerivationIndex ix =
    derivationIndexFromString (derivationIndexToString ix) === pure ix

specEmptyPath
    :: SpecWith ()
specEmptyPath = it "empty derivation path" $ do
    derivationPathFromString "" `shouldBe`
        (Left "An empty string is not a derivation index!")

prop_roundtripStringDerivationPath
    :: DerivationPath
    -> Property
prop_roundtripStringDerivationPath path =
    derivationPathFromString (derivationPathToString path) === pure path
