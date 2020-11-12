{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Arbitrary () where

import Prelude

import Cardano.Address
    ( ChainPointer (..) )
import Cardano.Address.Derivation
    ( XPrv, XPub, generate, generateNew, toXPub, xprvToBytes )
import Codec.Binary.Bech32
    ( HumanReadablePart )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Codec.Binary.Encoding
    ( AbstractEncoding (..) )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( on )
import Data.List
    ( intercalate )
import Data.Word
    ( Word64 )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Options.Applicative.Derivation
    ( DerivationIndex
    , DerivationPath
    , derivationIndexToString
    , derivationPathFromString
    , indexToInteger
    , mkDerivationIndex
    )
import Test.QuickCheck
    ( Arbitrary (..), choose, elements, oneof, vector )

import qualified Data.ByteString as BS

--
-- Arbitrary Instances
--

instance Arbitrary XPrv where
    arbitrary = oneof
        [ flip generateNew (mempty :: ByteString) . BS.pack <$> vector 16
        , generate . BS.pack <$> vector 32
        ]

instance Arbitrary XPub where
    arbitrary =
        toXPub <$> arbitrary

instance Arbitrary DerivationIndex where
    arbitrary = unsafeFromRight . mkDerivationIndex
        <$> choose (indexToInteger minBound, indexToInteger maxBound)

instance Arbitrary DerivationPath where
    arbitrary = do
        n <- choose (1, 10)
        ixs <- vector @DerivationIndex n
        pure $ unsafeFromRight $ derivationPathFromString $
            intercalate "/" (derivationIndexToString <$> ixs)

instance Arbitrary (AbstractEncoding HumanReadablePart) where
    arbitrary = elements
        [ EBase16
        , EBase58
        , EBech32 [humanReadablePart|bech32|]
        ]

instance Arbitrary Natural where
    arbitrary =
        fromIntegral <$> choose (1 :: Word64, 10000000000)

instance Arbitrary ChainPointer where
    arbitrary = do
        slot <- arbitrary
        ix1 <- fromIntegral <$> choose (1 :: Word64, 1000000)
        ix2 <- fromIntegral <$> choose (1 :: Word64, 1000000)
        pure $ ChainPointer slot ix1 ix2

--
-- Extra Instances
--

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . xprvToBytes

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    (==) = (==) `on` xprvToBytes

--
-- Useful functions
--

-- | Use the 'Right' of an Either
unsafeFromRight :: (HasCallStack, Show left) => Either left right -> right
unsafeFromRight = either (error . show) id
