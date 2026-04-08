{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Vendored minimal sized-list from @basement@ (BSD-3-Clause).
-- Only the subset used by BIP39 encoding.
module Cardano.Address.Crypto.ListN
    ( ListN
    , unListN
    , toListN
    , toListN_
    , map
    , mapM
    , foldl'
    ) where

import Prelude hiding
    ( foldl', map, mapM )

import Control.DeepSeq
    ( NFData )
import qualified Data.List as List
import Data.Proxy
    ( Proxy (..) )
import GHC.Stack
    ( HasCallStack )
import GHC.TypeLits
    ( KnownNat, Nat, natVal )

import qualified Control.Monad as M

-- | A list with a phantom type-level length.
newtype ListN (n :: Nat) a = ListN { unListN :: [a] }
    deriving stock (Show, Eq)
    deriving newtype NFData

-- | Construct a 'ListN' from a list, checking length at runtime.
toListN
    :: forall (n :: Nat) a
     . KnownNat n
    => [a] -> Maybe (ListN n a)
toListN l
    | expected == fromIntegral (length l) = Just (ListN l)
    | otherwise = Nothing
  where
    expected = natVal (Proxy @n)

-- | Construct a 'ListN' from a list, erroring if length mismatches.
toListN_
    :: forall (n :: Nat) a
     . (HasCallStack, KnownNat n)
    => [a] -> ListN n a
toListN_ l
    | expected == got = ListN l
    | otherwise =
        error
            ( "toListN_: expecting list of "
                <> show expected
                <> " elements, got "
                <> show got
                <> " elements"
            )
  where
    expected = fromIntegral (natVal (Proxy @n))
    got = length l

-- | Map a function over a sized list.
map :: (a -> b) -> ListN n a -> ListN n b
map f (ListN l) = ListN (List.map f l)

-- | Monadic map over a sized list.
mapM :: Monad m => (a -> m b) -> ListN n a -> m (ListN n b)
mapM f (ListN l) = ListN <$> M.mapM f l

-- | Strict left fold over a sized list.
foldl' :: (b -> a -> b) -> b -> ListN n a -> b
foldl' f acc (ListN l) = List.foldl' f acc l
