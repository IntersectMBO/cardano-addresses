{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Data.Word7
    (
      -- * Types
      Word7

      -- * Conversions
    , toWord7
    , toWord8
    , toWord7s
    , toNatural

      -- * Encode
    , putVariableLengthNat

    ) where

import Prelude

import Data.Binary.Put
    ( Put, putWord8 )
import Data.Bits
    ( shiftR, (.&.), (.|.) )
import Data.Word
    ( Word8 )
import Numeric.Natural
    ( Natural )


-- | A 'Word7' algebraic data-type.
-- @since 2.0.0
newtype Word7 = Word7 Word8
  deriving (Eq, Show)


--
-- Conversions
--
toWord7 :: Word8 -> Word7
toWord7 x = Word7 (x .&. 0x7F)

toWord8 :: Word7 -> Word8
toWord8 (Word7 x) = x

toWord7s :: Natural -> [Word7]
toWord7s = reverse . go
    where
        go n
            | n <= 0x7F = [Word7 . fromIntegral $ n]
            | otherwise = (toWord7 . fromIntegral) n : go (shiftR n 7)

toNatural :: [Word7] -> Natural
toNatural =
    fst .
    foldr (\(Word7 x) (res, index) -> (res + limit index + fromIntegral x, index +1))
    (0,0)

limit :: Int -> Natural
limit pow = 2 ^ pow - 1

--
-- Decoding
--
putVariableLengthNat :: Natural -> Put
putVariableLengthNat = putWord7s . toWord7s

putWord7s :: [Word7] -> Put
putWord7s [] = pure ()
putWord7s [Word7 x] = putWord8 x
putWord7s (Word7 x : xs) = putWord8 (x .|. 0x80) >> putWord7s xs
