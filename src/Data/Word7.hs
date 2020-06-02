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

      -- * Helpers
    , limit
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
-- > toWord7 1
-- > Word7 1
-- > toWord7 127
-- > Word7 127
-- > toWord7 128
-- > Word7 0
toWord7 :: Word8 -> Word7
toWord7 x = Word7 (x .&. 0x7F)

toWord8 :: Word7 -> Word8
toWord8 (Word7 x) = x

-- > toWord7s 1
-- > [Word7 1]
-- > toWord7s 128
-- > [Word7 1,Word7 0]
-- > toWord7s 19099
-- > [Word7 1,Word7 21,Word7 27]
toWord7s :: Natural -> [Word7]
toWord7s = reverse . go
    where
        go n
            | n <= 0x7F = [Word7 . fromIntegral $ n]
            | otherwise = (toWord7 . fromIntegral) n : go (shiftR n 7)

toNatural :: [Word7] -> Natural
toNatural =
    fst .
    foldr (\(Word7 x) (res, pow) ->
               (res + (fromIntegral x)*(limit pow + 1), pow + 7)
          )
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
