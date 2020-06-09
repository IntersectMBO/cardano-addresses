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

      -- * Encode / Decode
    , putVariableLengthNat
    , getVariableLengthNat
    ) where

import Prelude

import Data.Binary.Get
    ( Get, getWord8 )
import Data.Binary.Put
    ( Put, putWord8 )
import Data.Bits
    ( shiftL, shiftR, (.&.), (.|.) )
import Data.List
    ( foldl' )
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

word7sToNat :: [Word7] -> Natural
word7sToNat = foldl' f 0
  where
    f n (Word7 r) = shiftL n 7 .|. (fromIntegral r)

toNatural :: [Word7] -> Natural
toNatural =
    fst .
    foldr (\(Word7 x) (res, pow) ->
               (res + (fromIntegral x)*(limit pow + 1), pow + 7)
          )
    (0,0)
  where
    limit :: Int -> Natural
    limit pow = 2 ^ pow - 1

--
-- Decoding
--
putVariableLengthNat :: Natural -> Put
putVariableLengthNat = putWord7s . toWord7s
  where
    putWord7s :: [Word7] -> Put
    putWord7s [] = pure ()
    putWord7s [Word7 x] = putWord8 x
    putWord7s (Word7 x : xs) = putWord8 (x .|. 0x80) >> putWord7s xs

getVariableLengthNat :: Get Natural
getVariableLengthNat = word7sToNat <$> getWord7s
  where
    getWord7s :: Get [Word7]
    getWord7s = do
      next <- getWord8
      case next .&. 0x80 of
        0x80 -> (:) (toWord7 next) <$> getWord7s
        _ -> pure [Word7 next]
