module UnlimitedBits
  ( fromHex
  , hexXor
  , toHex
  , toBase64
  ) where

import Data.Char
import Data.Word
import qualified Data.Bits as B
import Unsafe.Coerce
import Numeric

data Bits = Bits [Word8] Int deriving (Show)

codes :: String
codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

containsBits :: Bits -> Bool
containsBits (Bits xs _) = not $ null xs

(.&.) :: Bits -> Word8 -> Word8
(Bits [] _) .&. _ = 0
(Bits (x:[]) off) .&. mask = (x `B.shiftL` off) B..&. mask
(Bits (x:y:_) off) .&. mask = val B..&.mask
  where
    val = (x `B.shiftL` off) + (y `B.shiftR` (8 - off))

shiftL :: Bits -> Int -> Bits
shiftL (Bits [] _) _ = (Bits [] 0)
shiftL (Bits xs off) by = (Bits (drop (quot (off + by) 8) xs) (mod (off + by) 8))

arrFromHex :: String -> [Word8]
arrFromHex [] = []
arrFromHex ('\n':[]) = []
arrFromHex (_:[]) = error "Invalid input length"
arrFromHex (x:y:xs) = ((fromIntegral (digitToInt x)) `B.shiftL` 4 + (fromIntegral (digitToInt y))):arrFromHex(xs)

xor :: Bits -> Bits -> Bits
xor (Bits a _) (Bits b _) = Bits (zipWith B.xor a b) 0

hexXor :: String -> String -> String
hexXor a b = toHex $ xor (fromHex a) (fromHex b)

toHex :: Bits -> String
toHex (Bits xs _) = foldr z  [] $ map fromIntegral xs
  where
    z x rest = intToDigit (x `B.shiftR` 4):intToDigit (x B..&. 0x0f):rest

fromHex :: String -> Bits
fromHex xs = Bits (arrFromHex xs) 0

toBase64 :: Bits -> String
toBase64 bits = if containsBits bits then
                  codes !! (fromIntegral $ B.shiftR (bits .&. 0xfc) 2):toBase64 (bits `shiftL` 6)
                else
                  []
