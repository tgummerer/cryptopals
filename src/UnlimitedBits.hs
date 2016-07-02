module UnlimitedBits
  ( fromHex
  , toBase64
  ) where

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

hexval :: Char -> Word8
hexval x
  | x >= 'A' && x <= 'F' = (unsafeCoerce x) - 65 + 10
  | x >= 'a' && x <= 'f' = (unsafeCoerce x) - 97 + 10
  | x >= '0' && x <= '9' = (unsafeCoerce x) - 48
  | otherwise = error "invalid hex character"

arrFromHex :: String -> [Word8]
arrFromHex [] = []
arrFromHex ('\n':[]) = []
arrFromHex (_:[]) = error "Invalid input length"
arrFromHex (x:y:xs) = ((hexval x) `B.shiftL` 4 + hexval y):arrFromHex(xs)

fromHex :: String -> Bits
fromHex xs = Bits (arrFromHex xs) 0

toBase64 :: Bits -> String
toBase64 bits = if containsBits bits then
                  codes !! (fromIntegral $ B.shiftR (bits .&. 0xfc) 2):toBase64 (bits `shiftL` 6)
                else
                  []
