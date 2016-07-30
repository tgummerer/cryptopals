module UnlimitedBits
  ( fromHex
  , hexXor
  , toHex
  , toBase64
  , toAsciiString
  , fromAsciiString
  , xorWord
  , xor
  , nrSetBits
  ) where

import Data.Char
import Data.Word
import qualified Data.Bits as B
import Unsafe.Coerce
import Numeric

codes :: String
codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

xor :: [Word8] -> [Word8] -> [Word8]
xor = zipWith B.xor

hexXor :: String -> String -> String
hexXor xs ys = toHex $ xor (fromHex xs) (fromHex ys)

toHex :: [Word8] -> String
toHex = foldr z [] . map fromIntegral
  where
    z x rest = intToDigit (x `B.shiftR` 4):intToDigit (x B..&. 0x0f):rest

fromHex :: String -> [Word8]
fromHex [] = []
fromHex ('\n':[]) = []
fromHex (_:[]) = error "Invalid input length"
fromHex (x:y:xs) = ((fromIntegral (digitToInt x)) `B.shiftL` 4 + (fromIntegral (digitToInt y))):fromHex(xs)


toBase64' :: [Word8] -> String
toBase64' xs =
  codes !! (fromIntegral ((B.shiftR a 2) B..&. 0x3f)):
  codes !! (fromIntegral (((B.shiftL a 4) B..|. (B.shiftR b 4)) B..&. 0x3f)):
  (if length xs < 2 then '=' else codes !! (fromIntegral (((B.shiftL b 2) B..|. (B.shiftR c 6)) B..&. 0x3f))):
  (if length xs < 3 then '=' else codes !! (fromIntegral (c B..&. 0x3f))):[]
  where
    a = xs !! 0
    b = if length xs < 2 then 0 else xs !! 1
    c = if length xs < 3 then 0 else xs !! 2

toBase64 :: [Word8] -> String
toBase64 [] = []
toBase64 xs = toBase64' (take 3 xs) ++ toBase64 (drop 3 xs)

toAsciiString :: [Word8] -> String
toAsciiString = map (chr . fromIntegral)

fromAsciiString :: String -> [Word8]
fromAsciiString = map (fromIntegral . ord)

xorWord :: [Word8] -> [Word8] -> [Word8]
xorWord xs mask = zipWith (B.xor) xs (concat $ repeat mask)

nrSetBits :: [Word8] -> Int
nrSetBits = foldr (\x acc -> acc + B.popCount x) 0
