module UnlimitedBits
  ( fromHex
  , hexXor
  , toHex
  , toBase64
  , toAsciiString
  , fromAsciiString
  , extractBits
  , xorWord
  , xor
  , nrSetBits
  ) where

import Data.Char
import Data.Word
import qualified Data.Bits as B
import Unsafe.Coerce
import Numeric

data Bits = Bits [Word8] Int deriving (Show)

codes :: String
codes = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

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
toHex (Bits xs _) = foldr z [] $ map fromIntegral xs
  where
    z x rest = intToDigit (x `B.shiftR` 4):intToDigit (x B..&. 0x0f):rest

fromHex :: String -> Bits
fromHex xs = Bits (arrFromHex xs) 0

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

toBase64'' :: [Word8] -> String
toBase64'' [] = []
toBase64'' xs = toBase64' (take 3 xs) ++ toBase64'' (drop 3 xs)

toBase64 :: Bits -> String
toBase64 (Bits xs _) = toBase64'' xs

toAsciiString :: Bits -> String
toAsciiString (Bits xs _) = map (chr . fromIntegral) xs

fromAsciiString :: String -> Bits
fromAsciiString xs = Bits (map (fromIntegral . ord) xs) 0

extractBits :: Bits -> [Word8]
extractBits (Bits xs _) = xs

xorWord :: Bits -> [Word8] -> Bits
xorWord (Bits xs _) mask = Bits (zipWith (B.xor) xs (concat $ repeat mask)) 0

nrSetBits :: Bits -> Int
nrSetBits (Bits xs _) = foldr (\x acc -> acc + B.popCount x) 0 xs
