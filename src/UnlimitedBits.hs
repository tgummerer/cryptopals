module UnlimitedBits
  ( fromHex
  ) where

import Data.Word
import qualified Data.Bits as B
import Unsafe.Coerce
import Numeric

data Bits = Bits [Word8] Integer deriving (Show)

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
