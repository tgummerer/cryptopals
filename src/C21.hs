module C21
  ( pkcs7Padding
  ) where

import Data.Word

pkcs7Padding' :: Int -> [Word8]
pkcs7Padding' n = replicate n $ fromIntegral n

pkcs7Padding :: [Word8] -> [Word8]
pkcs7Padding xs = xs ++ pkcs7Padding' (5 - length xs `mod` 5)
