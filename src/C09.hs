module C09
  ( pkcs7Padding
  ) where

import Data.Word

pkcs7Padding' :: Int -> [Word8]
pkcs7Padding' n = replicate n $ fromIntegral n

pkcs7Padding :: [Word8] -> Int -> [Word8]
pkcs7Padding xs padTo = xs ++ pkcs7Padding' (padTo - length xs `mod` padTo)
