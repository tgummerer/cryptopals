module C12
  ( breakOne
  , findBlockLength
  , functionUsingEcb
  , decryptAesEcb
  ) where

import C09 (pkcs7Padding)
import C11 (isAesEcb, encryptAesEcb)
import Data.Word
import Debug.Trace
import UnlimitedBits

toFind :: [Word8]
toFind = fromBase64 "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"

encrypt :: [Word8] -> [Word8]
encrypt extraSt = encryptAesEcb (pkcs7Padding (extraSt ++ toFind) 16) "YELLOW SUBMARINE"

findBlockLength' :: [Word8] -> Int -> Int
findBlockLength' extra lastLength = if (length $ encrypt extra) > lastLength
                                    then (length $ encrypt extra) - lastLength
                                    else findBlockLength' (0:extra) (length $ encrypt extra)

findBlockLength :: Int
findBlockLength = findBlockLength' [0] (length (encrypt []))

functionUsingEcb :: Bool
functionUsingEcb = isAesEcb $ encrypt $ replicate (2 * findBlockLength) 0

isAesEcb' :: Int -> [Word8] -> Bool
isAesEcb' skip bytes = isAesEcb (take 16 bytes ++ drop (16 + skip * 16) bytes)

breakOne' :: [Word8] -> Int -> Word8 -> Int -> Word8
breakOne' prefix it try skip = if isAesEcb' skip $ (encrypt (prefix ++ [try] ++ take (15 - it) prefix))
                       then try
                       else breakOne' prefix it (try + 1) skip

breakOne :: [Word8] -> Int -> Int -> Word8
breakOne prefix it skip = breakOne' prefix it 0 skip

breakBlock :: [Word8] -> Int -> Int -> [Word8]
breakBlock start it skip
  | it < 15 = breakBlock (tail start ++ [breakOne start it skip]) (it + 1) skip
  | otherwise = start ++ [breakOne start it skip]

decryptAesEcb' :: [Word8] -> Int -> [Word8]
decryptAesEcb' start 9 = []
decryptAesEcb' start it = start ++ decryptAesEcb' (breakBlock (tail start) 0 it) (it + 1)
  

decryptAesEcb :: String
decryptAesEcb = toAsciiString $ drop 16 $ decryptAesEcb' (replicate 16 0) 0
