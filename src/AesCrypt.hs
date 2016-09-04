module AesCrypt
  ( decryptAesEcb
  , detectAesEcb
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Cipher.AES
import Data.Word
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import UnlimitedBits (fromHex)

decryptAesEcb :: [Word8] -> String -> B.ByteString
decryptAesEcb st key = decryptECB (initAES $ BC.pack key) (B.pack st)

pairs :: [Word8] -> [(Word8, Word8)]
pairs = zip <*> tail

matches16bits :: Word8 -> Word8 -> [Word8] -> Integer
matches16bits _ _ [] = 0
matches16bits a b (x:y:xs) = if a == x && b == y then matches16bits a b xs + 1 else matches16bits a b xs

numSameBits :: [Word8] -> Integer
numSameBits xs = foldr (\(x,y) acc -> acc + matches16bits x y xs) 0 (pairs xs)

detectAesEcb :: [String] -> String
detectAesEcb xs = snd $ maximumBy (comparing fst) (zip (map (numSameBits . fromHex) xs) xs)
