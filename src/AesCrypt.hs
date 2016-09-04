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
import qualified Data.Set as Set
import UnlimitedBits (fromHex)

decryptAesEcb :: [Word8] -> String -> B.ByteString
decryptAesEcb st key = decryptECB (initAES $ BC.pack key) (B.pack st)

pairs :: [Word8] -> [(Word8, Word8)]
pairs = zip <*> tail

matches16bits :: Word8 -> Word8 -> [Word8] -> Integer
matches16bits _ _ [] = 0
matches16bits a b (x:y:xs) = if a == x && b == y then matches16bits a b xs + 1 else matches16bits a b xs

findBlocks :: [Word8] -> Set.Set B.ByteString
findBlocks [] = Set.empty
findBlocks xs = Set.insert (B.pack $ take 16 xs) $ findBlocks (drop 16 xs)

isAesEcb :: [Word8] -> Bool
isAesEcb xs = if (fromIntegral $ Set.size (findBlocks xs)) < (fromIntegral (fromIntegral (length xs)) / 16)
              then True
              else False

numSameBlocks :: [Word8] -> Int
numSameBlocks xs = (length xs) `div` 16 - Set.size (findBlocks xs)

detectAesEcb :: [String] -> String
detectAesEcb xs = snd $ maximumBy (comparing fst) (zip (map (numSameBlocks . fromHex) xs) xs)
