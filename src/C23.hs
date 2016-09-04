module C23
  ( encryptRandom
  , encryptionOracle
  ) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Set as Set
import Crypto.Cipher.AES
import Data.Word

data AlgorithmType = ECB | CBC deriving (Show, Eq)

xor :: [Word8] -> [Word8] -> [Word8]
xor = zipWith Bits.xor

encryptAesEcb :: [Word8] -> String -> [Word8]
encryptAesEcb st key = B.unpack $ encryptECB (initAES $ BC.pack key) (B.pack st)

encryptAesCbc :: [Word8] -> [Word8] -> String -> [Word8]
encryptAesCbc _ [] _ = []
encryptAesCbc iv st key = encrypted ++ encryptAesCbc encrypted (drop kl st) key
  where kl = length key
        encrypted = encryptAesEcb (xor (take kl st) iv) key

encryptRandom :: [Word8] -> IO ([Word8], AlgorithmType)
encryptRandom xs = return (encryptAesEcb xs "YELLOW SUBMARINE", ECB)

findBlocks :: [Word8] -> Set.Set B.ByteString
findBlocks [] = Set.empty
findBlocks xs = Set.insert (B.pack $ take 16 xs) $ findBlocks (drop 16 xs)

isAesEcb :: [Word8] -> Bool
isAesEcb xs = if (fromIntegral $ Set.size (findBlocks xs)) < (fromIntegral (fromIntegral (length xs)) / 16)
              then True
              else False
                

encryptionOracle :: [Word8] -> AlgorithmType
encryptionOracle xs
  | isAesEcb xs = ECB
  | otherwise =  CBC
