module C22
  ( decryptAesCbc
  ) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Cipher.AES
import Data.Word

xor :: [Word8] -> [Word8] -> [Word8]
xor = zipWith Bits.xor

decryptAesEcb :: [Word8] -> String -> [Word8]
decryptAesEcb st key = B.unpack $ decryptECB (initAES $ BC.pack key) (B.pack st)

decryptAesCbc :: [Word8] -> [Word8] -> String -> [Word8]
decryptAesCbc _ [] _ = []
decryptAesCbc iv st key = xor (decryptAesEcb (take kl st) key) iv ++ decryptAesCbc (take kl st) (drop kl st) key
  where kl = length key
