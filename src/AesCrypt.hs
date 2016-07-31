module AesCrypt
  ( decryptAesEcb
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Cipher.AES
import Data.Word

decryptAesEcb :: [Word8] -> String -> B.ByteString
decryptAesEcb st key = decryptECB (initAES $ BC.pack key) (B.pack st)
