module C16
  ( containsAdminKeyword
  , doBitFlippingAttack
  ) where

import AesCrypt (decryptAesEcb)
import C09 (pkcs7Padding)
import C10 (decryptAesCbc)
import C11 (encryptAesCbc)
import Data.Word
import qualified Data.List as L
import UnlimitedBits
import qualified Data.ByteString.Char8 as BC

passphrase :: String
passphrase = "YELLOW SUBMARINE"

suffix :: String
suffix = ";comment2=%20like%20a%20pound%20of%20bacon"

encryptStuff :: String -> [Word8]
encryptStuff userData = encryptAesCbc (replicate 16 0) (pkcs7Padding (fromAsciiString $ "comment1=cooking%20MCs;userdata=" ++ userData ++ suffix) 16) passphrase

decryptStuff :: [Word8] -> String
decryptStuff st = toAsciiString (decryptAesCbc (replicate 16 0) st passphrase)

containsAdminKeyword :: [Word8] -> Bool
containsAdminKeyword st = ";admin=true;" `L.isInfixOf` (decryptStuff st)

cipherText :: [Word8]
cipherText = encryptStuff (replicate 16 'a')

doBitFlippingAttack :: [Word8]
doBitFlippingAttack = take 32 cipherText ++
                      xor (take 16 $ drop 32 cipherText) (xor (fromAsciiString ";admin=true;1234") (fromAsciiString suffix)) ++
                      drop 48 cipherText
