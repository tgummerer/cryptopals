module C13
  ( decrypt
  , createAdminProfile
  ) where

import AesCrypt (decryptAesEcb)
import C09 (pkcs7Padding)
import C11 (encryptAesEcb)
import Data.Word
import UnlimitedBits
import qualified Data.ByteString.Char8 as BC

passphrase :: String
passphrase = "YELLOW SUBMARINE"

email = "1@example.com"

decrypt :: [Word8] -> String
decrypt st = BC.unpack $ decryptAesEcb st passphrase

profileFor :: String -> [Word8]
profileFor mail = encryptAesEcb (pkcs7Padding (fromAsciiString $ "email=" ++ mail ++ "&uid=10&role=user") 16) passphrase

createAdminProfile :: [Word8]
createAdminProfile = take 32 (profileFor email) ++
                     take 16 (drop 16 (profileFor $ "0123456789admin" ++ toAsciiString(replicate 10 10)))
