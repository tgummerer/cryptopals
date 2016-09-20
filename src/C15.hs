module C15
  ( hasPkcs7Padding
  ) where

import Data.Word
import UnlimitedBits

hasPkcs7Padding' :: [Word8] -> (String, Bool)
hasPkcs7Padding' st = if all (\x -> x == last st) (drop (length st - (fromIntegral $ last st)) st)
                      then (toAsciiString $ take (length st - (fromIntegral $ last st)) st, True)
                      else ("", False)

hasPkcs7Padding :: String -> (String, Bool)
hasPkcs7Padding = hasPkcs7Padding' . fromAsciiString
