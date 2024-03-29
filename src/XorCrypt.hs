module XorCrypt
  ( decryptXor
  , encryptXor
  , findXorEncrypted
  , breakRepeatingKeyXor
  ) where
  
import UnlimitedBits
import Data.Char
import Data.Ord
import Data.Word
import qualified Data.List as L
import qualified Data.Map as M

isValidChar :: Char -> Bool
isValidChar c = c == '\n' || ord c > 31 && ord c < 127

isScoringChar :: Char -> Bool
isScoringChar c = ord c >= 65 && ord c <= 90
                  || ord c >= 97 && ord c <= 122

findFrequencies :: String -> M.Map Char Double
findFrequencies xs = M.fromListWith (+) [(toLower c, 1) | c <- filter isScoringChar xs]

-- Algorithm based on http://crypto.stackexchange.com/questions/30209/developing-algorithm-for-detecting-plain-text-via-frequency-analysis
englishFreq :: [(Char, Double)]
englishFreq = zip ['a'..'z'] [ 0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015
                             , 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749
                             , 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758
                             , 0.00978, 0.02360, 0.00150, 0.01974, 0.00074 ]

chi2 :: String -> (String, Double)
chi2 st = (st, computeChi2 st)
  where
    computeChi2 st = if length (filter isValidChar st) == length st then
                       foldr f 0.0 englishFreq
                     else
                       1000.0
    f (char, freq) acc = acc + ((freq * fromIntegral len - M.findWithDefault 0.0 char frequencies) ** 2) / (freq * fromIntegral len)
    frequencies = findFrequencies st
    len = length $ filter isScoringChar st

findClosestEnglishMatch :: [String] -> (String, Double)
findClosestEnglishMatch = L.minimumBy (comparing snd) . map chi2

decryptXor :: String -> (String, Double)
decryptXor st = findClosestEnglishMatch $ map (toAsciiString . xorWord (fromHex st)) (subLists [0..255])
  where subLists = map (: [])

findXorEncrypted :: [String] -> [String]
findXorEncrypted = take 5 . map fst . L.sortBy (comparing snd) . map decryptXor

encryptXor :: String -> String -> String
encryptXor st = toHex . xorWord (fromAsciiString st) . fromAsciiString

findNormalizedDistance :: [Word8] -> Int -> Double
findNormalizedDistance [] _ = 0
findNormalizedDistance xs size = fromIntegral (hammingDistance (take size xs) (take size $ drop size xs))
  + findNormalizedDistance (drop size xs) size

findLikelyKeySize :: Int -> [Word8] -> Int
findLikelyKeySize max st = snd $ L.minimumBy (comparing fst) $ zip (map (findNormalizedDistance st) [2..max]) [2..max]

every :: Int -> [Word8] -> [Word8]
every n [] = []
every n (x:xs) = x:every n (drop (n - 1) xs)

breakRepeatingKeyXor :: [Word8] -> String
breakRepeatingKeyXor xs = L.concat $ L.transpose $ map (fst . decryptXor . toHex) $ L.take n $ map (every n) $ iterate tail xs
  where n = findLikelyKeySize 40 xs
