module XorCrypt
  ( decryptXor
  ) where
  
import UnlimitedBits
import Data.Char
import Data.Ord
import qualified Data.List as L
import qualified Data.Map as M

findFrequencies :: String -> M.Map Char Double
findFrequencies xs = M.fromListWith (+) [(toLower c, 1.0) | c <- filter isLetter xs]

-- Algorithm based on http://crypto.stackexchange.com/questions/30209/developing-algorithm-for-detecting-plain-text-via-frequency-analysis
englishFreq :: [(Char, Double)]
englishFreq = zip ['a'..'z'] [ 0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015
                             , 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749
                             , 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758
                             , 0.00978, 0.02360, 0.00150, 0.01974, 0.00074 ]

chi2 :: String -> (String, Double)
chi2 st = (st, computeChi2 st)
  where
    computeChi2 st = if (length $ filter (\x -> isAscii x && isLetter x) st) < 1 then
                       -- some high double number that won't be reached as chi
                       -- value by a normal sentence
                       10000000.0
                     else
                       foldr f 0.0 englishFreq
    f (char, freq) acc = acc + ((freq * (fromIntegral len) - M.findWithDefault 0.0 char frequencies) ** 2) / (freq * (fromIntegral len))
    frequencies = findFrequencies st
    len = length $ filter isLetter st

findClosestEnglishMatch :: [String] -> (String, Double)
findClosestEnglishMatch xs = head $ L.sortBy (comparing $ snd) (map chi2 xs)

decryptXor :: String -> (String, Double)
decryptXor st = findClosestEnglishMatch $ map (toAsciiString . xorWord (fromHex st)) [0..255]
