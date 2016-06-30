import UnlimitedBits as U
import Numeric

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ show  $ fromHex contents
