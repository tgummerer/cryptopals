import UnlimitedBits as U
import Numeric

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ toBase64  $ fromHex contents
