import Test.Hspec
import UnlimitedBits


main :: IO ()
main = hspec $ do
  describe  "UnlimitedBits" $ do
    it "parses contents from hex" $ do
      (toBase64 $ fromHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
