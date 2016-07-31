import Test.Hspec
import AesCrypt
import UnlimitedBits
import XorCrypt
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = hspec $ do
  describe  "UnlimitedBits" $ do
    it "parses contents from hex" $
      toBase64 (fromHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    it "transforming from hex and back returns same result" $
      toHex (fromHex "686974207468652062756c6c277320657965") `shouldBe` "686974207468652062756c6c277320657965"
    it "transform hex encoding to ascii string" $
      toAsciiString (fromHex "48656c6c6f20576f726c64") `shouldBe` "Hello World"
    it "xor two hex numbers" $
      hexXor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965" `shouldBe` "746865206b696420646f6e277420706c6179"
    it "base64 to hex" $
      toHex (fromBase64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t") `shouldBe` "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    it "hamming distance" $
      hammingDistance (fromAsciiString "this is a test") (fromAsciiString "wokka wokka!!!") `shouldBe` 37

  describe "Decrypt" $ do
    it "decrypt hex encoded string" $
      fst (decryptXor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") `shouldBe` "Cooking MC's like a pound of bacon"
    it "find xor encrypted string in bunch of hex strings" $ do
      contents <- readFile "testdata/1.4.txt"
      elem "Now that the party is jumping\n" (findXorEncrypted (lines contents)) `shouldBe` True
    it "encrypt string repeating-key xor" $
      encryptXor "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE" `shouldBe` "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    it "break repeating key xor" $ do
      contents <- readFile "testdata/1.6.txt"
      head (lines $ breakRepeatingKeyXor (fromBase64 $ filter (/= '\n') contents)) `shouldBe` "I'm back and I'm ringin' the bell "

  describe "Aes" $ do
    it "decrypt AES encoded string" $ do
      contents <- readFile "testdata/1.7.txt"
      head (lines $ BC.unpack $ decryptAesEcb (fromBase64 $ filter (/= '\n') contents) "YELLOW SUBMARINE") `shouldBe` "I'm back and I'm ringin' the bell "
