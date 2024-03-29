import Test.Hspec
import AesCrypt
import UnlimitedBits
import XorCrypt
import Data.Char (isSpace)
import Data.Word
import qualified Data.ByteString.Char8 as BC
import C09
import C10
import C11
import qualified C12 as C12
import qualified C13 as C13
import qualified C14 as C14
import qualified C15 as C15

main :: IO ()
main = hspec $ do
  describe  "UnlimitedBits" $ do
    it "(solution 1.1) parses contents from hex" $
      toBase64 (fromHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    it "transforming from hex and back returns same result" $
      toHex (fromHex "686974207468652062756c6c277320657965") `shouldBe` "686974207468652062756c6c277320657965"
    it "transform hex encoding to ascii string" $
      toAsciiString (fromHex "48656c6c6f20576f726c64") `shouldBe` "Hello World"
    it "(solution 1.2) xor two hex numbers" $
      hexXor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965" `shouldBe` "746865206b696420646f6e277420706c6179"
    it "base64 to hex" $
      toHex (fromBase64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t") `shouldBe` "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    it "hamming distance" $
      hammingDistance (fromAsciiString "this is a test") (fromAsciiString "wokka wokka!!!") `shouldBe` 37

  describe "Decrypt" $ do
    it "(solution 1.3) decrypt hex encoded string" $
      fst (decryptXor "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") `shouldBe` "Cooking MC's like a pound of bacon"
    it "(solution 1.4) find xor encrypted string in bunch of hex strings" $ do
      contents <- readFile "testdata/1.4.txt"
      elem "Now that the party is jumping\n" (findXorEncrypted (lines contents)) `shouldBe` True
    it "(solution 1.5) encrypt string repeating-key xor" $
      encryptXor "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" "ICE" `shouldBe` "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    it "(solution 1.6) break repeating key xor" $ do
      contents <- readFile "testdata/1.6.txt"
      head (lines $ breakRepeatingKeyXor (fromBase64 $ filter (/= '\n') contents)) `shouldBe` "I'm back and I'm ringin' the bell "

  describe "Aes" $ do
    it "(solution 1.7) decrypt AES encoded string" $ do
      contents <- readFile "testdata/1.7.txt"
      head (lines $ BC.unpack $ decryptAesEcb (fromBase64 $ filter (/= '\n') contents) "YELLOW SUBMARINE") `shouldBe` "I'm back and I'm ringin' the bell "
    it "(solution 1.8) detect AES in ECB mode" $ do
      contents <- readFile "testdata/1.8.txt"
      take 10 (detectAesEcb $ lines contents) `shouldBe` "d880619740"

  describe "2.1" $ do
    it "(solution 2.09) add pkcs#7 padding" $
      toAsciiString (pkcs7Padding (fromAsciiString "YELLOW SUBMARINE") 20) `shouldBe` "YELLOW SUBMARINE\x04\x04\x04\x04"
    it "(solution 2.10) decrypt cbc" $ do
      contents <- readFile "testdata/2.2.txt"
      head (lines $ toAsciiString (decryptAesCbc [0 :: Word8,0..] (fromBase64 $ filter (/= '\n') contents) "YELLOW SUBMARINE")) `shouldBe` "I'm back and I'm ringin' the bell "
    it "(solution 2.11) encryption oracle" $ do
      (st, algo) <- encryptRandom $ fromAsciiString $ replicate 1024 '0'
      encryptionOracle st `shouldBe` algo
    it "(solution 2.12) decrypt ecb" $ do
      C12.findBlockLength `shouldBe` 16
      C12.functionUsingEcb `shouldBe` True
      C12.breakOne (replicate 15 0) 0 0 `shouldBe` 82
      C12.breakOne ((replicate 14 0) ++ [82]) 1 0 `shouldBe` 111
      take 16 C12.decryptAesEcb `shouldBe` "Rollin' in my 5."
    it "(solution 2.13) ecb cut-and-paste" $ do
      take 37 (C13.decrypt C13.createAdminProfile) `shouldBe` "email=1@example.com&uid=10&role=admin"
    it "(solution 2.14) decrypt ecb hard" $ do
      C14.findExtraLength `shouldBe` 11
      take 16 C14.decryptAesEcb `shouldBe` "Rollin' in my 5."
    it "(solution 2.15) pkcs7PaddingChecker" $ do
      C15.hasPkcs7Padding "ICE ICE BABY\x04\x04\x04\x04" `shouldBe` ("ICE ICE BABY", True)
      C15.hasPkcs7Padding "ICE ICE BABY\x01\x02\x03\x04" `shouldBe` ("", False)
