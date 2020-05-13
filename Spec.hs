import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import RSA

main :: IO ()
main = hspec $ do
  let str = "pwd"
      hex = bytes str
      bitLen = 256
      byteLen = bitLen `div` 16
      padByteLen = byteLen - (length hex)
      padded = pad hex bitLen

  describe "Encoding" $ do
    it "converts to bytes" $ do
      hex `shouldBe` ["70","77","64"]
    it "adds padding header" $ do
      head <$> padded `shouldReturn` "02"
    it "adds padding stop symbol" $ do
      head . drop (padByteLen - 1) <$> padded `shouldReturn` "ff"
    it "puts string at end of encoded message" $ do
      drop padByteLen <$> padded `shouldReturn` hex
    it "produces correct key size" $ do
      length <$> padded `shouldReturn` byteLen
    it "encodes key to integer" $ do
      -- This key is deterministic since it is too short to have padding
      encode str 32 `shouldReturn` bytesToInt ("02ff" ++ (concat hex))

  describe "Decoding" $ do
    it "decodes key" $ do
      let encoded = 12875495268 -- 02ff707764 in decimal
      decode encoded `shouldBe` str
    it "decodes key with padding" $ do
      let encoded = 11709360739743588 -- 0299999ff707764 in decimal
      decode encoded `shouldBe` str

  describe "Primality testing" $ do
    it "correctly tests large primes" $ do
      let p = 2147483647
      isProbablePrime p `shouldBe` True
      let p = 4547337172376300111955330758342147474062293202868155909489
      isProbablePrime p `shouldBe` True
    it "correctly tests large pseudoprimes" $ do
      let p = 4547337172376300111955330758342147474062293202868155909393
      isProbablePrime p `shouldBe` False
      let p = 47362882341088596725068562696893704769436677460225591859092704246296157080253
      isProbablePrime p `shouldBe` False
