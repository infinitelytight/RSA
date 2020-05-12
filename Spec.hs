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
