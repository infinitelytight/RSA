import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Encoding
import Keys
import Math
import RSA

main :: IO ()
main = hspec $ do
  let keySize = 256
      keySizeBytes = keySize `div` 16

      plaintext = "pwd"
      hex = bytes plaintext
      padSizeBytes = keySizeBytes - (length hex) -- Number of padding bytes
      padded = pad hex keySize

  describe "Encoding" $ do
    it "converts string to hex-notation bytes" $ do
      hex `shouldBe` ["70","77","64"]
    it "adds padding header" $ do
      head <$> padded `shouldReturn` "02"
    it "adds padding stop symbol" $ do
      head . drop (padSizeBytes - 1) <$> padded `shouldReturn` "ff"
    it "puts plaintext string at end of encoded message" $ do
      drop padSizeBytes <$> padded `shouldReturn` hex
    it "produces correct ciphertext size" $ do
      length <$> padded `shouldReturn` keySizeBytes
    it "encodes key to integer" $ do
      -- This key is deterministic since it is too short to have padding
      encode plaintext 32 `shouldReturn` bytesToInt ("02ff" ++ (concat hex))

  describe "Decoding" $ do
    it "decodes message" $ do
      -- "key" in hex (without random padding) = 02ff707764 => in decimal:
      let encoded = 12875495268
      decode encoded `shouldBe` plaintext
    it "decodes message with padding" $ do
      -- "key" in hex (with 99999 as padding) = 0299999ff707764 => in decimal:
      let encoded = 11709360739743588
      decode encoded `shouldBe` plaintext

  describe "Primality testing" $ do
    it "correctly tests large primes" $ do
      let p = 2147483647
      isProbablePrime p `shouldReturn` True
      let p = 4547337172376300111955330758342147474062293202868155909489
      isProbablePrime p `shouldReturn` True
    it "correctly tests large pseudoprimes" $ do
      let p = 4547337172376300111955330758342147474062293202868155909393
      isProbablePrime p `shouldReturn` False
      let p = 47362882341088596725068562696893704769436677460225591859092704246296157080253
      isProbablePrime p `shouldReturn` False

  describe "Encryption" $ do
    -- Since key is short (64 bits), the encoded message will not have padding,
    -- so encryption and decryption is testable because it is deterministic
    let pub = Public {e = 3, n = 16525003591035973849}
        priv = Private {d = 2754167263817553827, n = 16525003591035973849}

    -- String "pwd" in hex = 02ff707764 => in decimal = 12875495268
        plaintext = "pwd"
        dec = 12875495268
    -- Manual encryption
        ciphertext = modExp dec (e pub) (n pub) 1
    -- Automated encryption
        encrypted = encrypt plaintext pub

    it "encrypts unpadded message" $ do
      encrypted `shouldReturn` ciphertext
    it "decrypts unpadded message" $ do
      let decrypted = encrypted >>= \ct -> return $ decrypt ct priv
      decrypted `shouldReturn` plaintext

    -- Longer 128-bit key
    let pub = Public {e = 7, n = 251515307574106143670689697734577448081}
        priv = Private {d = 5389613733730845935120800346634140743,
                        n = 251515307574106143670689697734577448081}
        encrypted = encrypt plaintext pub
        decrypted = encrypted >>= \ct -> return $ decrypt ct priv

    -- Since there is random padding here we can only test the final outcome
    it "decrypts padded message" $ do
      decrypted `shouldReturn` plaintext

    it "encrypts using 1024 bit keys" $ do
      keys <- keyPair 1024
      let (pub,priv) = keys
          encrypted  = encrypt plaintext pub
          decrypted  = encrypted >>= \ct -> return $ decrypt ct priv
      decrypted `shouldReturn` plaintext

    it "encrypts using 2048 bit keys" $ do
      keys <- keyPair 2048
      let (pub,priv) = keys
          encrypted  = encrypt plaintext pub
          decrypted  = encrypted >>= \ct -> return $ decrypt ct priv
      decrypted `shouldReturn` plaintext
