module Encoding where

import Data.Char
import Data.List
import Numeric

import Math

------------------------
--  Helper functions  --
------------------------

-- Searches for string inside another string and returns index.
indexOf :: String -> String -> Maybe Int
indexOf sub str = findIndex (isPrefixOf sub) (tails str)

-- Splits string into chunks of size n.
chunks :: Int -> String -> [String]
chunks n str = case splitAt n str of
                 (a, b) | null a    -> []
                        | otherwise -> a : chunks n b

-------------------------------
--  Hex/decimal conversions  --
-------------------------------

-- Converts decimal to hexadecimal.
decToHex :: (Show a, Integral a) => a -> String
decToHex x = showIntAtBase 16 intToDigit x ""

-- Converts hexadecimal to decimal.
hexToDec :: Integral a => String -> a
hexToDec x = case readHex x of
              (x, _):_ -> x
              _        -> error "Invalid hex"

-- Converts string to byte array in hexadecimal notation.
bytes :: String -> [String]
bytes x = map (decToHex . ord) x

-- Big-endian conversion of a hex string to decimal.
bytesToInt :: String -> Integer
bytesToInt [] = 0
bytesToInt (x:xs) = c * (16 ^ length xs) + bytesToInt xs
    where c = hexToDec [x]

----------------------------
--  Padding and encoding  --
----------------------------

-- Armours hex-encoded bytes with random padding according to PKCS#1 v1.5.
-- The scheme pads bytes to match the modulus size. For RSA keys of bit-length
-- 1024, this means padding to 64 (1024 / 16) bytes.
-- The byte `02` indicates that the padding scheme is operating in mode 2.
-- The byte `FF` signifies the start of the plaintext.
pad :: [String] -> Int -> IO [String]
pad xs bits = do
    -- Two bytes are for the padding header ("02") and stop ("ff") bytes
    let padLength = (bits `div` 16) - length xs - 2
    -- "ff" (255 in decimal) should not be in the padding
    randInts <- sequence $ replicate padLength (randInt (1, 254))
    let randBytes = map decToHex randInts
    return $ "02" : randBytes ++ ["ff"] ++ xs

-- Encodes a string (a key) to a padded integer that can then be encrypted.
-- For example, let the string = "key" and key size = 96 bits:
-- 1) convert to bytes -> ["6b","65","79"]
-- 2) add padding      -> ["02,"e6","ff","6b","65","79"]
-- 3) concat to hex    -> 02e6ff6b6579
-- 4) convert to int   -> 3191150962041
encode :: String -> Int -> IO Integer
encode str keySize = do
    padded <- pad (bytes str) keySize
    let byteStr = concat padded
    return $ bytesToInt byteStr

-- Reverses encoding. First removes padding (all symbols up to and including
-- "ff") then converts remaining bytes back to ASCII.
decode :: Integer -> String
decode x = do
    let hex   = decToHex x
        raw   = (\x -> drop (x + 2) hex) <$> indexOf "ff" hex
        bytes = chunks 2 <$> raw
        dec   = map hexToDec <$> bytes
        ascii = map (chr . fromIntegral) <$> dec
    case ascii of
      Just k  -> k
      Nothing -> error "Decode failed"
