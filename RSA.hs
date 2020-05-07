module RSA where

import Data.Char
import System.Random
import Numeric
import Data.List

------------------------
--  Helper functions  --
------------------------

-- Euclid's algorithm for calculating the greatest common divisor
egcd :: (Integral a) => a -> a -> a
egcd x 0 = x
egcd x y = egcd y (x `rem` y)

-- Extended Euclidean algorithm for calculating the coefficients x and y of
-- Bézout's identity, ax + by = gcd(a, b)
eea :: (Integral a) => a -> a -> (a, a)
eea a b | b == 0    = (1, 0)
        | otherwise = (a', b')
        where a'     = snd coeffs
              b'     = (fst coeffs) - (a `div` b) * (snd coeffs)
              coeffs = eea b (a `mod` b)

-- Modular multiplicative inverse, solves `ax = 1 mod m` for x
mmi :: (Integral a) => a -> a -> a
mmi a m = fst (eea a m) `mod` m

-- Carmichael totient function, λ(n)
totient :: Integral a => a -> a -> a
totient x y = lcm (x - 1) (y - 1)

-- Modular exponentiation, x = a^e mod m (recursive; x starts with 1)
modExp :: Integral a => a -> a -> a -> a -> a
modExp a e m x | e <= 0    = x
               | otherwise = modExp a' e' m x'
             where a' = a * a `mod` m
                   e' = e `div` 2
                   x' = if e `mod` 2 == 1 then a * x `mod` m else x

coprime :: Integral a => a -> a -> Bool
coprime x y = egcd x y == 1

isPrime :: Integer -> Bool
isPrime k | k < 2     = False
          | otherwise = null [x | x <- [2..isqrt k], k `mod` x == 0]
    where isqrt = floor . sqrt . fromIntegral

-- Generates random n-bit prime
randPrime :: Int -> IO Integer
randPrime n = do
    r <- randomRIO (2^n, (2^(n + 1)) - 1)
    if (isPrime r) then return r else randPrime n

randInt :: Int -> Int -> IO Int
randInt low high = randomRIO (low, high)

----------------------
--  Key generation  --
----------------------

data Key = Public (Integer, Integer) | Private (Integer, Integer)
    deriving (Eq, Ord, Show)

-- Public key exponent, e, is coprime to λ(n) (e is usually set to 65,537)
pubExp :: Integer -> Integer
pubExp m = head [n | n <- [3..m - 1] , coprime n m]

-- Private key exponent, d. This is the modular multiplicative inverse of
-- `e mod λ(n)`, that is, `d = (1 / e) mod λ(n)`. Since e and λ(n) are coprime,
-- solving `de = 1 mod λ(n)` for d can be done using the extended Euclidean
-- algorithm. Let m = λ(n).
-- de = 1 mod m
-- Since this means that m divides evenly into `de - 1`, it can be written as
-- mn = de - 1, where n is an integer
-- Since gcd(e, m) = 1, then, by Bézout's identity
-- xe + ym = 1
-- Once solved, the ym term can be ignored, and the coefficient of e, x, can
-- then be used to calculate d (solution is with respect to mod m):
-- d = x mod m
privExp :: Integer -> Integer -> Integer
privExp e m = mmi e m

-- Returns all key components (n, e, and d) from two primes
keygen :: Integer -> Integer -> (Integer, Integer, Integer)
keygen p q = (e, d, n)
    where n = p * q       -- modulus
          m = totient p q -- λ(n)
          e = pubExp m    -- public key
          d = privExp e m -- private key

-- Returns n-bit public-private keypair
keyPair :: Int -> IO (Key, Key)
keyPair keySize = do
    let bits = keySize `div` 2
    p <- randPrime bits
    q <- randPrime bits
    let (e, d, n) = keygen p q
    return $ (Public (e, n), Private (d, n))

---------------------------------
--  Encryption and decryption  --
---------------------------------

--join :: [Int] -> Integer
--join = read . concat . map show

indexOf :: String -> String -> Maybe Int
indexOf sub str = findIndex (isPrefixOf sub) (tails str)

-- Split string into chunks of size n
chunks :: Int -> String -> [String]
chunks n str = case splitAt n str of
                 (a, b) | null a    -> []
                        | otherwise -> a : chunks n b

--encode :: String -> [Integer]
--encode = map (fromIntegral . ord)
--
--decode :: [Integer] -> String
--decode = map (chr . fromIntegral)
--
--encrypt :: String -> Key -> [Integer]
--encrypt plaintext (Public (e, n)) = map (\p -> modExp p e n 1) (encode plaintext)
--
--decrypt :: [Integer] -> Key -> [Integer]
--decrypt ciphertext (Private (d, n)) = map (\c -> modExp c d n 1) ciphertext

decToHex :: (Show a, Integral a) => a -> String
decToHex x = showIntAtBase 16 intToDigit x ""

hexToDec :: Integral a => String -> a
hexToDec x = case readHex x of
              (x, _):_ -> x
              _        -> error "Invalid hex"

-- Converts string to byte array in hexadecimal notation
bytes :: String -> [String]
bytes x = map (decToHex . ord) x

-- Big-endian conversion of a hex string to decimal
bytesToInt :: String -> Integer
bytesToInt [] = 0
bytesToInt (x:xs) = c * (16 ^ length xs) + bytesToInt xs
    where c = hexToDec [x]

-- Armours hex-encoded bytes with random padding according to PKCS#1 v1.5.
-- The scheme pads the bytes to match the modulus size. For RSA keys of
-- bitlength 1024, this means padding to 64 bytes.
-- The byte `02` indicates that the padding scheme is operating in mode 2.
-- The byte `FF` signifies the start of the plaintext.
pad :: [String] -> Int -> IO [String]
pad xs bits = do
    -- Two bytes are for the padding header ("02") and stop ("ff") bytes
    let padLength = (bits `div` 16) - length xs - 2
    -- "ff" (255 in decimal) should not be in the padding
    randInts <- sequence $ replicate padLength (randInt 1 254)
    let randBytes = map decToHex randInts
    return $ "02" : randBytes ++ ["ff"] ++ xs

-- Let plaintext = "key"
-- 1) convert to bytes -> ["6b","65","79"]
-- 2) add padding      -> ["02,"e6","ff","6b","65","79"] (96-bit key)
-- 3) concat to hex    -> 0x02e6ff6b6579
-- 4) convert to int   -> 3191150962041
encode :: String -> Int -> IO Integer
encode str keysize = do
    padded <- pad (bytes str) keysize
    let byteStr = concat padded
    return $ bytesToInt byteStr

-- Reverses encoding. First removes padding (all bytes to "ff") then converts
-- numbers back to ASCII.
decode :: Integer -> String
decode x = do
    let hex   = decToHex x
        unpad = (\x -> drop (x + 2) hex) <$> indexOf "ff" hex
        bytes = chunks 2 <$> unpad
        dec   = map hexToDec <$> bytes
        key   = map (chr . fromIntegral) <$> dec
    case key of
      Just k  -> k
      Nothing -> error "Decode failed"
