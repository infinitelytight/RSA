import Data.Char (ord, chr)
import System.Random

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

coprime :: Integral a => a -> a -> Bool
coprime x y = egcd x y == 1

isPrime :: Integer -> Bool
isPrime k | k < 2     = False
          | otherwise = null [x | x <- [2..isqrt k], k `mod` x == 0]
    where isqrt = floor . sqrt . fromIntegral

getPrime :: Integer -> IO Integer
getPrime n = do
    r <- randomRIO (2 ^ n, (2 ^ (n + 1)) - 1)
    if (isPrime r) then return r else getPrime n

----------------------
--  Key generation  --
----------------------

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

-- Returns key pair (public key, private key)
keygen :: ((Integer, Integer), (Integer, Integer))
keygen = ((e, n), (d, n))
    where p = 11 -- randPrime
          q = 13 -- randPrime
          n = p * q -- modulus
          m = totient p q -- λ(n)
          e = pubExp m
          d = privExp e m

---------------------------------
--  Encryption and decryption  --
---------------------------------

join :: [Int] -> Integer
join = read . concat . map show

encode :: String -> [Integer]
encode = map (fromIntegral . ord)

decode :: [Integer] -> String
decode = map (chr . fromIntegral)

encrypt :: String -> (Integer, Integer) -> [Integer]
encrypt plaintext (e, n) = map (\p -> p^e `mod` n) (encode plaintext)

decrypt :: [Integer] -> (Integer, Integer) -> [Integer]
decrypt ciphertext (d, n) = map (\c -> c^d `mod` n) ciphertext

------------
--  Test  --
------------

-- TODO generate key with random seed
pubKey = fst keygen
privKey = snd keygen

plaintext = "hello"
encoded = encode plaintext
ciphertext = encrypt plaintext pubKey
decrypted = decrypt ciphertext privKey
decoded = decode decrypted

_main = print plaintext
    *> print encoded
    *> print ciphertext
    *> print decrypted
    *> print decoded
