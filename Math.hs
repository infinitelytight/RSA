module Math where

import MillerRabin
import System.Random
import Numeric

-----------------------------
--  Mathematics functions  --
-----------------------------

-- Computes greatest common divisor using Euclid's algorithm.
egcd :: (Integral a) => a -> a -> a
egcd x 0 = x
egcd x y = egcd y (x `rem` y)

-- Computes coefficients x and y of Bézout's identity, ax + by = gcd(a, b),
-- using the extended Euclidean algorithm.
eea :: (Integral a) => a -> a -> (a, a)
eea a b | b == 0    = (1, 0)
        | otherwise = (a', b')
      where a'     = snd coeffs
            b'     = (fst coeffs) - (a `div` b) * (snd coeffs)
            coeffs = eea b (a `mod` b)

-- Computes modular multiplicative inverse (solves `ax = 1 mod m` for x).
mmi :: (Integral a) => a -> a -> a
mmi a m = fst (eea a m) `mod` m

-- Computes Carmichael totient function, λ(n).
totient :: Integral a => a -> a -> a
totient x y = lcm (x - 1) (y - 1)

-- Computes modular exponentiation, x = a^e mod m (recursive; x starts with 1)
modExp :: Integral a => a -> a -> a -> a -> a
modExp a e m x | e <= 0    = x
               | otherwise = modExp a' e' m x'
             where a' = a * a `mod` m
                   e' = e `div` 2
                   x' = if e `mod` 2 == 1 then a * x `mod` m else x

-- Test whether two numbers are coprime
coprime :: Integral a => a -> a -> Bool
coprime x y = egcd x y == 1

-- Primality test by trial division (inefficient for large numbers)
isPrime :: Integer -> Bool
isPrime k | k < 2     = False
          | otherwise = null [x | x <- [2..isqrt k], k `mod` x == 0]
        where isqrt = floor . sqrt . fromIntegral

-- Primality test using Miller-Rabin algorithm with five rounds
isProbablePrime :: Integer -> IO Bool
isProbablePrime x = do
    let rs = replicate 5 (randomRIO (2, x - 1))
    witnesses <- sequence rs
    let ts = map (millerRabinPrimality x) witnesses
    return $ all (== True) ts

-- Generates random n-bit prime
randPrime :: Int -> IO Integer
randPrime n = do
    -- Integer is signed, so upper bound for n-bit prime is 2^n - 1
    let upper = 2^n - 1
    -- Lower bound ensures n is always n bits rather than n-1 bits
    let lower = ceiling $ sqrt 2 * 2^(n - 1)
    r <- randomRIO (lower, upper)
    t <- isProbablePrime r
    if t then return r else randPrime n

-- Generates random integer between `low` and `high` inclusive (this is here to
-- avoid specifying type inline like `randomRIO (1,10) :: IO Int`).
randInt :: (Int, Int) -> IO Int
randInt (low, high) = randomRIO (low, high)
