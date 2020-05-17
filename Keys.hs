module Keys where

import Math

----------------------
--  Key generation  --
----------------------

data Key = Public  { e :: Integer, n :: Integer }
         | Private { d :: Integer, n :: Integer }
    deriving (Eq, Ord, Show)

-- Returns public key exponent, e, which is coprime to λ(n).
-- In RSA e is usually set to 65,537 so this function is normally not needed.
pubExp :: Integer -> Integer
--pubExp m = head [n | n <- [3..m - 1] , coprime n m]
pubExp m = 65537

-- Returns private key exponent, d. This is the modular multiplicative inverse
-- of `e mod λ(n)`, that is, `d = (1 / e) mod λ(n)`. Since e and λ(n) are
-- coprime, solving `de = 1 mod λ(n)` for d can be done using the extended
-- Euclidean algorithm. Let m = λ(n).
-- -- de = 1 mod m
-- Since this means that m divides evenly into `de - 1`, it can be written as
-- -- mn = de - 1, where n is an integer
-- Since gcd(e, m) = 1, then, by Bézout's identity
-- -- xe + ym = 1
-- Once solved, the ym term can be ignored, and the coefficient of e, x, can
-- then be used to calculate d (solution is with respect to mod m):
-- -- d = x mod m
privExp :: Integer -> Integer -> Integer
privExp e m = mmi e m

-- Returns all key components (e, d, and n) from two primes.
keygen :: Integer -> Integer -> (Integer, Integer, Integer)
keygen p q = (e, d, n)
    where n = p * q       -- modulus
          m = totient p q -- λ(n)
          e = pubExp m    -- public key
          d = privExp e m -- private key

-- Returns n-bit public-private keypair.
keyPair :: Int -> IO (Key, Key)
keyPair keySize = do
    let bits = keySize `div` 2
    p <- randPrime bits
    q <- randPrime bits
    let (e, d, n) = keygen p q
    return $ (Public {e = e, n = n}, Private {d = d , n = n})

-- Returns the length of a key in bits, used to determine key size from modulus.
bitLength :: Key -> Int
bitLength (Public _ n) = floor $ (logBase 2 i) + 1
    where i = fromIntegral n
