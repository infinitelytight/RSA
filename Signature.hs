module Signature where

import Data.Bits
import Data.Char
import Data.Unique

import Encoding
import Keys
import Math
import RSA

--------------------------
--  Digital signatures  --
--------------------------

type Digest = Integer

-- Generates a message digest, based on Java's hashCode() function.
-- This hash function is obviously not secure. Normally SHA is used in RSA.
hash :: String -> Digest
hash str = toInteger . abs $ md .&. md
    where md = foldl (\x char -> (shift x 5) - x + ord char) 0 str

-- Produces a digital signature on the message digest with the sender's own
-- private key. The hash function used in this program returns an integer so no
-- encoding is required.
sign :: Digest -> Key -> Integer
sign digest (Private d n) = modExp digest d n 1
sign _      (Public  _ _) = error "Sign with your private key"

-- Verifies whether the signed message is from the purported sender.
verify :: Integer -> Key -> Digest -> Bool
verify signature (Public e n)  md = md == modExp signature e n 1
verify _         (Private _ _) _  = error "Verify with your public key"
