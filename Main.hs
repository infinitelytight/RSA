module Main where

import Encoding
import Keys
import RSA
import Signature

----------------------
--  Sample program  --
----------------------

import System.Random

alice :: IO (Key, Key)
alice = keyPair 1024

bob :: IO (Key, Key)
bob = keyPair 1024

plaintext = "password"

main :: IO ()
main = do
    putStrLn $ "Plaintext: " ++ plaintext

    putStrLn "\nConverted to hex"
    print $ bytes plaintext

    a <- alice
    b <- bob
    let (aPub, aPriv) = a
        (bPub, bPriv) = b
        aKeySize     = bitLength aPub

    putStrLn "\nEncoded according to padding scheme"
    pad (bytes plaintext) aKeySize >>= print

    putStrLn "\nAs decimal"
    encode plaintext aKeySize >>= print

    ciphertext <- encrypt plaintext bPub
    putStrLn "\nEncrypted with Bob's public key"
    print ciphertext

    let md        = hash plaintext
        signature = sign md aPriv
    putStrLn "\nMessage digest signed by Alice using her private key"
    print signature

    putStrLn "\nDecrypted with Bob's private key"
    print $ decrypt ciphertext bPriv

    putStrLn "\nSignature verified with Alice's public key"
    print $ verify signature aPub md
