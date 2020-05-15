module RSA where

import Encoding
import Keys
import Math

-- Armours plaintext with random padding and performs RSA encryption.
-- Plaintext is a string that is commonly used as the key for some faster
-- symmetric-key algorithm, like AES, to encrypt sensitive data.
encrypt :: String -> Key -> IO Integer
encrypt plaintext (Public e n) = do
    let keySize = bitLength n
    padded <- encode plaintext keySize
    let ciphertext = modExp padded e n 1
    return ciphertext
encrypt _ (Private _ _) = error "Wrong key. Encrypt with public key."

-- Reverses encryption and decodes padded message.
decrypt :: Integer -> Key -> String
decrypt ciphertext (Private d n) = decode $ modExp ciphertext d n 1
decrypt _          (Public  _ _) = error "Wrong key. Decrypt with private key."
