import Data.Char (ord, chr)

join :: [Int] -> Integer
join = read . concat . map show

encode :: String -> [Integer]
encode = map (fromIntegral . ord)

decode :: [Integer] -> String
decode = map (chr . fromIntegral)

encrypt :: String -> (Integer, Integer) -> [Integer]
encrypt plaintext (n, e) = map (\p -> p^e `mod` n) (encode plaintext)

decrypt :: [Integer] -> (Integer, Integer) -> [Integer]
decrypt ciphertext (n, d) = map (\c -> c^d `mod` n) ciphertext

keygen :: ((Integer, Integer), (Integer, Integer))
keygen = ((n, e), (n, d))
    where p = 11
          q = 13
          n = p * q
          phi = (p - 1) * (q - 1)
          e = 7 -- 65537
          d = 103 -- (1 `mod` phi) `div` d

pubKey = fst keygen
privKey = snd keygen

plaintext = "h"
encoded = encode plaintext
ciphertext = encrypt plaintext pubKey
decrypted = decrypt ciphertext privKey
decoded = decode decrypted

main = print plaintext
    *> print encoded
    *> print ciphertext
    *> print decrypted
    *> print decoded
