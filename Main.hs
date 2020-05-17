module Main where

import Keys
import Signature

keys = keyPair 512
plaintext = "pwd"

main :: IO ()
main = do
    keys <- keyPair 512
    let (pub,priv) = keys
        md = hash plaintext
        signature = sign md priv
        verified = verify signature pub md
    print verified
