# RSA

This is an insecure, but "full" implementation of RSA in Haskell*, going beyond
the mathematical proof-of-work, to demonstrate the cryptosystem in practice
without the use of external libraries.

---

There are countless RSA implementations on the internet, written for
cryptography assignments and as programming exercises. Like those, this one also
comes with an obligatory disclaimer that homemade crypto it isn't secure.

This project, however, goes a little further than "textbook RSA" by including
the major elements of the RSA system often omitted from basic versions:

- A real padding scheme is used to armour the plaintext
- Keys are generated with proper bit-lengths and with conventional parameters
- Digital signatures are supported
- Encryption, decryption, and key generation is fast

Additionally, all algorithms involved are written from scratch in vanilla
Haskell and are adequately efficient.

This code still has many vulnerabilities and is susceptible to side-channel
attacks, but the components of the RSA algorithm are all here for those
interested in understanding or writing their own system.

\**It must be noted that I've been using Haskell for only about a month, so this
code is probably not idiomatic and does not take advantage of Haskell's more
powerful features.*
