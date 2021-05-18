# haskell-ctf-crypto
A cryptography library in Haskell meant for use in GHCi and short scripts.
The main purpose of this library is quickly solving CTF problems.

## To do:
- [ ] Write show and read instances for my datatypes so I can enter them into Haskell easily. Bonus points for interoperability with sagemath.
- [ ] Implement polynomial rings.
- [ ] Implement polynomial quotient rings.
- [ ] Write tests for polynomial rings and quotient rings.
- [ ] Write alternate show functions for Z that can print the number in hexadecimal and like Python's bytes (printable characters are printed, and hex chars are shown as `\xab`, where ab are the hex digits).
- [ ] Implement ElGamal decryption and encryption functions.
- [ ] Implement RSA encryption and decryption functions, along with some helper functions to create a full private key from only a few parameters, for instance from (p, q, e), (n, phi, e), and (n, d, e).
- [ ] Implement multiprime RSA too.
- [ ] Implement primality tests.
- [ ] Get some Legendre symbols.
- [ ] Create some network helpers analogous to pwntools' `remote(url, port)`.
