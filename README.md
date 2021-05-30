# haskell-ctf-crypto
A cryptography library in Haskell meant for use in GHCi and short scripts.
The main purpose of this library is quickly solving CTF problems.

## To do:
- [ ] Implement polynomial rings.
- [x] Implement primality tests (Miller-Rabin).
- [ ] ~~Implement Number Theoretic Transform.~~
- [ ] ~~Implement polynomial quotient rings.~~
- [ ] ~~Write tests for polynomial rings and quotient rings.~~
- [ ] ~~Write alternate show functions for Z that can print the number in hexadecimal and like Python's bytes (printable characters are printed, and hex chars are shown as `\xab`, where ab are the hex digits).~~

- [x] Implement ElGamal decryption and encryption functions.
- [x] Implement RSA encryption and decryption functions, along with some helper functions to create a full private key from only a few parameters, for instance from (p, q, e), (n, phi, e), and (n, d, e).
- [x] Implement multiprime RSA too.
- [x] Implement elliptic curves over GF(p).
- [ ] Write some helper functions so I don't have to enter as many characters when running using complicated datatypes like elliptic curves: for instance, take `gpow (EC (ZnZ (Z 6) (Z 3623)) (ZnZ (Z 730) (Z 3623)) (ZnZ (Z 14) (Z 3623)) (ZnZ (Z 19) (Z 3623))) 947`. This could instead be written as `ec_pow (6, 730) 14 19 3623`. Also write some to initalize lists of `ZZP` or similar from bare integers in a list with a separate modulus.
- [ ] Write a helper function that accepts an integer element and modulus, and calculates whether it's a power of 2, composite, or prime, and puts it in a GF2X, ZZN or ZZP respectively.
- [x] Get some Legendre symbols.
- [ ] Write some pretty printers and corresponding readers but don't make them default because they're too much of a pain.
- [ ] Rewrite a heap of functions for performance.
- [ ] Create some network helpers analogous to pwntools' `remote(url, port)`.
