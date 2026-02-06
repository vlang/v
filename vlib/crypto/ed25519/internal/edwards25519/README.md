## Description

`crypto.ed25519.internal.edwards25519` provides arithmetic primitives operations that are useful to
implement cryptographic schemes over the edwards25519 elliptic curve, including:

1.  Arithmetic functions for point addition, doubling, negation, scalar multiplication
    with an arbitrary point, with the base point, etc.
2.  Arithmetic functions dealing with scalars modulo the prime order L of the base point.

This modules was port of Golang `edwards25519` library from [edwards25519](https://github.com/FiloSottile/edwards25519) to the V language.

## About Edwards25519

Twisted Edwards curves are a family of elliptic curves allowing complete addition
formulas without any special case and no point at infinity.
Curve edwards25519 is based on prime 2^255 - 19 for efficient implementation.
Equation and parameters are given in RFC 7748.
