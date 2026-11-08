# net.tls

`net.tls` is an **opt-in, pure-V** implementation of TLS 1.3 (RFC 8446) for TCP
connections. It is **not** the default TLS backend: `net.ssl` still selects the
vendored mbedTLS (or OpenSSL with `-d use_openssl`) unless you explicitly build
with `-d pure_tls`.

## Status

Experimental. Client and server handshake, record layer, and (eventually) hybrid
post-quantum key exchange are being added incrementally. TLS 1.2, DTLS, and
session resumption are out of scope for v1.

## Build

```sh
./v -d pure_tls your_program.v
```

## Design notes

- Pure V only: no `C.`/`JS.` symbols in `.v` files. Enforced by running tests
  with `-Wimpure-v`.
- Classical cryptography reuses the existing pure-V primitives
  (`crypto.ecdsa`, `crypto.x509`, `x.crypto.curve25519`, `crypto.hkdf`,
  `crypto.aes`, `crypto.sha256`).
- Post-quantum support targets hybrid key exchange (`X25519MLKEM768`) and
  ML-DSA certificate verification first; SLH-DSA stays on the OpenSSL wrapper
  until a pure-V SLH-DSA module exists.
- OpenSSL and mbedTLS remain the correctness oracles for differential tests;
  they are never runtime dependencies of this module.
module tls