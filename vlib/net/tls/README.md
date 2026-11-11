# net.tls

`net.tls` is an **opt-in, pure-V** implementation of TLS 1.3 (RFC 8446) for TCP
connections. It is **not** the default TLS backend: `net.ssl` still selects the
vendored mbedTLS (or OpenSSL with `-d use_openssl`) unless you explicitly build
with `-d pure_tls`.

## Status

Pure-V, opt-in via `-d pure_tls`. TLS 1.3 only. ECDSA P-256 and RSA verify.
Unix trust-store paths only. ML-KEM-768/512/1024 supported. No 0-RTT, no
session resumption, no TLS 1.2, no DTLS.

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

## Scope and Limitations (accepted non-goals for v1)

- RSA *key generation* / RSA cert generation — ECDSA P-256 only (no crypto.rsa module).
- Windows system roots — Unix CA-bundle paths only; Windows trust store requires C interop.
- OpenSSL cross-check in CI — no openssl binary; DER validated structurally via parse_der.
- Session resumption, 0-RTT, SLH-DSA, DTLS, TLS 1.2, client-cert auth — out of scope.