# net.tls

`net.tls` is an **opt-in, pure-V** implementation of TLS 1.3 (RFC 8446) for TCP
connections. It is **not** the default TLS backend: `net.ssl` still selects the
vendored mbedTLS (or OpenSSL with `-d use_openssl`) unless you explicitly build
with `-d pure_tls`.

## Status

Pure-V, opt-in via `-d pure_tls`. TLS 1.3 only. ECDSA P-256/P-384/P-521,
ML-DSA-44/65/87 and RSA leaves with RSA/ECDSA/ML-DSA direct-root verify.
Trust roots resolve on every OS: explicit `ca_store`/`ca_cert`, then
`SSL_CERT_FILE`/`SSL_CERT_DIR`, then Unix bundle paths, then the embedded
Mozilla bundle. X25519MLKEM768 hybrid key exchange (the `x.crypto.mlkem`
library also implements raw 512/1024 variants, which `net.tls` does not
negotiate). No 0-RTT, no session resumption, no TLS 1.2, no DTLS.

## Build

```sh
./v -d pure_tls your_program.v
```

## Server credentials

Configure both `Config.cert` and `Config.key` (PEM file paths), or provide
`cert_pem` and `key_pem`. The server accepts P-256/P-384/P-521 ECDSA
(SEC1 or PKCS#8 PEM), ML-DSA-44/65/87 (PKCS#8 PEM) and RSA (PKCS#1 or
PKCS#8 PEM) leaf certificates and keys, and selects the CertificateVerify
scheme the client offers for the key type (ECDSA: the curve's scheme;
ML-DSA: the variant's scheme 0x0904/0x0905/0x0906; RSA: PSS preferred over
PKCS#1 v1.5).
If neither is configured, the server creates a temporary self-signed
`localhost` certificate; clients using chain verification will not
trust it unless it is explicitly added to their trust store.

## Design notes

- Pure V only: no `C.`/`JS.` symbols in `.v` files. Enforced by running tests
  with `-Wimpure-v`.
- Classical cryptography reuses the existing pure-V primitives
  (`crypto.ecdsa`, `crypto.x509`, `x.crypto.curve25519`, `crypto.hkdf`,
  `crypto.aes`, `crypto.sha256`).
- Post-quantum key exchange uses `X25519MLKEM768`; post-quantum signatures
  use ML-DSA-44/65/87 end to end (x509 parsing/generation, chain
  verification, CertificateVerify). SLH-DSA remains on the OpenSSL wrapper
  until a pure-V SLH-DSA module exists.
- OpenSSL and mbedTLS remain the correctness oracles for differential tests;
  they are never runtime dependencies of this module.

## Scope and Limitations (accepted non-goals for v1)

- RSA *key generation* — V can verify and sign with RSA but cannot generate
  RSA keys (no crypto.rsa module); test fixtures live in `testdata/`.
- OpenSSL cross-check in CI — no openssl binary; DER validated structurally via parse_der.
- Session resumption, 0-RTT, SLH-DSA, DTLS, TLS 1.2, client-cert auth,
  HelloRetryRequest, NewSessionTicket, KeyUpdate — out of scope.
- Chain building — v1 verifies the leaf directly against a trusted root only;
  intermediates are ignored. RSA-PSS-signed CA certificates are rejected.
- Signature hashes follow the algorithm OID exactly (no SHA-1/MD5 anywhere);
  ML-DSA signs the raw TBS/content bytes with no pre-hash.

## Trust roots (`cacert.pem`)

`cacert.pem` is the Mozilla CA bundle as distributed by curl
(`https://curl.se/ca/cacert.pem`, MPL-2.0, extracted from Mozilla's
`certdata.txt`; this snapshot is dated 2026-11-05). It is embedded into
`net.tls` binaries via `$embed_file` and used as the last-resort trust
source, which is what makes default chain verification work on Windows and
macOS with zero C interop. To refresh it, download the current bundle over
TLS, confirm all 118+ certificates parse (`test_certstore_embedded_bundle`
pins the count — bump it deliberately), and re-run the `x509` and `tls`
suites.
