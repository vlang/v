# net.quic — build dependencies

Tracking issue: [vlang/v#27675](https://github.com/vlang/v/issues/27675).

## TLS 1.3 approach

`net.quic` implements a QUIC-scoped TLS 1.3 handshake (RFC 8446) in pure V,
rather than patching vendored mbedTLS (which has no QUIC support in any
released version — see the issue for the full rationale). X.509 certificate
parsing and chain validation are delegated to mbedTLS's already-bound C
functions (`mbedtls_x509_crt_parse`, `mbedtls_x509_crt_verify`,
`mbedtls_pk_parse_key`, `mbedtls_pk_verify_ext`/`mbedtls_pk_verify`) — the same
thing `net.http`'s HTTP/1.1 and HTTP/2 backends already do. No mbedTLS source
patch is required for this.

## OpenSSL dependency: hard, not opt-out

TLS 1.3 signature verification for `net.quic` needs P-256 ECDH (for the
`secp256r1` `key_share` group) and RSA-PSS (for the `rsa_pss_rsae_*` signature
schemes), neither of which exist anywhere in V today. Both are added as new
OpenSSL bindings (`vlib/crypto/ecdsa/ecdsa.c.v`, new `vlib/crypto/rsa_pss/`),
following the same `-lcrypto` linkage `crypto.ecdsa` already uses for ECDSA
sign/verify.

This was a candidate for a `-d no_openssl_quic` opt-out flag (falling back to
Ed25519-only cert chain support), but is **not needed**: Windows CI
(`windows_ci_gcc.yml`) already builds and runs `vlib/crypto/ecdsa/ecdsa_test.v`
against OpenSSL today, with explicit OpenSSL diagnostics steps beforehand. So
the exact dependency `net.quic` needs is already proven to build and pass on
Linux, macOS, and Windows. **Decision: P-256 ECDH and RSA-PSS are a hard
dependency of `net.quic`.** No opt-out build flag, no reduced-interop fallback
mode.

## mbedTLS X.509-only usage (no `mbedtls_ssl_context`)

`net.quic` calls `mbedtls_x509_crt_parse`/`_verify` and `mbedtls_pk_parse_key`/
`mbedtls_pk_verify` directly, without ever constructing an
`mbedtls_ssl_context` — a usage pattern the existing `net.mbedtls.SSLConn` path
never exercises (it always builds a full SSL context/config). This is
confirmed to work: see `vlib/net/mbedtls/x509_standalone_test.v`, which parses
and verifies a certificate with no `mbedtls_ssl_context` in scope, relying only
on the module's existing `init()` (`v_mbedtls_threading_setup()`, already
called automatically on `import net.mbedtls` regardless of whether an
`SSLConn` is ever constructed).
