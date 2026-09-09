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

TLS 1.3 key exchange for `net.quic` needs P-256 ECDH (for the `secp256r1`
`key_share` group), which didn't exist anywhere in V before this. It's added
as a new OpenSSL binding (`vlib/crypto/ecdsa/ecdsa.c.v`), following the same
`-lcrypto` linkage `crypto.ecdsa` already uses for ECDSA sign/verify.

This was a candidate for a `-d no_openssl_quic` opt-out flag (falling back to
Ed25519-only cert chain support), but is **not needed**: Windows CI
(`windows_ci_gcc.yml`) already builds and runs `vlib/crypto/ecdsa/ecdsa_test.v`
against OpenSSL today, with explicit OpenSSL diagnostics steps beforehand. So
the exact dependency `net.quic` needs is already proven to build and pass on
Linux, macOS, and Windows. **Decision: P-256 ECDH is a hard dependency of
`net.quic`.** No opt-out build flag, no reduced-interop fallback mode.

## HTTP/3 connection lifetime

Callers that construct an HTTP/3 connection directly with `new_h3_conn` must call `H3Conn.free`
after the connection will no longer be polled. This releases the OpenSSL and mbedTLS resources
owned by its QUIC handshake. The method is idempotent, so a `defer` is the simplest cleanup:

```v ignore
mut h3 := quic.new_h3_conn(mut connection, params)
defer {
	h3.free()
}
```

The `net.http` HTTP/3 transport performs this cleanup itself; this requirement applies to direct
`net.quic` users.

CertificateVerify signature verification (ECDSA and RSA-PSS) and certificate
chain-of-trust validation (including RSA-PKCS1v1.5-signed certificates, still
common among real-world CAs — `net.quic` advertises this via the
`signature_algorithms_cert` extension, RFC 8446 §4.2.3) are both handled
through mbedTLS's already-vendored, already-bound C functions
(`mbedtls_pk_verify_ext`, `mbedtls_x509_crt_verify`) — no OpenSSL dependency
for either. (An earlier draft of this file added a separate
`vlib/crypto/rsa_pss/` OpenSSL module for RSA-PSS specifically; it was removed
as unused dead code once the mbedTLS path above was confirmed to cover the
same need.)

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
