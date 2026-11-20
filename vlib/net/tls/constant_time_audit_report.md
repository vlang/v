# Constant-time audit report — TLS / crypto assurance

Tasking: raise assurance without editing existing sources (new files only),
with deterministic fuzz-shape tests, RFC KATs, and a written constant-time
audit (report only, no source fixes). Branch `feat/tls-suites`, head
`5c63d57cfe`. Line numbers below are as read during this session; the
working tree is shared with other agents and may have shifted since.

## Verdicts

### F1 — CONFIRMED: secret-dependent branches in `math.big` exponentiation

RSA private-key operations (sign/decrypt) branch on secret exponent bits:

- `vlib/math/big/integer.v:581` — `mod_pow` squares-and-multiplies with
  `if n & 1 == 1`, i.e. one multiply-or-skip per exponent bit.
- `vlib/math/big/integer.v:605` — `big_mod_pow` dispatches on exponent size
  into Montgomery / binary-window paths with the same property.
- `vlib/math/big/exponentiation.v:109,126,254,271` — sliding-window loops
  branch on `x.get_bit(...)` of the exponent.

Reachability (secret reaches the branch): `vlib/crypto/rsa/rsa.v:186`
(`decrypt`: `c.big_mod_pow(d, n)`), `rsa.v:229` (`sign_pkcs1_v15`),
`rsa.v:307` (`sign_pss`), `vlib/net/tls/rsa_verify.v:124`
(`rsa_pkcs1_sign_digest`), `vlib/net/tls/pss.v:91` (`rsa_pss_sign`).
Public-exponent verify paths (`rsa.v:238`, `rsa_verify.v:109`, both with
`e = 65537`) are explicitly *not* affected.

Exploit scenario: a co-located attacker (shared host / precise local timer)
sampling server RSA signing latency — e.g. CertificateVerify signatures on
handshakes using RSA credentials — recovers `d` bit-by-bit, the classic
timing attack. Remote exploitation is hampered by network jitter; severity
is medium. Mitigation direction (not applied, report-only): constant-time
Montgomery ladder or RSA blinding in `math.big`.

### R1 — REFUTED: `constant_time_compare` length early-return

`vlib/crypto/subtle/comparison.v:22-25` returns 0 when lengths differ. All
compared lengths are public protocol constants (16-byte tags, 32/48-byte
hashes). No secret leaks.

### R2 — REFUTED: AES-GCM tag check

`vlib/crypto/aes/aes_gcm.v:127-129` rejects short input, then `bytes_equal`
(`aes_gcm.v:281-290`) accumulates `diff |= a[i] ^ b[i]` over the full tag
before a single uniform `AES-GCM authentication failed` error. No oracle.

### R3 — REFUTED: ChaCha20-Poly1305 tag check

`vlib/x/crypto/chacha20poly1305/chacha20poly1305.v:183` compares with
`subtle.constant_time_compare` and returns one uniform `unmatching tag`
error. No oracle.

### R4 — REFUTED: `hmac.equal`

`vlib/crypto/hmac/hmac.v:39-41` delegates to `constant_time_compare`. The
length caveat in its comment only concerns callers mixing hash functions,
not timing.

### R5 — REFUTED: TLS Finished / PSK-identity / binder compares

`vlib/net/tls/conn.v:661`, `server.v:1155,1225`, `ticket.v:403-409` all use
`constant_time_compare`; Finished length is pinned by `parse_finished`
to the public suite hash length. Good practice confirmed.

### R6 — REFUTED: RSA PKCS#1 v1.5 verify

`vlib/crypto/rsa/rsa.v:238-250`, `vlib/net/tls/rsa_verify.v:108-121`:
`sig.len != k` early error depends only on the public modulus size, the
error is uniform (`PKCS#1 v1.5 verification failed` / `error_bad_signature`),
and the final EM check is constant-time. TLS 1.3 never PKCS#1-decrypts
(signatures only), so there is no Bleichenbacher-style oracle.

### R7 — REFUTED: RSA-PSS verify zero-scan early exits

`vlib/crypto/rsa/rsa.v:348-354`, `vlib/net/tls/pss.v:137-148` return early
on non-zero padding bytes, but the scanned buffer derives from the
attacker-supplied signature through a public-key operation — no secret is
involved, and every failure maps to one uniform error. Timing reveals only
the attacker's own input. PSS salts come from `crypto.rand` (sign paths).

### R8 — REFUTED: X25519 ladder

`vlib/x/crypto/curve25519/curve25519.v:334-364` runs a fixed 255-iteration
ladder with a mask-based conditional swap
(`edwards25519/element.v:690`); zero/low-order rejects act on public
points. Constant-time by construction.

### R9 — REFUTED: DER tag/length structural checks

Tag comparisons (`!= 0x30`, `!= 0x02`, …) in `vlib/net/tls/cert.v`,
`cert_verify_sig.v`, and `vlib/crypto/x509` act on attacker-controlled
bytes with fail-closed errors. No secret operand.

### R10 — REFUTED: HKDF/AEAD length caps

Branches on lengths only (`hkdf` 255-block cap, AES-GCM `max_*_size`,
ChaCha20-Poly1305 plaintext cap). Lengths are not secret.

### N1 — NOTE (not a finding): ECDSA P-256 via vendored mbedTLS

`vlib/crypto/ecdsa/ecdsa_notd_use_openssl.v` delegates scalar multiply to
`mbedtls_ecdh_compute_shared` / `mbedtls_ecdsa_write_signature` with a
blinding RNG on the sign path. Timing properties live in the C dependency
and are outside this V-side audit; the V glue contributes no
secret-dependent branch. Supply-chain assumption, recorded not verified.

## KAT sources (all embedded, no network)

SHA-256/SHA-224 FIPS 180-2 B.1/B.2; SHA-384/512 family digests pinned from
`sha512_test.v`; HMAC RFC 2202 §3 cases 1–3 (+`equal`); HKDF RFC 5869 A.1
cases 1 and 3 (`extract`/`expand`/`key` agreement); AES-128/256-GCM NIST
SP 800-38D worked examples; ChaCha20-Poly1305 RFC 8439 §2.8.2; X25519
RFC 7748 §6.1 and §5.2; ECDSA P-256 OpenSSL-backend oracle vector plus
RFC 6979 determinism (sign-twice equality); RSA RFC 8017 §5.1 textbook
RSAEP (`65^17 mod 3233 = 2790`) plus a fixed 1024-bit PKCS#1 v1.5 vector
(deterministic, self-generated once, now pinned); textbook DH (`p=23,g=5`)
for the FFDHE math primitive (the stack negotiates ECDHE-only).

## Tests run

- `vlib/crypto/kat_test.v` — 14 tests, PASS (205 ms runtime in-tree;
  497 ms on pristine HEAD `5c63d57cfe`). No skips, no network.
- `vlib/net/tls/fuzz_test.v` — 10 tests, PASS (914 ms runtime,
  ~11k parser invocations, fixed seeds; far under the 60 s budget). The
  shared working tree was mid-edit by other agents during this session
  (transient compile errors in `messages.v`, `handshake.v`, `client.v`,
  `client_auth.v`, `cert_verify_sig.v` — none in the new files, and the
  pre-existing `messages_test.v` failed identically at the time); the
  suite was additionally gated on a pristine HEAD `5c63d57cfe` worktree
  (882 ms runtime) and re-run green in-tree after those edits landed.
- `v fmt -w` applied to both new test files; this report checked with
  `v check-md`. `VJOBS=4`, private `TMPDIR` under
  `%LOCALAPPDATA%\Temp\opencode\assure`.

## Created paths (new files only, no existing file modified)

- `vlib/net/tls/fuzz_test.v`
- `vlib/net/tls/testdata/fuzz/README.md`
- `vlib/net/tls/testdata/fuzz/client_hello_seed.hex`
- `vlib/net/tls/testdata/fuzz/server_hello_seed.hex`
- `vlib/net/tls/testdata/fuzz/plain_record_seed.hex`
- `vlib/net/tls/testdata/fuzz/nst_seed.hex`
- `vlib/net/tls/testdata/fuzz/ee_seed.hex`
- `vlib/net/tls/testdata/fuzz/handshake_ch_seed.hex`
- `vlib/net/tls/testdata/fuzz/handshake_sh_seed.hex`
- `vlib/net/tls/testdata/fuzz/pem_seed.pem`
- `vlib/crypto/kat_test.v`
- `vlib/net/tls/constant_time_audit_report.md` (this file)

Helpers kept outside the repo (assurance TMPDIR, not part of the change):
`genrsa.v` (fixed RSA KAT material), `genseeds.v` / `genpem.v` (corpus
generation), `dbg_pem.v` (throwaway debug), pristine worktree of HEAD for
gating.
