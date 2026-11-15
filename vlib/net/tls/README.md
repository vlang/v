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
Mozilla bundle. X25519, ECDHE P-256/P-384, FFDHE 2048/3072, pure
ML-KEM-768/1024 and X25519MLKEM768 hybrid key exchange. AES-128-GCM,
AES-256-GCM (SHA-384 schedule) and
ChaCha20-Poly1305 record protection, negotiated by server preference. TLS 1.3
CCM suites (0x1304/0x1305) are not negotiated: no CCM primitive exists in the
stdlib and no major peer offers them. Stateless session tickets with
`psk_dhe_ke` resumption (ticket plus external-PSK offers) and mTLS
(`verify_client` + `client_cert`/`client_key`) are implemented, plus
KeyUpdate, HelloRetryRequest, ALPN selection, SNI certificates,
post-handshake auth, exporters, SSLKEYLOGFILE, ticket rotation,
Ed25519 (0x0807), record fragmentation and OCSP stapling (transport);
there is no TLS 1.2, no DTLS.

## Build

```sh
./v -d pure_tls your_program.v
```

## Server credentials

Configure both `Config.cert` and `Config.key` (PEM file paths), or provide
`cert_pem` and `key_pem`. The server accepts P-256/P-384/P-521 ECDSA
(SEC1 or PKCS#8 PEM), ML-DSA-44/65/87 (PKCS#8 PEM),
SLH-DSA-SHAKE-128s/128f/192s/192f/256s/256f (PKCS#8 PEM) and RSA (PKCS#1 or
PKCS#8 PEM) leaf certificates and keys, and selects the CertificateVerify
scheme the client offers for the key type (ECDSA: the curve's scheme;
ML-DSA: the variant's scheme 0x0904/0x0905/0x0906; SLH-DSA: the parameter
set's scheme 0x0917/0x0918/0x0919/0x091a/0x091b/0x091c; RSA: PSS preferred
over PKCS#1 v1.5).
If neither is configured, the server creates a temporary self-signed
`localhost` certificate; clients using chain verification will not
trust it unless it is explicitly added to their trust store.

## Session resumption

The server sends one `NewSessionTicket` per connection: an AES-256-GCM box
holding the resumption master secret, so servers stay stateless (the ticket
key is generated per `Listener`). The client captures the ticket on its next
`read()` and passes it as `Config.resume_from` on the next dial; a valid
ticket resumes the session with `psk_dhe_ke` and no certificate flight
(`Conn.resumed()` reports it, `Conn.session_state()` returns the ticket).
Tickets with a bad box, wrong suite, stale age or bad binder fall back to a
full handshake. No early data (0-RTT) is sent.

## Mutual TLS

Set `Config.verify_client` on the server and `Config.client_cert` /
`Config.client_key` (PEM paths or bytes) on the client. The server sends a
`CertificateRequest`; the client answers with its chain and
`CertificateVerify` before its `Finished`. Client certificates verify
against `ca_store`/system roots when `verify_chain` is set (no hostname
check applies to client certificates). A client with no credential sends an
empty `Certificate`, which the server rejects. No `CertificateRequest` is
sent on a resumed (PSK) handshake: the ticket authenticates the peer.

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
- Session resumption accepts ticket plus external-PSK `psk_dhe_ke`
  identities (see above); HelloRetryRequest and KeyUpdate are
  implemented.
- TLS 1.2, DTLS, CCM suites (0x1304/0x1305) — out of scope: no stdlib CCM
  primitive and no major peer offers them.
- SLH-DSA remains on the OpenSSL wrapper until a pure-V SLH-DSA module exists.
- Chain building — v1 verifies the leaf directly against a trusted root only;
  intermediates are ignored. RSA-PSS-signed CA certificates are rejected.
- Signature hashes follow the algorithm OID exactly (no SHA-1/MD5 anywhere);
  ML-DSA signs the raw TBS/content bytes with no pre-hash.

## Interop notes

- `openssl s_client -tls1_3` negotiates all three suites against `net.tls`
  listeners (verified 0x1301/0x1302/0x1303 with chain verification OK). Two
  compat fixes were required: accept record legacy versions 0x0301..0x0303 on
  plaintext records, echo the ClientHello session_id in the ServerHello, and
  ignore middlebox-compat ChangeCipherSpec records during the handshake.
- Baseline record throughput on this box (`-prod`, 200 x 16 KiB records):
  AES-128-GCM seal ~1012 ms / open ~1472 ms, AES-256-GCM seal ~2011 ms /
  open ~1302 ms, ChaCha20-Poly1305 seal/open ~19 ms each. ChaCha20-Poly1305
  is the fastest on machines without AES hardware acceleration.

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

## Timeouts and async I/O

The blocking default is preserved: with all timeouts at zero, `dial()` and
`Conn.read`/`Conn.write` block (the TCP connect itself is still bounded by
the OS, about 5 seconds). `nonblock.v` adds opt-in timeouts without
changing any existing API:

- `TimeoutConfig` pairs a normal `Config` with millisecond timeouts:
  `connect_timeout_ms` (dial wait; can only shorten the OS limit, never
  extend it), `handshake_timeout_ms` (absolute socket deadlines around
  `handshake()`), and `read_timeout_ms` / `write_timeout_ms`
  (steady-state socket timeouts on the live `Conn` via `net.TcpConn`).
- `dial_with_timeouts(host, port, cfg, timeouts)` dials, handshakes and
  applies the steady-state timeouts, closing the half-open connection on
  any failure. Timeout failures carry `net.err_timed_out_code`; use
  `is_timeout(err)` to detect them. The legacy `Conn.set_read_timeout`
  remains a no-op: use `apply_timeouts` / `set_conn_timeouts` and the
  `conn_read_timeout` / `conn_write_timeout` getters instead.
- `dial_async(host, port, tcfg)` runs the same dial in the background and
  delivers exactly one `DialResult` on the returned buffered channel.
  Close `tcfg.cancel` to cancel: the worker checks it between phases and
  reports a cancelled result. Do not close the result channel itself.
- A full async (non-blocking) I/O state machine is out of scope: there is
  no event loop or readiness API, and a handshake already blocked in a
  socket read still runs until `handshake_timeout_ms`.

## Protocol features (RFC 8446)

Beyond the v1 baseline this stack implements:

- KeyUpdate: `Conn.rekey()` rotates the sending traffic secret and sends
  `update_not_requested`. Receiving a KeyUpdate rotates the read secret
  (sequence reset); `update_requested` is answered with our own update.
  Unknown post-handshake types stay ignored.
- HelloRetryRequest: full cookie-less flight on both sides with the
  `message_hash` transcript. The server retries for its most-preferred
  group when the client shared none of its groups.
- ALPN: the server selects its first overlap from
  `Config.alpn_protocols` into EncryptedExtensions;
  `Conn.negotiated_alpn()` exposes it (empty when unnegotiated).
- SNI: `Config.cert_map` maps hostnames to credentials (file or PEM,
  like `cert`/`key`); the server selects on `server_name`, else the
  default credential (fail closed, no downgrade).
- External PSK: `Config.psk_identity`/`psk_key` (bytes, both sides) is
  offered as an extra `psk_dhe_ke` identity; the server matches bytes
  in constant time and verifies that identity's binder.
- Post-handshake auth: `Conn.request_client_auth_post()` sends a
  post-handshake CertificateRequest; the client answers automatically
  on its next read and the server verifies via the mTLS path. Traffic
  keys never change.
- Exporters: `Conn.exporter(label, context, len)` derives from the
  exporter master secret stored at handshake completion.
- SSLKEYLOGFILE: when set, `CLIENT_RANDOM <client_random_hex>
  <master_secret_hex>` lines are appended at handshake completion
  (best-effort, never fails the handshake).
- Ticket rotation: `Config.ticket_keys` seals with `[0]` and tries
  every key on open; `save_ticket_keys`/`load_ticket_keys` persist the
  raw 32-byte concat file (0600 where the OS allows). Empty
  `ticket_keys` keeps the single-key behavior byte-identical.
- Groups: X25519 plus ECDHE P-256/P-384 (via `crypto.ecdsa`), FFDHE
  2048/3072 (RFC 7919 primes, `math.big` modpow), pure ML-KEM-768/1024
  and hybrid X25519+ML-KEM-768 key shares with server preference
  order. Pure group codepoints were measured from OpenSSL 4.1.0
  (`openssl s_client -groups MLKEM512|MLKEM768|MLKEM1024 -msg`):
  supported_groups 0x0200/0x0201/0x0202 with key-share lengths
  800/1184/1568; hybrids 0x11ec (X25519), 0x11eb/0x11ed (P-256/P-384).
- Ed25519: scheme 0x0807 is offered, selected and signed
  (`crypto.ed25519`). Certificate-chain verification of Ed25519 CV
  signatures lives in `cert_verify_sig.v` (separate owner).
- Fragmentation: handshake flights over 2^14 are chunked into
  multiple records on send and reassembled across record boundaries
  on receive (needed for large SLH-DSA flights); small flights keep
  the single-record fast path.
- OCSP: `Config.ocsp_response` (file or PEM bytes, opaque) is
  stapled as CertificateStatus when the client offers
  `status_request`; `Conn.ocsp_response()` returns the raw bytes.
  Transport only — nothing here verifies OCSP.
- Defaults stay wire-identical: no new extensions are offered unless
  configured (the client always offers `status_request` so servers
  with a staple configured can answer; it is ignored otherwise).
