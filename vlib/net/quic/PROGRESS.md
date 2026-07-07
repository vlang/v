# HTTP/3 (QUIC) implementation progress

Tracking issue: [vlang/v#27675](https://github.com/vlang/v/issues/27675).

This branch (`http3-quic-foundation`) is a work-in-progress foundation for
HTTP/3 support in `net.http`, built on a new QUIC transport (`net.quic`). See
the tracking issue for the full architectural rationale (why TLS 1.3 is
implemented from scratch in V instead of patching vendored mbedTLS, v1 scope
decisions, and the full phase breakdown).

**If you're picking this up:** read the tracking issue first, then this
checklist for exact status. Every checked item has passing tests under
`./vnew` and has been through a `/vreview` pass (see git log for details).

## Phase 0 — Cross-platform build risk (done)

- [x] Audited Windows CI (`windows_ci_gcc.yml`) — already builds+tests
      `crypto.ecdsa` against OpenSSL on all 3 platforms. **Decision:** P-256
      ECDH/RSA-PSS are a hard dependency of `net.quic`, no opt-out flag needed.
      See [../quic/README.md](README.md).
- [x] Verified mbedTLS's X.509 functions work standalone (no
      `mbedtls_ssl_context`) — see `vlib/net/mbedtls/x509_standalone_test.v`.
      Added `mbedtls_x509_crt_verify`/`mbedtls_pk_verify` bindings to
      `vlib/net/mbedtls/mbedtls.c.v` to support this.

## Phase 1 — Primitives (done)

- [x] `varint.v` — QUIC variable-length integer codec (RFC 9000 §16). NOT the
      same encoding as `encoding/leb128` — see the module doc comment.
- [x] `packet_number.v` — packet number encode (RFC 9000 §17.1) + reconstruct
      (Appendix A). A u64-underflow bug in the reconstruction algorithm's
      naive port was caught by the boundary-value tests and fixed.
- [x] `header.v` — long/short header parse+encode, zero-length CIDs, Version
      Negotiation as a distinct packet form.
  - Reserved-bit validation intentionally **deferred to Phase 3**: those bits
    are inside the header-protection-protected region, meaningless until
    protection is removed.
  - Coalesced-packet splitting intentionally **deferred to Phase 4**
    (`coalesce.v`) — `header.v`'s parsers already return bytes-consumed, which
    is the building block that phase needs.
  - Retry Integrity Tag verification intentionally **deferred to Phase 3/4**
    (needs the fixed-key AEAD primitive Phase 3 builds, invoked from Phase 4's
    `retry.v`). `header.v` currently only recognizes the Retry packet type.
- [x] P-256 ECDH added to `vlib/crypto/ecdsa/` (`derive_shared_secret`,
      `uncompressed_bytes`/`from_uncompressed_bytes` wire format helpers).
- [x] New `vlib/crypto/rsa_pss/` module — RSA-PSS sign/verify (no RSA existed
      in V before this).
- [x] `/vreview` pass on Phase 0+1: found and fixed a wire-integer-truncation
      bug in `parse_long_header`'s `token_len` handling (a crafted oversized
      varint silently wrapped past a 32-bit `int` bounds check instead of
      being rejected) and a minor OpenSSL leak in
      `PublicKey.from_uncompressed_bytes`. Both have regression tests.

## Phase 2 — QUIC-scoped TLS 1.3 handshake + key schedule (NOT STARTED)

The largest, highest-risk phase. Sub-phases, in build order:

- [ ] **2a — Initial secrets** (`initial_secrets.v`): RFC 9001 §5.2, fixed
      public salt + HKDF (reuses `crypto.hkdf`, already exists). Fully
      self-contained — no dependency on the state machine. Start here.
      Edge case: keyed off the **original** client DCID, even after a Retry
      switches the wire DCID — needs distinct `original_dcid`/`current_dcid`
      tracking (lands properly in Phase 9's `QuicConn`, but this sub-phase's
      tests should exercise the distinction).
- [ ] **2b — Key schedule** (`tls13_keyschedule.v`): full RFC 8446 §7.1 chain
      (Early → Handshake → Master secret), pinned to `TLS_AES_128_GCM_SHA256`
      only for v1. Transcript hash covers handshake message bytes only — no
      TLS record-layer framing exists in QUIC. Support HelloRetryRequest.
- [ ] **2c — Messages + state machine** (`tls13_messages.v`,
      `tls13_handshake.v`): ClientHello…Finished, the `quic_transport_parameters`
      extension (0x39), client state machine. Use mbedTLS's `mbedtls_pk_verify_ext`
      for CertificateVerify validation (reserve the new OpenSSL P-256 binding
      for ECDHE only) — **confirm during implementation whether mbedTLS's PSS
      salt-length handling matches `rsae` semantics**; if not, fall back to the
      `rsa_pss` module for that one signature scheme.
- [ ] Author an RFC-8448-style TLS 1.3 test vector suite from scratch
      (`vlib/net/quic/testdata/tls13_vectors/`) — RFC 8448 itself is non-QUIC
      TLS 1.3 and not directly reusable. Capture from a reference client/server
      (ngtcp2/quiche) via QUIC key-log export, cross-check independently before
      trusting any fixture, document provenance (tool/version/date).

## Phases 3-14 (NOT STARTED)

See the tracking issue for full detail on each. In order:

3. Packet protection & header protection (RFC 9001 §5) — the AEAD
   encrypt-then-mask ordering is the single most common bug class here.
4. Initial packet exchange — CRYPTO frame reassembly, core frames, datagram
   coalescing, Retry/Version Negotiation handling.
5. Full handshake completion — three independent packet number spaces (a
   common implementation mistake to conflate), key discard, key update.
6. Stream layer — STREAM frames, connection+stream flow control interplay.
   Note: even client-only v1 must receive server-initiated uni streams from
   day one (HTTP/3's control/QPACK streams need this).
7. Loss detection & NewReno congestion control (RFC 9002).
8. Connection lifecycle — idle timeout, CONNECTION_CLOSE, stateless reset,
   ECN fallback, PMTU (pinned to 1200 bytes for v1).
9. `QuicConn` — top-level struct, `poll()`/`process_timeouts()` event-loop
   contract.
10. HTTP/3 framing (RFC 9114) — incremental/resumable parsing (structurally
    different from HTTP/2's single-shot framing).
11. QPACK (RFC 9204) — absolute indexing, encoder/decoder streams, blocked-
    stream handling. Not HPACK — don't reuse `h2_hpack_static.v`.
12. HTTP/3 client wiring into `Request`/`Response`/`Transport`.
13. Server support — explicitly out of committed scope, but Phases 1-9 are
    designed to need no rework for it (`role` field already present).
14. 0-RTT — explicitly out of committed scope.

## Scope decisions in effect (see tracking issue for rationale)

- Client first, server is a later phase.
- Congestion control: NewReno, not CUBIC.
- 0-RTT deferred.
- Single-threaded, caller-driven event loop (`poll()`/`process_timeouts()`),
  not a background thread per connection — matches V's lack of native
  async I/O and QUIC's one-socket-many-connections-by-CID model.

## Validation workflow (apply to every new phase)

- Build `./vnew` (not `./v`) and run all tests through it.
- Every new file gets a paired `_test.v`.
- Run `/vreview` on the diff before committing — it's caught a real bug in
  every phase so far (see Phase 0+1 above). Full-file read is mandatory for
  new files, not just a diff scan.
- Format with `./vnew fmt -w <file>` before committing.
