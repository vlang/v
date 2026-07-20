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

- [x] **2a — Initial secrets** (`initial_secrets.v`): RFC 9001 §5.2, fixed
      public salt + HKDF (reuses `crypto.hkdf`, already exists). Also
      implements `hkdf_expand_label` (RFC 8446 §7.1) — the shared derivation
      primitive 2b/2c and Phase 3 will all reuse; do not re-implement it
      elsewhere. Verified against RFC 9001 Appendix A.1's own published test
      vectors (`initial_secret`, `client_initial_secret`,
      `server_initial_secret`, plus chained `quic key`/`quic iv`/`quic hp`
      derivations, obtained from the raw RFC text directly, not
      re-summarized) — exact match, not just internal self-consistency.
      Edge case: keyed off the **original** client DCID, even after a Retry
      switches the wire DCID — covered by
      `test_derive_initial_secrets_is_sensitive_to_which_dcid_is_passed`
      (full `original_dcid`/`current_dcid` tracking still lands in Phase 9's
      `QuicConn`; this sub-phase's test only proves the derivation itself is
      correctly DCID-sensitive).
- [x] **2b — Key schedule** (`tls13_keyschedule.v`): full RFC 8446 §7.1 chain
      (Early → Handshake → Master secret), pinned to `TLS_AES_128_GCM_SHA256`
      only for v1. `derive_secret`/`derive_early_secret`/
      `derive_handshake_secrets`/`derive_application_secrets` cover the
      whole chain through both application traffic secrets_0;
      `exporter_master_secret`/`resumption_master_secret` intentionally
      omitted (v1 uses neither TLS exporters nor 0-RTT — see the doc
      comment on `derive_application_secrets`). HelloRetryRequest's
      synthetic-transcript rule (RFC 8446 §4.4.1) implemented as
      `synthetic_client_hello1_hash`; wiring it into a real running
      transcript is Phase 2c's job. Verified against RFC 8448 §3's own
      published intermediate secrets (Early/Handshake/Master + all 4
      traffic secrets), independent of the RFC 9001 vectors 2a uses —
      including an end-to-end chained test and an independent
      recomputation proving Transcript-Hash covers only raw handshake
      message bytes, no record-layer framing (RFC 8446 §4.4.1 / RFC 9001
      §4), both extracted from the raw RFC text programmatically, not
      hand-transcribed.
- **2c — Messages + state machine** (`tls13_messages.v`, `tls13_handshake.v`):
  ClientHello…Finished, the `quic_transport_parameters` extension (0x39),
  client state machine. In progress — sub-items:
  - [x] Generic handshake message framing (`HandshakeType` enum — the real
        RFC 8446 §B.3 v1.3 set only, TLS-1.2-era RESERVED values correctly
        rejected, not silently accepted as unused variants —
        `encode_handshake_message`/`parse_handshake_message`, 1-byte type +
        3-byte length per RFC 8446 §4). Incremental/resumable by design:
        `parse_handshake_message` peels off exactly one message and reports
        bytes consumed, since QUIC's CRYPTO stream can deliver messages
        split across packets (Phase 4's reassembly job, not this file's).
  - [x] Finished message MAC (RFC 8446 §4.4.4) —
        `compute_finished_verify_data`/`verify_finished`, side-agnostic
        (caller picks client vs. server traffic secret). Uses
        `crypto.hmac.equal` (constant-time) for the peer-supplied
        verify_data comparison, not `==` — Phase-R verified this specific
        wiring by forcing `verify_finished` to always return `true` and
        confirming three negative tests (tampered data, wrong base secret,
        stale transcript checkpoint) all caught it. Verified against an
        RFC 8448 §3 vector extended with EncryptedExtensions/Certificate/
        CertificateVerify bytes (needed to compute the real transcript hash
        the server's Finished authenticates) — first extraction attempt was
        silently corrupted by RFC page-break footer text ("[Page 7]",
        "January 2019" — "20"/"19" parsed as valid hex byte pairs) landing
        inside the Certificate line range; caught via a byte-count mismatch
        (451 extracted vs. 445 octets the RFC itself labels the block) and
        a failed end-to-end HMAC cross-check, both before trusting the
        vector, not after.
  - [x] `quic_transport_parameters` extension inner payload (RFC 9000 §18,
        `transport_parameters.v`) — all 17 §18.2 parameters (including the
        nested `preferred_address` struct), unknown IDs ignored not
        rejected (also exercises the §18.1 "31*N+27" grease pattern),
        `ack_delay_exponent`/`max_udp_payload_size`/`max_ack_delay`/
        `active_connection_id_limit` validity-checked against the spec's
        own stated bounds (accept+reject boundary-pair tests for all 4).
        Duplicate parameter IDs rejected (a defensive addition beyond an
        explicit RFC MUST, Phase-R verified). The u64-space-before-
        truncating-cast pattern from the Phase 1 `header.v` fix is applied
        to the length bounds check here too — this is the first Phase 2
        file that parses a loop of peer-controlled wire bytes, so it's the
        first place that exact bug class could have recurred.
        `initial_source_connection_id` cross-check against the packet
        header's SCID, and the outer TLS extension_type=0x39 + length-
        prefix wrapping (RFC 9001 §8.2), remain: the former needs Phase 9's
        `QuicConn` (owns the packet header), the latter is
        ClientHello/EncryptedExtensions' job below.
  - [x] ClientHello construction (`tls13_client_hello.v`) — legacy_version
        0x0303, empty legacy_session_id (RFC 9001 §8.4 PROHIBITS TLS 1.3
        middlebox compatibility mode over QUIC — a client MUST NOT request
        it, which a non-empty session ID would do), single cipher suite
        (TLS_AES_128_GCM_SHA256), six extensions: server_name,
        supported_versions, supported_groups (secp256r1 only),
        signature_algorithms (ECDSA P-256 + RSA-PSS, matching planned
        CertificateVerify support), key_share (Phase 1's P-256 ECDH
        public key), quic_transport_parameters. Two exact RFC 8448 §3
        cross-checks (supported_versions, and server_name with hostname
        "server" — real sub-structures RFC 8448's own ClientHello happens
        to share byte-for-byte with ours, despite the overall messages
        differing). `/vreview` caught and fixed a real gap here:
        `QuicTransportParameters` deliberately doesn't reject the four
        server-only fields itself (documented as the client-side caller's
        job) — `build_client_hello` is that caller and hadn't actually
        done it, so a caller could have silently produced a ClientHello
        violating RFC 9000 §18.2's "a client MUST NOT include any
        server-only transport parameter." Fixed, Phase-R verified.
  - [x] ServerHello / EncryptedExtensions parsing (`tls13_server_hello.v`).
        Generic `parse_extension_list`/`find_extension` (RFC 8446 §4.2,
        duplicate-extension rejection mirroring
        `transport_parameters.v`'s duplicate-ID rejection).
        `parse_server_hello` returns a `ParsedHelloRetryRequest |
        ParsedServerHello` sum type, distinguished by RFC 8446 §4.1.3's
        magic Random value (independently verified via a live SHA-256
        computation of "HelloRetryRequest", not just transcribed —
        Phase-R verified the discrimination logic itself, not just the
        two happy-path shapes). Validates every statically-checkable RFC
        8446 §4.1.3 MUST (legacy_version, empty legacy_session_id_echo,
        legacy_compression_method, mandatory supported_versions/
        key_share). `parse_encrypted_extensions` rejects early_data
        (0-RTT not offered). Real happy-path test against RFC 8448 §3's
        own ServerHello. `/vreview` caught and fixed a real gap: the
        real-ServerHello key_share branch didn't reject an empty
        key_exchange, even though RFC 8446 §4.2.8's
        `opaque key_exchange<1..2^16-1>` requires at least 1 byte — the
        parallel check already existed for the cookie extension but was
        missed here. Fixed, Phase-R verified.
        Deferred to Phase 2c's still-pending state machine (needs
        connection-level state this parsing layer doesn't have): whether
        cipher_suite/selected_version/key_share group was actually
        offered (v1 only ever offers one of each, so today's fixed-value
        checks already cover the practical case), second-HRR rejection,
        and cross-checking EncryptedExtensions against what was actually
        sent rather than only the unconditionally-wrong early_data case.
  - [x] Certificate / CertificateVerify message parsing
        (`tls13_certificate.v`) — `parse_certificate` (RFC 8446 §4.4.2:
        certificate_request_context + a non-empty chain of
        CertificateEntry, each a non-empty DER cert_data plus its own
        per-entry extensions; v1 is client-only so "must always be
        non-empty" is enforced unconditionally, not deferred to a
        caller-supplied role), `parse_certificate_verify` (RFC 8446
        §4.4.3: algorithm validated against the exact fixed set v1 itself
        offers — a state-free check since that set never varies per
        connection — + signature bytes), and
        `certificate_verify_signed_content` (RFC 8446 §4.4.3's exact
        64-byte-pad + context-string + separator + transcript-hash
        construction, verified byte-for-byte against the RFC's own worked
        example). `/vreview` caught and fixed an over-strict check: this
        file initially rejected a zero-length `signature`, but RFC 8446
        §4.4.3 declares `opaque signature<0..2^16-1>` — zero is
        syntactically legal, unlike `cert_data<1..2^24-1>` and
        `key_exchange<1..2^16-1>` (real minimums of 1, correctly enforced
        elsewhere). No real implementation ever sends an empty signature,
        so this wasn't an active interop break, but it was inconsistent
        with the file's own exact-RFC-fidelity approach — removed, and
        the test now asserts acceptance at that boundary instead.
        **Not yet built** (the actual crypto integration, deliberately
        split out as its own next step): mbedTLS X.509 chain validation
        of the parsed `certificate_list`, and `mbedtls_pk_verify_ext`-based
        CertificateVerify signature verification — **confirm during
        implementation whether mbedTLS's PSS salt-length handling matches
        `rsae` semantics**; if not, fall back to the `rsa_pss` module for
        that one signature scheme. `CertificateRequest` rejected with a
        clear error (no client-cert auth in v1) — not yet implemented,
        belongs with the state machine below since it's about *reacting*
        to a message type, not parsing one we've decided to accept.
  - [ ] Client state machine (`tls13_handshake.v`) wiring the above together
        with transcript accumulation, including second-HelloRetryRequest
        rejection and the TLS-alert-to-QUIC-CONNECTION_CLOSE mapping
        (0x0100-0x01ff, RFC 9001 §4.8).
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
