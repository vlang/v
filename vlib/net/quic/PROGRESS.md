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
- [x] ~~New `vlib/crypto/rsa_pss/` module — RSA-PSS sign/verify (no RSA existed
      in V before this).~~ Removed again as unused dead code once mbedTLS's
      already-vendored `mbedtls_pk_verify_ext` was confirmed to cover
      RSA-PSS verification (CertificateVerify) with no OpenSSL dependency,
      matching README.md's own note (Codex P3, vlang/v#27680
      pullrequestreview-4806500473: this entry was left marked complete
      after the module's removal, pointing contributors at a module that no
      longer exists).
- [x] `/vreview` pass on Phase 0+1: found and fixed a wire-integer-truncation
      bug in `parse_long_header`'s `token_len` handling (a crafted oversized
      varint silently wrapped past a 32-bit `int` bounds check instead of
      being rejected) and a minor OpenSSL leak in
      `PublicKey.from_uncompressed_bytes`. Both have regression tests.

## Phase 2 — QUIC-scoped TLS 1.3 handshake + key schedule (done)

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
      Edge case: `derive_initial_secrets` is DCID-agnostic and MUST be
      RE-DERIVED from the CURRENT wire DCID whenever it changes — after a
      Retry, that means the Retry packet's Source Connection ID, not the
      client's original DCID (RFC 9001 §5.2: "The secrets used for
      constructing subsequent Initial packets change when a server sends a
      Retry packet"). This doc previously stated the opposite (keyed off
      the original DCID forever); corrected after a Codex finding
      (vlang/v#27680 pullrequestreview-4783410111) caught this file still
      contradicting the already-corrected `initial_secrets.v` doc comment —
      covered by
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
- [x] **2c — Messages + state machine** (`tls13_messages.v`, `tls13_handshake.v`):
  ClientHello…Finished, the `quic_transport_parameters` extension (0x39),
  client state machine. Sub-items:
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
  - [x] mbedTLS X.509 chain validation, standalone (no `mbedtls_ssl_context`,
        same discipline as Phase 0). New `net.mbedtls` public API
        (`x509_standalone.c.v`: `build_certificate_chain`/
        `verify_certificate_chain`/`free_certificate_chain`) rather than
        `net.quic` reimplementing C bindings itself — matches how
        `net.http`'s TLS clients already depend on `net.mbedtls` instead
        of duplicating it. `net.quic`'s own
        `verify_server_certificate_chain` (`tls13_certificate_chain.v`)
        converts a `ParsedCertificate` into DER blobs and wraps the
        result. Every C-interop calling convention was verified against
        mbedTLS's actual vendored source (`x509_crt.c`), not memory or
        assumption: DER certs need their EXACT length (not the +1
        NUL-terminator convention this module's PEM helpers use — that
        would be a real out-of-bounds read one byte past a V slice, not a
        harmless extra byte, though this specific detail is verified by
        source inspection only since mbedTLS's DER parser tolerates a
        too-long buflen for well-formed input and no test can observe the
        difference); `mbedtls_x509_crt_parse_der` always copies its input
        (no dangling pointer back into caller-owned memory); repeated
        parse calls on the same chain correctly append via a documented
        walk-to-tail-and-link algorithm (also source-verified only — this
        codebase has one real test cert fixture, so no functional test yet
        exercises an actual 2+-certificate chain).
        `/vreview` caught and fixed two real issues: (1)
        `VerifiedCertificateChain.free()` was double-free-prone on a
        second call — the exact class of bug `SSLConn.shutdown()` already
        guards against with a documented comment, a sibling I should have
        checked before writing this; fixed with a `mut` receiver that
        nulls the pointer after freeing (also only reasoning-verified: a
        double-free of this size doesn't reliably crash on this
        platform's allocator, so no test can prove the difference
        either). (2) `verify_certificate_chain`'s CA-bundle parse check
        used `parse_ret < 0`, inconsistent with every other PEM-parsing
        call site in `net.mbedtls` (`!= 0`) — a real gap, since
        `mbedtls_x509_crt_parse` can return a *positive* count of
        certs that failed to parse within an otherwise-valid PEM bundle;
        `< 0` would have silently accepted that. Fixed to `!= 0`.
        **Resolves the plan's flagged open question about mbedTLS's PSS
        salt-length semantics, ahead of building the code that needs it**:
        checked the vendored `mbedtls_config.h` — `MBEDTLS_USE_PSA_CRYPTO`
        is disabled (commented out, no project override), so
        `mbedtls_pk_verify_ext`'s documented "salt length not verified
        under PSA crypto" caveat does not apply to this vendored build;
        setting `mbedtls_pk_rsassa_pss_options.expected_salt_len` to the
        exact digest output length (32/48/64 for SHA-256/384/512) will
        correctly enforce RFC 8446 §4.2.3's `rsa_pss_rsae_*` requirement
        ("salt length MUST be equal to the length of the output of the
        digest algorithm"). No fallback to the `rsa_pss` module needed.
  - [x] `mbedtls_pk_verify_ext` CertificateVerify signature verification.
        New `v_mbedtls_x509_crt_get_pk` C shim (`mbedtls_helpers.h`,
        following the established pattern) safely extracts the leaf
        certificate's embedded public key without hand-replicating
        mbedTLS's struct layout — `mbedtls_x509_crt.pk` isn't
        `MBEDTLS_PRIVATE`-wrapped, but the surrounding struct is still kept
        opaque on the V side, so the shim is the one place that touches the
        real field, resolved by the real C compiler. `net.mbedtls`
        (`x509_standalone.c.v`) gained `get_leaf_public_key`/
        `verify_ecdsa_signature`/`verify_rsa_pss_signature`; `net.quic`
        (`tls13_certificate_chain.v`) gained
        `VerifiedCertificateChain.verify_certificate_verify_signature`,
        dispatching `ParsedCertificateVerify.algorithm` to the matching
        digest (SHA-256/384/512) and mbedTLS call, feeding
        `certificate_verify_signed_content`'s already-tested RFC 8446
        §4.4.3 construction. `mbedtls_pk_rsassa_pss_options` is hand-
        replicated (not a C shim) — confirmed via generated-C inspection
        that V emits it as a designated initializer (`.field = value`),
        so the real vendored struct layout resolves it, not a V-side
        guess. RSA-PSS salt length is pinned to the exact digest length
        (not `MBEDTLS_RSA_SALT_LEN_ANY`) per RFC 8446 §4.2.3; confirmed via
        `rsa.c` source read that the check is real (not a PSA-crypto no-op,
        `MBEDTLS_USE_PSA_CRYPTO` is disabled in this build) and matches
        mbedTLS's own `ssl_tls13_generic.c` TLS 1.3 code doing the
        identical thing. Tested with a **genuine RSA-PSS sign+verify round
        trip** (`net.mbedtls/x509_standalone_signature_test.v`) using this
        codebase's existing self-signed test cert's matching private key —
        real cryptography, not reasoning about C-binding correctness — for
        all three hash sizes, plus corrupted-signature and wrong-message
        rejection. No EC private key exists anywhere in this repo, so the
        ECDSA path is tested only via rejecting an incompatible (RSA-typed)
        key, documented as an honest coverage gap rather than skipped
        silently. `/vreview` caught and fixed one real bug: calling
        `verify_certificate_verify_signature` on an already-`free()`d chain
        dereferenced a garbage near-null pointer (UB) rather than erroring
        cleanly — `free()` nulls the chain pointer, and the C shim's
        pointer arithmetic on a null `crt` produces a small non-null
        "pointer" that only crashes once something reads through it.
        Phase-R confirmed this is a real, reliable segfault (not
        theoretical) via an isolated throwaway probe before applying the
        fix. Fixed with an explicit nil guard + a permanent regression
        test. `CertificateRequest` rejection (no client-cert auth in v1)
        remains deferred to the state machine below, since it's about
        *reacting* to a message type, not verifying one we've already
        decided to accept.
  - [x] Client state machine (`tls13_handshake.v`) — happy path complete:
        `Tls13ClientHandshake.start` (ephemeral ECDHE keygen + ClientHello)
        through `process_server_hello`/`process_encrypted_extensions`/
        `process_certificate_or_request`/`process_certificate_verify`/
        `process_finished`, deriving Handshake and Application secrets at
        the correct transcript checkpoints and returning the client's own
        framed Finished message. `TlsAlert` + `tls_alert_to_quic_error`
        implement the RFC 9001 §4.8 mapping (0x100 + alert description);
        every fatal path goes through `handshake_error`, which attaches
        the mapped code via `error_with_code` so a future caller can read
        it via `.code()` when building CONNECTION_CLOSE.
        Second-HelloRetryRequest rejection is implemented (RFC 8446
        §4.1.4); a **first** HelloRetryRequest is not — it needs
        `build_client_hello` to speak a cookie extension it doesn't yet,
        and RFC 8446 §4.4.1's synthetic `message_hash` transcript
        substitution, deliberately deferred rather than half-built, and
        reported as an explicit "not yet implemented" error rather than
        silently mishandled. `CertificateRequest` is rejected outright
        (no client-cert auth in v1), including the RFC 8446 §4.4.2
        `certificate_request_context`-must-be-empty check for the
        Certificate case (a `/vreview` finding — parsed but unchecked
        before the fix).
        Tested end-to-end with a **genuine fake TLS 1.3 server**
        (`tls13_handshake_test.v`): real ECDHE (Phase 1's OpenSSL P-256
        binding), real RSA-PSS CertificateVerify signing (mbedTLS, same
        approach as `x509_standalone_signature_test.v`), real Finished
        HMACs computed via this codebase's own key-schedule functions
        acting as an independent "other side" — the fake server verifies
        the client's own returned Finished, proving both sides agree, not
        just that the client didn't crash. Certificate CHAIN TRUST is not
        exercised end-to-end in this test (this repo has no CA-flagged
        test certificate, the same limitation as Phase 2c part 6's own
        tests) — one test drives to Certificate and asserts the expected
        "not a CA" failure is propagated correctly; a second test installs
        an already-verified chain directly (the same white-box technique
        `tls13_certificate_chain_test.v` uses) to test CertificateVerify/
        Finished independent of that gap.
        `/vreview` caught and fixed two real bugs: (1) `free()` had no
        idempotency guard — `VerifiedCertificateChain.free()` nulls its
        own pointer so a second call safely no-ops, but
        `ecdsa.PrivateKey.free()` has no equivalent, so a second `free()`
        call double-freed the ephemeral ECDHE key. Phase-R confirmed via
        an isolated probe that this is a **real, reliably reproducible
        crash** (OpenSSL's `EVP_PKEY_free` aborts on a double-free,
        unlike the mbedTLS double-free cases discussed elsewhere in this
        file, which don't reliably crash) — fixed with a `freed bool`
        guard matching `SSLConn.shutdown()`'s pattern. (2) the
        `certificate_request_context` gap described above. Both have
        permanent regression tests, confirmed to fail on the pre-fix code
        via Phase-R before the fix landed.
- [x] Author an RFC-8448-style TLS 1.3 test vector suite from scratch
      (`vlib/net/quic/testdata/tls13_vectors/`) — a real handshake captured
      from Cloudflare quiche (`cloudflare/quiche-qns:latest`, pinned by
      digest, via quic-interop-runner's published image reference — not
      guessed), running in Docker: client + server + `tcpdump` on a bridge
      network, `SSLKEYLOGFILE` set (an undocumented but functional env var
      for quiche, confirmed empirically since neither `quiche-client
      --help` nor `quiche-server --help` mentions it), decrypted and
      dissected with `tshark` 4.6.6 using the keylog. `extract_handshake.py`
      reconstructs each handshake message's exact raw bytes from tshark's
      PDML tree — cross-checked against tshark's own independently-reported
      per-message size for every message (the extraction script itself had
      two real bugs, both caught by this check before anything was
      trusted: Wireshark's tree shows some fields' bytes twice, once raw
      and once under a friendly-named alias covering the identical span;
      and a field with both its own raw value AND a child annotation node
      needs its own value captured, not skipped by recursing past it).
      Directory structure follows this repo's own established
      `crypto/blake2b/testdata/` convention (README + raw fixture +
      generation script, real test lives in the parent directory as a
      normal `_test.v` file) rather than inventing a new one.
      `tls13_quiche_vector_test.v` parses every real captured message
      (ClientHello, ServerHello, EncryptedExtensions, Certificate,
      CertificateVerify, both directions' Finished) with this module's own
      production functions and cross-validates what a standard keylog
      capture can actually prove (documented precisely in the README,
      since it's a real, non-obvious boundary): message **parsing** against
      an independent implementation's real wire bytes (not hand-constructed
      or self-round-tripped, a genuinely new form of coverage); a REAL
      ECDSA P-256 CertificateVerify signature verifying successfully —
      closing the exact gap Phase 2c's own signature-verification work
      documented (no EC private key exists anywhere in this repo, so
      `net.mbedtls/x509_standalone_signature_test.v` could only exercise
      ECDSA via a key-type-mismatch rejection, never a genuine accepted
      signature); and both directions' real Finished MACs, using the
      keylog's real `CLIENT_HANDSHAKE_TRAFFIC_SECRET`/
      `SERVER_HANDSHAKE_TRAFFIC_SECRET` against the real captured Finished
      bytes. **Not validated by this capture** (documented honestly, not
      silently skipped): the Early Secret → Handshake Secret → traffic-
      secret HKDF chain itself, since a standard keylog exports only the
      already-derived traffic secrets, not the raw ECDHE shared secret —
      that chain is already independently cross-checked against RFC 8448's
      own official worked values in `tls13_keyschedule_test.v`, a
      different but equally valid form of independent validation.

## Phase 3 — Packet protection and header protection (done)

- [x] `packet_protection.v` — `QuicPacketProtectionKeys` (quic_key/quic_iv/
      quic_hp, RFC 9001 §5.1) derived via `hkdf_expand_label` (the same
      primitive Phase 2a/2b already use) from any one level's one-directional
      traffic secret; verified against the RFC 9001 Appendix A.1 vectors
      already used directly against `hkdf_expand_label` in
      `initial_secrets_test.v` (duplicated locally since V compiles each
      `_test.v` file as its own independent unit — top-level consts aren't
      shared across sibling test files in the same module).
      `encrypt_packet_payload`/`decrypt_packet_payload` wrap
      `crypto.aes.AesGcm`; the AEAD nonce XORs the packet's FULL,
      RECONSTRUCTED packet number (never the truncated wire bytes) into the
      low 8 bytes of the 12-byte IV. `protect_packet`/`unprotect_packet`
      combine packet + header protection in the one correct order (encrypt
      payload → sample ciphertext → derive mask → apply to header on the
      send side; unprotect header first, since the packet number's length is
      itself protected, → AEAD-decrypt on the receive side) so a future
      caller can't accidentally sequence the two steps backwards — the
      single most common bug class in this area.
- [x] `header_protection.v` — AES-ECB mask derivation (RFC 9001 §5.4.3,
      the only construction v1 needs, since `TLS_AES_128_GCM_SHA256` is
      pinned throughout); sample always taken at a fixed 4-byte offset past
      the packet-number field regardless of that field's real (still-
      protected) length, per RFC 9001 §5.4.2. `unprotect_header` validates
      the Reserved Bits are zero once unmasked (RFC 9000 §17.2/§17.3.1 MUST —
      noted as intentionally deferred to this phase back in Phase 1's
      `header.v` entry above) and returns a plain error for a receiver to
      map to PROTOCOL_VIOLATION, distinct from an AEAD auth failure (which
      callers must silently drop, never escalate to a connection close, per
      RFC 9001's own security guidance — documented on
      `decrypt_packet_payload`, enforced by a future phase's receive loop,
      not this one).
- [x] `/vreview` pass: found and fixed two gaps before commit — (1) the
      missing Reserved Bits check above (RFC 9001/9000 MUST, no diff-driven
      review would find an absent check without a requirements-driven read);
      (2) `packet_protection_nonce` indexed its `iv` parameter without
      validating its length, unlike the sibling `hp_key.len` check
      `header_protection_mask` already has on the very same
      `QuicPacketProtectionKeys` struct (whose fields are all `pub` and
      externally constructible, not only ever produced by
      `derive_packet_protection_keys`) — a too-short `iv` panicked (index
      out of range) instead of returning a graceful error. Both fixed with
      permanent regression tests, confirmed to fail on the pre-fix code via
      Phase-R before landing.
- [x] Known-answer test against a REAL captured packet: the very first UDP
      datagram (`frame 1`, a single non-coalesced Client Initial, 1200 bytes)
      from the SAME quiche capture Phase 2c's TLS-layer vectors came from
      (`testdata/tls13_vectors/quiche_p256_handshake.pcap`), extracted with a
      minimal standalone pcap/UDP parser (no Wireshark dependency needed
      here, since only raw bytes are wanted, not TLS dissection). Initial
      secrets are derived purely from the packet's own (always-visible)
      DCID; after removing header protection and AEAD-decrypting, the
      plaintext is checked for containing the EXACT ClientHello bytes
      already independently verified in `tls13_quiche_vector_test.v` — the
      SAME bytes obtained via a completely different extraction path there
      (tshark/PDML TLS dissection vs. this file's raw UDP parsing), a small
      independent cross-check that neither extraction made the same
      mistake. A companion negative test flips one bit in the same real
      packet's ciphertext and confirms AEAD authentication fails cleanly.
      Fulfills the plan's own suggested Phase 3 test strategy ("known-answer
      tests against Phase 2's captured Initial packets") without needing a
      fresh capture.
- [x] Self-consistency round-trip tests across all 4 packet-number lengths
      and both header forms (long/short), plus negative tests for tampered
      ciphertext and for decrypting with the wrong direction's keys.

## Phase 4 — Initial packet exchange (done)

- [x] `frame.v` — PADDING/PING/ACK/CRYPTO/CONNECTION_CLOSE parsing and
      encoding (RFC 9000 §19), scoped to exactly the frame types legal in
      the Initial/Handshake packet number spaces (§12.4 Table 3). ACK's
      Gap/ACK Range Length wire encoding is resolved into already-computed
      `[smallest, largest]` ranges; the gap math was verified against a
      hand-derived numeric example (not just a self round trip, which
      can't catch a bug consistently wrong in both directions) with the
      exact expected wire bytes hardcoded. Every other frame type (STREAM,
      MAX_DATA, ...) reports "not yet implemented" rather than a
      wire-format error, since they're real, valid QUIC frames just
      deferred to later phases.
- [x] `crypto_stream.v` — per-encryption-level CRYPTO frame reassembly,
      tolerating out-of-order arrival and overlapping retransmissions (RFC
      9000 §19.6); a content mismatch on any overlap — including between
      two not-yet-promoted out-of-order fragments that overlap each other
      before either touches the contiguous stream — is rejected at the
      point of overlap via one shared validated-append path, rather than
      surfacing later as a confusing transcript-hash/Finished-MAC failure.
- [x] `coalesce.v` — datagram splitting by walking long-header `Length`
      fields; stops cleanly (not as a fabricated bogus packet) at a short
      header, a Version Negotiation packet, a Retry packet, or trailing
      non-packet padding (see the `/vreview` finding below).
      `pad_initial_payload` pads a sender's own Initial packet to the RFC
      9000 §14.1 1200-byte minimum via PADDING frames INSIDE the
      AEAD-protected payload (§14.1's primary mechanism), not raw bytes
      appended after protection -- see the Codex-round fixes below. A
      trailing chunk that fails to parse as a legitimate next packet
      (truncated header, unsupported version, or an overrun Length field)
      is discarded and splitting stops there -- RFC 9000 §14.1's own
      "coalesced with invalid packets, which a receiver will discard"
      allowance -- rather than failing every packet already validated
      before it. A subsequent long-header packet whose DCID doesn't match
      the datagram's first packet is likewise excluded from the result,
      not treated as an error (RFC 9000 §12.2, SHOULD), and scanning
      continues past it for whatever legitimately-addressed packet may
      follow.
      `parse_frames` (frame.v) rejects a packet payload containing zero
      frames as PROTOCOL_VIOLATION (RFC 9000 §12.4) -- `parse_frame`
      (singular) already rejected an empty buffer, but the plural
      reassembly loop's own `for offset < buf.len` guard never even
      called into it for a genuinely empty payload.
- [x] `retry.v` — client-side Retry Integrity Tag (RFC 9001 §5.8)
      compute/verify, using AEAD_AES_128_GCM over an empty plaintext with a
      FIXED public key/nonce (not derived from the connection's own
      secrets). The fixed key/nonce were confirmed against two independent
      sources before trusting them: RFC 9001 §5.8's own text, and
      Cloudflare quiche's Rust source (`RETRY_INTEGRITY_KEY_V1`/
      `RETRY_INTEGRITY_NONCE_V1` in `packet.rs`) — the same reference
      implementation this module's TLS 1.3 test vectors were captured
      from. An invalid tag returns `false` (discard the packet silently),
      never an error — an off-path forger is exactly what this check
      exists to catch, so treating a bad tag as fatal would hand that
      forger a way to abort a legitimate handshake in progress.
      Tracking "at most one Retry per connection attempt" (RFC 9000
      §17.2.5.2) is documented as Phase 9 `QuicConn` state, since this
      module is a stateless verification primitive.
- [x] `version_negotiation.v` — a VN packet listing v1 itself MUST be
      silently discarded (RFC 9000 §6.2), not treated as a protocol
      violation: the connection attempt continues unchanged. A VN packet
      without v1 fails the connection attempt cleanly, since this client
      implements only v1 with no lower-version fallback. Before either
      check runs, the VN packet's DCID must echo the client's own original
      SCID (RFC 9000 §17.2.1); a mismatch is discarded as unauthenticated/
      spoofed rather than treated as a genuine response to this client's
      Initial packet, mirroring `retry.v`'s analogous anti-spoof CID check.
- [x] Integration test (`initial_exchange_test.v`): a full simulated
      Initial round trip tying Phases 2+3+4 together — a real ClientHello
      (Phase 2), real CRYPTO framing, real packet+header protection (Phase
      3), "transmitted" over a plain `[]u8` fake transport, then fully
      reversed on the receive side ending with the reassembled CRYPTO
      stream reproducing the exact original ClientHello bytes and
      re-parsing as a valid handshake message. Plus a tampered-datagram
      negative test.
- [x] `/vreview` pass: found and fixed three gaps before commit —
      (1) `split_coalesced_datagram` misinterpreted trailing raw
      UDP-datagram-level zero-byte padding (the shape a real Initial
      datagram padded to 1200 bytes has -- this client's own outgoing
      padding no longer produces this shape, see the Codex-round fixes
      below, but a received datagram from another implementation must
      still tolerate it) as a bogus additional coalesced packet, since
      neither `parse_long_header` nor `parse_short_header` (Phase 1,
      `header.v`) validated RFC 9000's Fixed Bit (0x40) — caught by the
      integration test above, then confirmed against the REAL captured
      server datagram from Phase 3's testdata, which turned out to have
      been misread as 3 packets instead of 2 real ones + trailing padding
      (an earlier, less careful reading of that same capture had assumed
      the third "packet" was genuine); (2) `parse_ack_frame` sized a `cap:`
      allocation hint directly off the attacker-controlled, unvalidated
      `ack_range_count` varint (up to 2^62-1) before confirming the buffer
      could plausibly contain that many ranges — a real DoS vector from a
      single small ACK frame; (3) `CryptoStreamReassembler` bounded the
      byte-offset range a fragment may claim but not the NUMBER of distinct
      out-of-order fragments that can accumulate within that range. All
      three have regression tests, Phase-R-verified to fail on the pre-fix
      code (the ACK allocation test needed a second iteration after its
      first chosen value, 2^40, happened to wrap to exactly 0 when narrowed
      to a 32-bit `int` and so accidentally exercised a harmless
      allocation regardless of whether the fix was present).
- [x] Codex review (vlang/v#27880, pullrequestreview-4836332922) found 9
      more gaps after the post-#27680-merge rebase, all fixed with
      regression tests, Phase-R-verified against the pre-fix code:
      `handle_version_negotiation` treated a VN packet listing v1 as a
      hard PROTOCOL_VIOLATION instead of the RFC 9000 §6.2-mandated
      silent discard (an unauthenticated VN packet with a trivial
      connection-kill primitive for an off-path attacker, if left as an
      error); Initial-packet padding moved from raw trailing datagram
      bytes to PADDING frames inside the AEAD-protected payload (RFC 9000
      §14.1's primary mechanism -- `pad_datagram_for_initial` removed,
      replaced by `pad_initial_payload`); `QuicRetryPacket`'s `dcid`/`scid`
      doc comments had the server's-new-CID label on the WRONG field
      (RFC 9000 §17.2.5.1: it's `scid`, not `dcid`); `parse_retry_packet`
      now also rejects a zero-length Retry Token (§17.2.5.2) and a Retry
      whose SCID equals the client's own Initial DCID (§17.2.5.1, an
      anti-degenerate-loop check found while verifying the other two Retry
      findings against the primary RFC text, not from Codex);
      `CryptoStreamReassembler.add` now deduplicates a retransmitted
      out-of-order fragment already covered by an existing pending one,
      instead of counting ordinary loss-recovery retransmissions against
      the distinct-fragment cap; `encode_crypto_frame`/`parse_crypto_frame`
      now reject offset+length exceeding the 2^62-1 varint limit (RFC 9000
      §19.6, confirmed verbatim); `scaled_ack_delay_micros` now saturates
      instead of silently wrapping a u64 left-shift overflow (a legal wire
      ACK Delay with a legal §18.2-maximum exponent of 20 shifts past bit
      64); `split_coalesced_datagram` now rejects a Version Negotiation
      packet coalesced after another packet (RFC 9000 §12.2: "there is no
      situation where a Retry or Version Negotiation packet is coalesced
      with another packet"). All RFC citations independently verified
      against the primary rfc-editor.org text (cached locally at
      `.claude/skills/code-review/rfc-texts/`, gitignored) rather than
      trusted from the review comments alone.

## Phase 5 — Full handshake completion (done)

- [x] `packet_number_space.v` — formalizes the THREE INDEPENDENT packet
      number spaces (Initial/Handshake/Application Data), flagged as the
      most common implementation mistake to get wrong (treating packet
      numbers as connection-global breaks ACK-frame interop with any
      compliant peer). `PacketNumberSpaceState` tracks per-space
      next-to-send/largest-received/largest-acked-by-peer, feeding directly
      into Phase 1's `encode_packet_number`/`decode_packet_number` with no
      adaptation. `QuicPacketNumberSpaces` groups the three as genuinely
      independent struct fields — no shared mutable state to accidentally
      conflate.
- [x] `handshake_confirm.v` — models "complete" (own Finished sent AND
      peer's Finished verified) and "confirmed" (HANDSHAKE_DONE received)
      as two distinct checkpoints, each with its own key-discard trigger
      (RFC 9001 §4.9.1/§4.9.2), plus a third, independent checkpoint for
      discarding Initial keys (first Handshake-space packet sent). The
      alternate ack-based confirmation path RFC 9001 permits is
      deliberately not implemented — v1 always waits for HANDSHAKE_DONE.
- [x] `key_update.v` — 1-RTT-only key phase bit rotation (RFC 9001 §6),
      receive side only (client-initiated rotation deferred, per plan).
      `resolve_read_keys` decides which keys to try decrypting an incoming
      packet with — matching the current phase, the retained previous
      phase (a reordered pre-update packet), or a freshly-derived next
      phase (a genuine new update) — purely from the packet's phase bit
      and packet number, per RFC 9001 §6.5, and never mutates state or
      authenticates anything itself. `note_successful_decrypt` commits the
      outcome only after the caller's own AEAD decryption has actually
      succeeded. `max_key_updates_accepted` is a coarse, time-independent
      cap standing in for RFC 9001 §6.1/§6.5's ack-plus-3xPTO pacing, which
      needs RTT/PTO estimation this module doesn't have yet (Phase 7).
- [x] `/vreview` pass: found and fixed one gap before commit —
      `note_successful_decrypt` trusted `resolution.is_new_update`/
      `is_previous_phase` at face value, but those flags are computed at
      `resolve_read_keys` time and go stale if a caller resolves more than
      one packet (e.g. two packets from the same coalesced datagram)
      before committing either; committing the first packet's genuine
      update flips the current phase, and a second, now-stale
      `is_new_update` resolution for a packet that actually matches the
      just-updated phase would otherwise be mis-committed as a SECOND
      update, permanently desynchronizing decryption. Fixed by having
      `note_successful_decrypt` re-derive its classification fresh from
      the packet's own real phase bit against current state, rather than
      trusting resolve-time flags — correct regardless of how many
      resolutions were computed before any commit. Has a regression test,
      Phase-R-verified to fail on the pre-fix code.

## Phase 6 — Stream layer and flow control (done)

- [x] `frame.v` extended — STREAM (0x08-0x0f, OFF/LEN/FIN bits), RESET_STREAM,
      STOP_SENDING, MAX_DATA, MAX_STREAM_DATA, MAX_STREAMS (bidi/uni),
      DATA_BLOCKED, STREAM_DATA_BLOCKED, STREAMS_BLOCKED (bidi/uni). A
      length-less STREAM frame (LEN bit clear) correctly consumes the rest
      of `parse_frames`' buffer, matching RFC 9000 §19.8's requirement that
      it be the last frame in its packet — a natural consequence of the
      wire format itself, not something requiring separate enforcement.
- [x] `stream.v` — `StreamId` category derivation (RFC 9000 §2.1),
      `QuicRole`-aware `is_locally_initiated`, `SendStreamState`/
      `RecvStreamState` (RFC 9000 §3.1/§3.2) driven by local actions and
      frame arrival respectively (ACK-driven and application-read-driven
      transitions are documented hooks for Phase 7/9, not implemented
      here). `QuicStream.send`/`recv` are nilable pointers (`&StreamSendHalf`/
      `&StreamRecvHalf`, matching `Tls13ClientHandshake.verified_chain`'s
      established convention) so every caller mutates the SAME shared half
      directly — see the `/vreview` finding below for why this replaced an
      earlier Optional-value design. `QuicStreamSet.get_or_create` auto-creates
      peer-initiated streams (including every lower-numbered stream in the
      same category, per RFC 9000 §2.1) while enforcing the caller-supplied
      `max_streams` limit (STREAM_LIMIT_ERROR) and refusing to fabricate a
      locally-initiated stream just because a frame references it
      (STREAM_STATE_ERROR); `open_local_stream` is the send-side mirror,
      allocating sequential IDs per category.
- [x] `stream_reassembly.v` — per-stream offset-ordered reassembly, mirroring
      Phase 4's `crypto_stream.v` design (validated-append + promote_ready,
      tolerating out-of-order arrival and overlapping retransmissions),
      extended with `note_final_size` reconciling a stream's final size
      (from a FIN-carrying STREAM frame or RESET_STREAM) against everything
      already received or buffered (FINAL_SIZE_ERROR on mismatch, RFC 9000
      §4.5) — the one genuine difference from CRYPTO streams, which have no
      final-size concept.
- [x] `flow_control.v` — `FlowControlWindow` (send-side accounting against a
      peer-raised limit) and `ReceiveWindow` (receive-side accounting with
      an auto-growth heuristic: advertise a higher limit once the
      application has consumed at least half the current window, avoiding
      a throughput stall). `initial_send_limit_for_stream`/
      `initial_receive_limit_for_stream` resolve RFC 9000 §4.1's
      easy-to-invert peer-relative transport-parameter naming
      (`initial_max_stream_data_bidi_local`/`_remote` mean opposite things
      depending on whose parameters and which side of the stream you're
      asking about) in one place, verified against a hand-derived worked
      example for all 4 stream categories from the client's own
      perspective, not just structurally.
- [x] Integration test (`stream_layer_test.v`): three streams — a
      client-opened bidi stream, a server-opened uni stream (the plan's own
      "even client-first phase must receive server-initiated unidirectional
      streams from day one"), and a second client-opened bidi stream — with
      STREAM frames delivered genuinely interleaved (not grouped by stream),
      each independently reassembled while one connection-level
      `ReceiveWindow` tracks the running total across all three.
- [x] `/vreview` pass: found and fixed one gap before commit —
      `QuicStream.send`/`recv` were originally Optional VALUE fields
      (`?StreamSendHalf`/`?StreamRecvHalf`); unwrapping via `s.recv or
      {...}` copies the struct out, so mutating the copy via
      `note_data()`/`note_size_known()` looks like in-place mutation but
      silently doesn't persist unless the caller remembers to explicitly
      reassign `s.recv = recv` afterward (the reassembler's own data
      survives regardless, via its internal pointer field, but `state`/
      `final_size` would silently revert). Fixed by switching to nilable
      pointers before any real caller could hit this, eliminating the
      whole bug class by construction rather than documenting the trap.
      Two mechanical V-compiler quirks surfaced and fixed along the way,
      unrelated to the finding above: `match` on a repeated array-index
      expression (`frames[N]`) doesn't reliably narrow a sum type across
      multiple field accesses within one arm once the sum type has enough
      variants — affected both new Phase 6 tests and two PRE-EXISTING
      tests in `frame_test.v`/`initial_exchange_test.v` that had worked
      fine with fewer variants; fixed by binding to a local variable
      before matching (the already-idiomatic pattern used everywhere
      else). Separately, a pre-existing test's "frame type 0x08 is not
      yet implemented" case became false once Phase 6 implemented STREAM
      frames at that exact type value; retargeted to 0x1e
      (HANDSHAKE_DONE), still genuinely unimplemented.

## Phase 7 — Loss detection and NewReno congestion control (done)

- [x] `rtt.v` — `RttEstimator` (RFC 9002 §5.3): first-sample-seeds-directly
      vs. subsequent-sample-EWMA as two genuinely distinct code paths (not
      the same formula with a placeholder initial value); ACK Delay
      unconditionally treated as zero for the Initial/Handshake spaces,
      decided from the `space` parameter inside `update()` itself rather
      than trusted to every caller; the peer's `max_ack_delay` clamp
      applied only once the handshake is confirmed; the ack_delay
      subtraction itself only applied when it cannot drive the sample
      below `min_rtt`. `pto_period()` factors out the
      `smoothed_rtt + max(4*rttvar, kGranularity)` term loss_detection.v
      scales per space/backoff.
- [x] `loss_detection.v` — `QuicLossDetectionTimer`: three independent
      per-space `LossDetectionSpaceState` (packet numbering genuinely is
      per-space, RFC 9000 §12.3) plus a single connection-wide
      `RttEstimator`/`pto_count`/PTO timer (the PTO timer is deliberately
      NOT per-space — sourced from whichever space's own deadline is
      earliest via `pto_time_and_space`, the plan's own explicitly flagged
      pitfall, the opposite mistake from treating packet numbers as
      connection-global). `detect_and_remove_lost_packets` implements
      RFC 9002 §6.1's packet-threshold (kPacketThreshold=3) OR
      time-threshold (9/8·max(latest_rtt,smoothed_rtt), floored at
      kGranularity) rule, either alone sufficient. `is_persistent_congestion`
      implements RFC 9002 §7.6.2 from a single detection-pass batch (a
      documented v1 scope choice — see its own doc comment for why this
      matches the realistic PTO-stall trigger pattern).
- [x] `congestion_control.v` — `NewRenoCongestionControl` (RFC 9002
      Appendix B): slow start / congestion avoidance via `is_in_slow_start()`
      (re-derives `congestion_window < ssthresh` every call — see the
      `/vreview` finding below for why this can't be shortcut to "has a
      loss ever happened"), ordinary-loss ssthresh halving via
      `on_congestion_event`, a distinctly harsher persistent-congestion
      collapse straight to `kMinimumWindow`, and `in_congestion_recovery`
      ensuring one recovery episode reacts exactly once regardless of how
      many packets it takes down. App-limited detection (RFC 9002 §7.8) is
      a documented v1 scope omission — no real send queue exists yet
      (Phase 9); spec-legal, only affects how eagerly cwnd grows, not
      correctness of loss/recovery handling.
- [x] Tests: first-vs-subsequent RTT formula, ACK-Delay-ignored-for-
      Initial/Handshake (two identically-seeded estimators, one fed a huge
      delay, must converge identically), max_ack_delay clamp only after
      confirmation, packet-threshold-only and time-threshold-only loss
      (each engineered so the other threshold structurally cannot also
      fire), single-PTO-timer-sourced-from-earliest-space (including the
      application_data-excluded-before-confirmation/included-after case),
      persistent-congestion collapse as its own dedicated test, and
      single-reaction-per-recovery-episode (multiple losses within one
      episode react once; a loss from a genuinely later episode reacts
      again).
- [x] `/vreview` (full A-G pass) found and fixed two issues before commit:
      (1) `NewRenoCongestionControl.is_in_slow_start()` originally
      shortcut to `ssthresh == none` ("a loss has ever happened") instead
      of RFC 9002's actual `congestion_window < ssthresh` — these diverge
      after a persistent-congestion collapse, which resets `congestion_window`
      to `kMinimumWindow` while leaving the larger, just-computed `ssthresh`
      untouched, so cwnd can legitimately fall back below an already-set
      ssthresh and must re-enter slow start, not stay in congestion
      avoidance. Caught via the from-scratch contract restatement, before
      any test was written against it. (2) A real DoS: `on_ack_received`'s
      newly-acked extraction originally iterated each ACK range's own
      `[smallest, largest]` span directly — but `largest_acknowledged` and
      `first_ack_range` are independent wire varints (RFC 9000 §19.3), so
      a tiny, well-formed ACK frame can legally claim a range spanning up
      to 2^62-1 packet numbers with no relationship to the frame's own
      wire size, and the parser's existing `ack_range_count` bound (against
      remaining buffer length) does nothing to limit an individual range's
      span. Fixed by iterating `sent_packets` (bounded by how many packets
      *we* actually have outstanding) and testing membership against the
      ranges instead of iterating the peer-supplied span — regression test
      constructs a `largest_acknowledged = 2^62` range and confirms
      `on_ack_received` still completes and resolves correctly.

## Phase 8 — Connection lifecycle (done)

- [x] `idle_timeout.v` — `effective_idle_timeout` resolves RFC 9000
      §10.1's min-of-non-zero rule across all 4 zero/non-zero
      combinations (0 means "no timeout", not literally zero).
      `IdleTimeoutState` tracks the deliberately ASYMMETRIC reset rule:
      an ack-eliciting packet RECEIVED restarts it, a non-ack-eliciting
      receive does not, but ANY packet SENT restarts it regardless — not
      "any packet either direction".
- [x] `connection_close.v` — `ConnectionCloseTracker`: `active` ->
      `closing` (this endpoint sent its own CONNECTION_CLOSE, may still
      send a rate-limited retransmission — at most once per received
      packet, RFC 9000 §10.2.1) -> `draining` (this endpoint received the
      peer's CONNECTION_CLOSE, or was already closing and then received
      one; MUST NOT send anything at all, §10.2.2's fully-silent
      requirement, a one-way absorbing state).
- [x] `stateless_reset.v` — `StatelessResetTracker` records
      stateless-reset tokens keyed by connection ID; `is_stateless_reset`
      is documented as callable ONLY after normal AEAD decryption has
      already failed (RFC 9000 §10.3.1 — never a first-choice
      interpretation, since a legitimate packet's ciphertext could
      coincidentally end in the same 16 bytes as an unrelated token), and
      compares the trailing 16 bytes via `crypto.subtle`'s
      constant-time compare (a token is a secret; a variable-time compare
      would leak a timing side-channel). Scoped-down CID handling: tokens
      are recorded for matching only, no full NEW_CONNECTION_ID/
      RETIRE_CONNECTION_ID rotation.
- [x] `ecn.v` — `EcnState` parses/records a peer's reported ECN counts
      (frame.v's `EcnCounts`, already implemented) without erroring, but
      `is_validated()` always reports false — no OS-level ECN socket
      option exists in V today to mark outgoing datagrams, so there is
      nothing to validate (a spec-legal fallback, RFC 9000 §13.4.2, not a
      violation); this is the checkpoint any future congestion-control
      integration must consult before reacting to an ECN-CE mark, and it
      can never be true in v1.
- [x] `pmtu.v` — pinned to the existing 1200-byte safe minimum (reusing
      `congestion_control.v`'s `max_datagram_size` rather than
      introducing a third constant for the same number, alongside
      `coalesce.v`'s `min_initial_datagram_size`), no active DPLPMTUD
      probing. Connection migration stays explicitly out of scope.
- [x] Tests: idle-timeout min-of-non-zero across all 4 combinations, the
      reset asymmetry (ack-eliciting-receive-or-any-send), closing's
      rate-limited retransmission vs. draining's fully-silent behavior,
      stateless reset matched only via a previously-recorded token, ECN
      counts recorded without erroring and never validated, and a runtime
      assertion pinning the PMTU at exactly 1200 bytes.
- [x] `/vreview` (full A-G pass) found and fixed one issue before commit:
      a real integer-overflow bug. `max_idle_timeout` is a peer-supplied
      transport-parameter varint (RFC 9000 §18.2) that
      `transport_parameters.v` accepts with NO upper bound (up to the
      full 2^62-1 varint range) — `effective_idle_timeout` originally
      multiplied it directly by `time.millisecond`, which overflows the
      i64 backing `time.Duration` for any peer-supplied value above
      ~9.2 trillion ms, silently producing a nonsensical (possibly
      negative) timeout: a hostile or buggy peer could self-inflict a
      near-immediate teardown, or effectively disable the idle timeout
      altogether. Fixed by clamping to `max_safe_idle_timeout_ms`
      (derived from `time.infinite`, i.e. i64::MAX ns, divided by
      `time.millisecond` — the exact mathematically-necessary bound, not
      an arbitrary policy number) before scaling; regression test feeds
      the maximum possible QUIC varint (2^62-1) through both the
      one-sided and both-sided paths and confirms a large-but-finite,
      strictly positive `Duration` comes back.

## Phase 9 — QuicConn top-level struct and event loop

No prior phase composes any two of the pieces built so far — every phase
built independently unit-tested state and deferred wiring to "a future
QuicConn." `vlib/net/quic/conn.v` is that wiring. Sub-phased like Phase 2,
landing as one PR:

- [x] **9a — Connection establishment** (`conn.v`): `dial()` picks
      `scid`/`original_dcid`, derives Initial secrets, builds ClientHello.
      `poll()`/`process_timeouts()` handle Retry/VN detection (RFC 9000
      §17.2.5.2's at-most-one-Retry + VN/Retry anti-spoof state, both
      explicitly documented in `retry.v`/`version_negotiation.v` as this
      phase's job), Initial/Handshake packet demux→unprotect→frame-parse,
      driving `Tls13ClientHandshake` through to `process_finished`, key
      derivation/promotion per level, CRYPTO-frame reassembly via
      `CryptoStreamReassembler`, ACK generation/processing for
      Initial/Handshake tied into `loss_detection.v`, idle timeout, and key
      discard on handshake confirmation (RFC 9001 §4.9).
- [x] **9b — Steady state**: 1-RTT packet processing (STREAM/ACK/
      CONNECTION_CLOSE dispatch into `QuicStreamSet` + flow control both
      directions), `open_stream`/`write_stream`/`read_stream`, full
      loss-detection↔congestion-control wiring for 1-RTT sends,
      MAX_STREAMS enforcement on locally-opened streams against the peer's
      current limit, stateless-reset detection on decrypt failure, ECN
      count recording, graceful/immediate close, 1-RTT key update rotation
      (`app_read_keys`/`app_write_keys`).
- [x] Tests: `conn_test.v`, hand-built ServerHello→Finished fixtures
      (reusing Phase 2's RFC 8448/quiche vectors, no live server) driving
      `dial()`+`poll()` to `handshake_confirmed`; STREAM data round-trip
      through `poll()`; CONNECTION_CLOSE handling; key update.
- [x] `/vreview` found and fixed two real bugs, both reproduced with a
      failing test before the fix landed (Phase R): (1) `write_stream`/
      `read_stream` never called `ensure_stream_windows` for a stream that
      only exists because RFC 9000 §2.1 auto-created it as a lower-numbered
      sibling of a peer-referenced higher stream ID — such a stream had no
      flow-control window, so a local write to it queued forever with no
      error, never reaching the wire; (2) `handle_peer_connection_close`
      and the stateless-reset branch of `note_one_rtt_processing_failed`
      transitioned to `.draining` without setting `closing_deadline` (RFC
      9000 §10.2.2 requires the draining period to be bounded, same as
      closing) — a peer-initiated close, the most common real-world close
      path, left the connection draining forever instead of eventually
      reaching `.closed`. Fixed with a shared `enter_draining(now)` helper
      mirroring `close_with_error`'s existing deadline formula.

Connection ID rotation/migration is explicitly OUT of scope (not deferred
to a later phase — `stateless_reset.v`/`pmtu.v` both say so independently):
`QuicConn` holds exactly one local `scid` and tracks the peer's current
`dcid`, no active CID set, no NEW_CONNECTION_ID/RETIRE_CONNECTION_ID.

## Phase 10: HTTP/3 framing (RFC 9114) — CODE COMPLETE, not yet wired to conn.v

Full section-by-section requirements matrix built BEFORE writing any code
(`.claude/skills/code-review/quic_conformance_matrix.md`, "HTTP/3 framing
layer" section) — the direct structural response to Phase 9's own
postmortem ("build the RFC checklist before/during implementation, not
reactively"). See that section for the complete requirement-by-requirement
breakdown; summary here.

New files, all in `vlib/net/quic/`:

- [x] `h3_reserved.v` — the single 0x1f*N+0x21 grease-codepoint formula
      shared identically across frame types (§7.2.8), stream types
      (§6.2.3), SETTINGS identifiers (§7.2.4.1), and error codes (§8.1) —
      confirmed byte-for-byte identical wording in all 4 RFC citations
      before writing one shared helper instead of four copies.
- [x] `h3_error.v` — `H3ErrorCode` enum, all 17 values (§8.1/Table 4).
- [x] `h3_stream_type.v` — unidirectional Stream Type header (§6.2/6.2.1/
      6.2.2/6.2.3): control (0x00), push (0x01 + push ID), reserved,
      unknown. Incremental (returns `none`, not an error, on a short
      buffer — RFC places no requirement on how header bytes are split
      across QUIC STREAM frames).
- [x] `h3_frame.v` — frame Type/Length envelope (§7.1) + all 7 defined
      frame types' payloads (DATA/HEADERS/CANCEL_PUSH/SETTINGS/
      PUSH_PROMISE/GOAWAY/MAX_PUSH_ID, §7.2.1-7.2.7) + the 4 H2-carryover
      reserved frame types (§7.2.8/Table 2) rejected outright as
      H3_FRAME_UNEXPECTED, distinct from grease/genuinely-unknown types
      (§9), which are preserved as `H3RawFrame` and never rejected.
      `H3FrameDecoder` is the incremental/resumable reader this phase's
      whole scope centers on — buffers partial bytes across `push()`
      calls, only ever returns a frame once its FULL declared Length is
      available. SETTINGS payload parsing rejects duplicate identifiers
      (a MAY in §7.2.4, chosen to enforce — documented as a choice, not
      claimed as a literal MUST) and the 5 reserved HTTP/2-carryover
      setting identifiers (§7.2.4.1/Table 3), while correctly NOT
      rejecting QPACK's own 0x01/0x07 (RFC 9204, not reserved by 9114
      itself) so Phase 11 can add them as recognized without touching
      this reserved set.
- [x] `h3_message_state.v` — the one piece of §4's message-framing rules
      that needs only a stream's ROLE, not request/response context:
      Table 1's per-role frame-type legality (`is_h3_frame_valid_on_stream`)
      and the control-stream SETTINGS-must-be-first-and-only-once
      discipline (`H3ControlStreamState`).
- [x] Tests: one `_test.v` per file, covering round-trips, every reserved/
      grease/unknown-type boundary, incremental byte-by-byte feeding,
      truncated/trailing-byte rejection, and a self-caught integer-overflow
      hardening case (`int(length)` on an attacker-controlled u64 up to
      2^62-1 could truncate/wrap before ever reaching a real bounds check
      — fixed and regression-tested before the first real compile, not
      found by an external round).

**Explicitly deferred to Phase 12** (needs request/response objects, a
real connection stream registry, or cross-frame state this framing-only
phase doesn't have — every instance is a matrix row marked `⏳ Phase 12`,
not a silent gap): §4.1's HEADERS→DATA*→trailer-HEADERS message-content
sequencing within one request/response; single-request-per-stream
enforcement; CONNECT's distinct framing; PUSH_PROMISE/CANCEL_PUSH/
MAX_PUSH_ID cross-frame state (max advertised push ID, push IDs already
seen); control-stream uniqueness/closure enforcement; unidirectional
unknown-stream-type abort/discard action; remapping an unrecognized
received error code to H3_NO_ERROR. **N/A for this v1 client role**
(confirmed by re-reading which endpoint the requirement binds, not
assumed): MAX_PUSH_ID's must-not-decrease check (binds a server receiving
it from a client); a server receiving a client-initiated push stream.

## Phase 11: QPACK (RFC 9204) — CODE COMPLETE, not yet wired to conn.v

Started and finished 2026-08-16, same session as Phase 10's merge — user
asked to "Start Phase 11 and be very careful on RFC compliance and edge
details... provide confidence score." New worktree
`S:\repo\vlang-http3-qpack`, branch `http3-quic-qpack`, cut off
`upstream/master` (all of 0-10 merged, no stacking needed). Same
methodology as Phase 10: fetched RFC 9204's full text
(`rfc-texts/rfc9204.txt`), read all of §1-§8 + Appendix A (static table) +
Appendix B (worked examples) + Appendix C (sample encoding algorithm) in
full BEFORE writing any code, built the `quic_conformance_matrix.md`
"QPACK" section from that reading, THEN implemented against it.

**Scope boundary, mirroring Phase 10's own precedent exactly:** QPACK's
tables, wire codecs, and the encoder/decoder state machines are all
self-contained (like Phase 10's `H3ControlStreamState`, they only need to
be *fed* bytes/events by whatever eventually owns a real QUIC stream) — so
that whole layer is Phase 11 scope, including the FULL encoder/decoder
driver state machines (dynamic table with eviction/reference-counting,
Known Received Count, blocked-stream tracking), not just the wire codecs.
Deferred to Phase 12: writing encoder-stream bytes onto a real QUIC
unidirectional stream under real flow control (§2.1.3); blocking/
unblocking a live HTTP/3 request stream's read progress (§2.2.1 — needs
Phase 12's request/response objects); applying a peer's actual SETTINGS
values to a real connection (Phase 11 only provides the pure extraction
helper, `qpack_settings.v`).

**12 new files**, each with a paired `_test.v` except the trivially small
`qpack_error.v`: `qpack_primitives.v` (prefixed integer + string literal
codec, generalized to QPACK's variable prefix widths, algorithm verified
against the already-shipped, tested `h2_hpack.v` before being written),
`qpack_huffman_table.v` + `qpack_huffman.v` (verbatim copies of
`h2_hpack_huffman_table.v`/`h2_hpack_huffman.v` — RFC 9204 §4.1.2 mandates
byte-identical reuse of RFC 7541 Appendix B's table, so copying a proven
implementation is strictly safer than a second hand-transcription; the
copy was verified byte-for-byte identical via a numeric diff before
trusting it), `qpack_static_table.v` (all 99 entries transcribed from the
fetched RFC text, indexed from 0 unlike HPACK's 61-entry table indexed
from 1), `qpack_dynamic_table.v` (insert/evict/duplicate/capacity,
absolute/relative-from-insert-count/relative-from-Base/post-Base indexing
as 4 DISTINCT resolution functions — conflating the encoder-instruction
and field-line relative-index contexts was the most likely transcription
error here — reference counting for eviction protection),
`qpack_error.v` (mirrors `h3_error.v`'s shape exactly), `qpack_stream_type.v`
(0x02/0x03 recognition + a `QpackStreamRegistry` at-most-one-of-each
tracker, a from-scratch sibling comparison against `H3ControlStreamState`
done before writing it), `qpack_settings.v` (pure extraction of the 2
QPACK SETTINGS from an already-decoded `[]H3Setting`),
`qpack_encoder_instructions.v` + `qpack_decoder_instructions.v` (wire
codecs for the 4 encoder-stream and 3 decoder-stream instruction types),
`qpack_field_line.v` (Required Insert Count wraparound math + Base sign/
delta math, transcribed directly from the RFC's own pseudocode, plus the 6
field line representation types), `qpack_encoder.v` (the `QpackEncoder`
driver — chose the RFC-offered "only reference acknowledged entries"
policy, so it never risks blocking a stream at all; a documented scope
decision, not an oversight), `qpack_decoder.v` (the `QpackDecoder` driver
— blocked-field-section detection, invalid-reference rejection, emits
Insert Count Increment after every insertion as its acknowledgment
policy).

**Verification, in order of how much confidence each step actually buys:**
(1) hand-derived every byte of RFC 9204 Appendix B's 5 worked examples
against my own algorithms WHILE transcribing them (not after) — this
independently reproduced the RFC's own shown intermediate values (Set
Dynamic Table Capacity's 3-byte encoding of 220, the running dynamic-table
Size totals of 106/160/217/215) before a single test ran; (2) then wrote
those exact byte sequences as `qpack_appendix_b_test.v`, an end-to-end
test exercising the encoder-instruction codec, dynamic table, field-line
codec, and decoder driver together against official ground truth — passed
outright on B.1-B.4, and B.5's one failure was correctly diagnosed as a
test-design issue (asserting my own encoder reproduce the RFC's
illustrative, deliberately-non-optimal raw-string choice, when this
implementation's `encode_prefixed_string` correctly picks the shorter
Huffman encoding instead) rather than patched around; (3) three more real
bugs were caught by writing per-file edge-case tests and RE-DERIVING the
encoder's indexing math by hand before trusting it, not by running code
and hoping: `encode_field_section` mixed up relative-vs-post-Base indexing
context for a newly-inserted entry (a field-line reference to an entry
just inserted during the same call has absolute index >= Base and MUST
use post-Base indexing, not relative-to-Base), used post-insert
`insert_count()` instead of pre-insert for an Insert-With-Name-Reference
instruction's own relative index (an off-by-one, since a decoder resolves
that index against table state as it stood BEFORE this instruction's own
insert takes effect), and passed an absolute index directly where a
Base-relative index was required for a literal-with-existing-dynamic-name
reference — all three caught and fixed by re-deriving the function's
contract from scratch before ever compiling it, the same discipline this
project's own postmortems establish as the highest-yield check available;
(4) three ordinary test-authoring mistakes (unrealistic RIC/total_inserts
test combinations outside the protocol's actual usage pattern, a missing
`u8+u8` concatenation, a test decoder that never received the Set
Dynamic Table Capacity instruction its paired encoder sent) were found and
fixed the normal way, by running the suite and reading the failure.
Full `net.quic` suite green throughout, 52/52 files, zero regressions in
Phases 0-10.

## Phase 12: HTTP/3 client wiring — COMPLETE (12a/12b/12c/12d all done)

Sub-scoped into 4 sequential sub-phases within one PR (mirroring Phase 2's
2a/2b/2c and Phase 9's 9a/9b precedent), each a hard dependency of the
next:

- **12a** (done) — surgical additions to already-merged Phase 9 code
  (`conn.v`/`tls13_handshake.v`): a negotiated-ALPN accessor (computed
  during the handshake but previously discarded), a `peer_stream_opened`
  event for peer-initiated stream discovery (careful to avoid a silent
  false-negative under UDP reordering caused by `get_or_create`'s
  RFC 9000 §2.1 sibling-auto-creation behavior), and a
  `stream_recv_status` query. No new files.
- **12b** (done) — HTTP/3 + QPACK connection wiring, pure `module quic`:
  `H3Conn` wraps `QuicConn` with control-stream/QPACK-stream driving, the
  request-stream message-framing state machine (RFC 9114 §4.1), and the
  previously-entirely-missing blocked-HEADERS retry/re-queue mechanism.
  Fixture-testable with no socket, same style as Phase 9's `conn_test.v`.
- **12c** (done) — UDP transport + `H3MuxConn` threading, `module http`
  (`h3_udp_dial.v`, `h3_mux_conn.v`): the repo's first UDP socket code and
  first background-thread-drives-a-non-thread-safe-`poll()`-state-machine
  code. This does NOT contradict the "single-threaded, caller-driven event
  loop" scope decision below -- `net.quic`'s own `QuicConn`/`H3Conn` stay
  exactly that; `H3MuxConn`'s driver thread is simply THE caller, on the
  `net.http` side, the same relationship `H2MuxConn`'s own background
  reader thread already has to the (blocking-transport-shaped) `H2Conn`
  layer. Needs only ONE lock (`qmu`), not `H2MuxConn`'s wmu/fmu/smu split,
  because there is exactly one thread that ever touches `h3`/the
  transport -- request threads queue via `do()`/`PendingH3Request` and
  block on their own condition variable, mirroring `H2MuxConn.wait_
  response`'s shape for the response half only; the driver thread alone
  opens streams and sends. `H3UdpTransport` (mirrors `H2Transport`)
  decouples the driver from a concrete `net.UdpConn`, letting
  `h3_mux_conn_test.v` drive a REAL driver thread against an in-memory
  fake and directly regression-test this sub-phase's own top risk: a
  request queued by `do()` between one driver loop iteration and the next
  must not be stranded on `cv.wait()` forever if the connection dies in
  that exact window (`fail_conn` drains `c.pending`, not just
  `c.streams`). `Transport` (`transport.v`) gained a third pool
  (`h3_conns`/`h3_dial_id`) folded into the existing shared idle-eviction
  scan (`evict_oldest_idle_locked`, `close_idle`) alongside h1/h2 -- the
  actual h3 dial-and-register call site (which would populate them) is
  12d's job.
  Deliberately NOT built: a genuinely reactive fake QUIC server for
  request/response-level testing -- would mean re-deriving a second
  QUIC/TLS 1.3 server-role implementation from scratch, since `conn_
  test.v`'s own fixture-handshake bytes are private, test-file-only
  helpers with no cross-module export. A manual/documented run against a
  real `quiche`/`ngtcp2` container (non-CI-blocking) remains the
  recommended pre-merge check for full wire-level behavior; a shared,
  exported cross-module fixture helper in `net.quic` is a scoped,
  worthwhile follow-up, not built inline here.
- **12d** (done) — `Transport`/`Request`/`Response` integration:
  `req.enable_http3` opt-in (default `false`, no automatic h2/h1
  fallback), `h3_client.v` (`H3ClientRequest`/`H3ClientResponse`
  conversion -- the concrete types themselves already exist, defined in
  12c's `h3_mux_conn.v` since that is what first needed them to compile),
  `transport_h3.v` (`h3_round_trip`, `H3DialCall` singleflight dial,
  mirroring `transport_h2.v`'s own shape minus every ALPN-probe-outcome
  branch h2 needs and h3 doesn't). `Version` gained a `v3_0` case
  (`version.v`/`response.v`) so `resp.version()` reports something
  meaningful for an h3 response instead of `.unknown`.
  **Known v1 limitation, discovered and documented during `/vreview`**:
  `net.quic`'s TLS 1.3 stack has no OS/default trust-store fallback at
  all (unlike the h1/h2 `ssl.SSLConn` path) -- `req.verify` is therefore
  effectively REQUIRED for HTTP/3 today; leaving it unset means every h3
  request fails certificate verification against every real server. This
  is documented prominently on `enable_http3`'s own doc comment
  (`request.v`/`http.v`), not silently left as a surprise. Likewise,
  `req.validate` (skip-verification) and `req.cert`/`req.cert_key`
  (mutual TLS) are not honorable on the h3 path in v1, both also
  documented there.

**Phase A adversarial-verification pass (done)**: a multi-agent Workflow (5
independent finder lenses — rfc/concurrency/pool-lifecycle/error-edges/
holistic — each adversarially verified by independent skeptics) reviewed
the combined 12a-12d diff and surfaced 7 confirmed, real bugs missed by
each sub-phase's own `/vreview` pass (all were cross-sub-phase interaction
gaps, invisible to any single sub-phase's own diff-scoped review). All 7
fixed, tested, and re-reviewed:
1. **Fresh-dial first-request race** — `driver_loop` drains `c.pending`
   (opening the first queued request's stream) before that same iteration
   ever reads/polls the wire, so the very first request on a brand-new
   connection could hit QUIC's own `STREAM_LIMIT` (peer transport
   parameters not yet learned) or "QPACK encoder stream not open yet" —
   and neither `h3_dial_and_do`'s nor `h3_await_dial`'s final call retried
   on `h3_err_retryable_code`, making `enable_http3` fail on essentially
   every first request to a fresh origin. Fixed with `h3_do_on_fresh_conn`
   (`transport_h3.v`), a bounded same-connection retry distinct from
   `h3_round_trip`'s own different-connection retry.
2. **RFC 9114 §5.2 GOAWAY draining never implemented** — `dispatch_h3_
   event`'s `.goaway` case only blocked new admission; it never read
   `ev.goaway_id` or failed any already-open stream at/above the boundary
   (unlike `h2_mux_conn.v`'s identical `H2GoawayFrame` handler), so an
   in-flight request above the boundary could hang indefinitely or later
   be marked non-retryable by `fail_conn`'s `!sent_headers` heuristic.
   Fixed: the `.goaway` handler now walks `c.streams` and fails every
   stream `id >= boundary` retryable, mirroring H2 exactly; `start_request`
   also gained its own `goaway_received` check to close the narrow
   admission-race window (a request queued just before GOAWAY, drained
   just after).
3. **`H3_REQUEST_REJECTED` (RFC 9114 §8.1) not retryable** — `dispatch_h3_
   event`'s `.request_error` case hardcoded `retryable = false` for every
   error code, unlike H2's `REFUSED_STREAM` parity check. Fixed.
4. **Orphaned pooled connection leak** — `h3_dial_and_do`'s superseded-
   connection cleanup called only `orphan.release()`, never `orphan.
   shutdown_when_idle()`; unlike `H2MuxConn.release()`, `H3MuxConn.
   release()` is a documented no-op for teardown, so the orphan's driver
   thread + UDP socket leaked for the process's remaining lifetime. Fixed.
5. **Self-terminated connection never removed from the pool** — unlike
   `H2MuxConn` (a mandatory `close_transport` self-removal callback),
   `H3MuxConn` had no way to tell `Transport` it died on its own (idle
   timeout, fatal UDP error) — the dead entry stayed in `t.h3_conns`,
   where `evict_oldest_idle_locked`'s h3 scan (checking only `active_
   streams == 0`, not `can_take_new_request()`) could mistake it for a
   genuinely idle connection and evict it instead of a real one elsewhere.
   Fixed with an optional `on_retired` callback on `H3MuxConn`, mirroring
   H2's pattern but nil-tolerant (H3's teardown doesn't depend on it the
   way H2's blocked reader does).
6. **Peer-controlled error code could collide with the retryable
   sentinel** — a QUIC RESET_STREAM error code is a full peer-controlled
   62-bit varint; narrowing it to `int` for `error_with_code` could
   produce `h3_err_retryable_code` itself, causing a non-idempotent
   request the server explicitly rejected to be silently replayed. Fixed:
   only a positive `int()` result is trusted as a real error code.
7. **Unbounded per-connection memory growth** — `H3Conn.request_streams`/
   `request_decoders` were never pruned once a request finished, growing
   with the total number of requests ever served by a long-lived pooled
   connection rather than the number in flight. Fixed: both maps are
   pruned in `finalize_request_stream_if_done`'s success path and in
   `fail_request_stream`'s failure path.

Two additional bugs found via this project's own follow-up review of the
same code (not the Workflow, which hit its session limit before every
verifier finished): a genuine RFC 9110 §15.2 gap where the request-stream
message-framing state machine had no concept of 1xx informational
responses at all, so a `103`-then-`200` sequence delivered the 103 as the
final status and misdelivered the real 200 response's fields as trailers
(fixed in `h3_request_stream.v`/`h3_conn.v`: the `.awaiting_response_
headers` → `.in_body` phase transition is now deferred until the decoded
`:status` is known non-1xx); and a `:status` pseudo-header validation gap
in `wait_response` (no length/duplicate/ordering/unknown-pseudo-header
checks, unlike `h2_mux_conn.v`'s equivalent), now fixed to match.

Two scope decisions made without a response after being flagged for
sign-off, proceeding with the lower-risk default in each case (revisitable
during review): server push is permanently disabled for v1 (never sending
MAX_PUSH_ID means no push is ever authorized per RFC 9114 §7.2.7, so any
received PUSH_PROMISE/CANCEL_PUSH is unconditionally rejected
`H3_ID_ERROR` — avoids building real max-push-id/seen-push-id cross-frame
state for a feature v1 never uses); `enable_http3` has no automatic
h2/h1 fallback if the h3 attempt fails (UDP has no fast-fail signal the
way a closed TCP port does, so auto-racing every `https://` request
against h3 would regress the common case; real happy-eyeballs-style
fallback is deferred as a separate follow-up feature).

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
