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
  - [x] mbedTLS X.509 chain validation, standalone (no `mbedtls_ssl_context`,
        same discipline as Phase 0). New `net.mbedtls` public API
        (`x509_standalone.v`: `build_certificate_chain`/
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
        (`x509_standalone.v`) gained `get_leaf_public_key`/
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
      `pad_datagram_for_initial` pads a sender's own datagram to the RFC
      9000 §14.1 1200-byte minimum.
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
- [x] `version_negotiation.v` — a VN packet listing v1 itself is a hard
      PROTOCOL_VIOLATION (RFC 9000 §6.2), not a retry trigger; a VN packet
      without v1 fails the connection attempt cleanly, since this client
      implements only v1 with no lower-version fallback.
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
      datagram padded to 1200 bytes has, and what `pad_datagram_for_initial`
      itself produces) as a bogus additional coalesced packet, since
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

## Phases 8-14 (NOT STARTED)

See the tracking issue for full detail on each. In order:

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
