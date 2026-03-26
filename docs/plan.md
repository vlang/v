# HTTP/2 + HTTP/3 Spec Compliance Implementation Plan

Branch: `http2_http3`
Last updated: 2026-03-26
**Status: ALL PHASES COMPLETE — 23/23 tests pass**

## Audit Summary

| RFC                 | Implemented | Partial | Not Implemented |  N/A  |
| ------------------- | :---------: | :-----: | :-------------: | :---: |
| RFC 7540 (HTTP/2)   |     37      |    7    |        8        |   5   |
| RFC 7541 (HPACK)    |     20      |    0    |        0        |   0   |
| RFC 9114 (HTTP/3)   |      8      |   12    |       12        |   2   |
| RFC 9204 (QPACK)    |     21      |    2    |        0        |   0   |
| RFC 9000 (QUIC)     |     10      |    5    |        0        |   0   |
| RFC 9001 (QUIC-TLS) |      8      |    2    |        0        |   0   |
| **Total**           |   **104**   | **28**  |     **20**      | **7** |

---

## Phase 1: Critical RFC Violations (MUST FIX)

### P1-1: HTTP/3 Error Codes (RFC 9114 §8.1) — NOT IMPLEMENTED
- [x] Define `H3ErrorCode` enum with all 17 codes (0x0100–0x0110)
- [x] Use H3 error codes in `close_with_error()` and `send_goaway()`
- [x] Send H3_MISSING_SETTINGS when peer control stream has no SETTINGS
- Files: `v3/client.v` or new `v3/errors.v`, `v3/server.v`

### P1-2: Unknown Frame/Stream Type Handling — RFC VIOLATION
- [x] HTTP/2: Unknown frame types MUST be ignored (RFC 7540 §5.5), currently sends GOAWAY
- [x] HTTP/3: Unknown frame types MUST be ignored (RFC 9114 §7.2.8), currently errors
- [x] HTTP/3: Unknown stream types MUST be ignored (RFC 9114 §6.2.3), currently errors
- Files: `v2/server_loop.v`, `v2/frame.v`, `v3/client.v`, `v3/streams.v`

### P1-3: Malformed Request Detection (RFC 7540 §8.1.2.6 / RFC 9114 §4.1.2) — NOT IMPLEMENTED
- [x] HTTP/2: Validate mandatory pseudo-headers (:method, :path present)
- [x] HTTP/2: Reject requests with content-length mismatch
- [x] HTTP/3: Same validations
- Files: `v2/server_loop.v`, `v3/server_handlers.v`

### P1-4: Connection-Specific Header Filtering (RFC 7540 §8.1.2.2) — NOT IMPLEMENTED
- [x] Reject or strip: Connection, Keep-Alive, Proxy-Connection, Transfer-Encoding, Upgrade
- [x] Apply to both client and server (HTTP/2)
- Files: `v2/server_loop.v`, `v2/client.v`

### P1-5: Header Field Lowercase Validation (RFC 9114 §4.2) — NOT IMPLEMENTED
- [x] Validate received header names are lowercase
- [x] Reject or error on uppercase header names
- Files: `v3/server_handlers.v`, `v3/client.v`

---

## Phase 2: Spec Compliance — HTTP/2

### P2-1: Stream State Machine Enforcement (RFC 7540 §5.1) — PARTIAL
- [x] Enforce valid state transitions in both client and server
- [x] Reject frames received on streams in wrong state
- Files: `v2/stream.v`, `v2/client.v`, `v2/server_loop.v`

### P2-2: Client-Side INITIAL_WINDOW_SIZE Adjustment (RFC 7540 §6.9.2) — PARTIAL
- [x] When server sends new INITIAL_WINDOW_SIZE, adjust all existing stream windows by delta
- Files: `v2/connection.v`

### P2-3: Cookie Header Compression (RFC 7540 §8.1.2.5) — NOT IMPLEMENTED
- [x] Split Cookie headers into individual fields before HPACK encoding
- [x] Rejoin on decode
- Files: `v2/hpack.v` or new helper

### P2-4: Pseudo-Header Validation (RFC 7540 §8.1.2.1/§8.1.2.3) — PARTIAL
- [x] Validate pseudo-headers appear before regular headers
- [x] Validate no unknown pseudo-headers
- [x] Reject responses without :status
- Files: `v2/server_loop.v`, `v2/client.v`

---

## Phase 3: Spec Compliance — HTTP/3

### P3-1: ALPN Verification Post-Handshake (RFC 9114 §3.2) — PARTIAL
- [x] Verify `h3` was negotiated after QUIC handshake using get_alpn_selected()
- Files: `v3/client.v`

### P3-2: Message Framing FIN / Ordering (RFC 9114 §4.1) — PARTIAL
- [x] Signal end-of-stream with QUIC FIN on last DATA frame
- [x] Enforce HEADERS before DATA ordering on request streams
- Files: `v3/client.v`, `v3/server_handlers.v`

### P3-3: GOAWAY 2-Phase Shutdown (RFC 9114 §5.2) — PARTIAL
- [x] Server sends GOAWAY with max stream ID first, then final GOAWAY
- [x] Respect peer's GOAWAY stream ID (already partial)
- Files: `v3/server.v`, `v3/client.v`

### P3-4: SETTINGS Expansion (RFC 9114 §7.2.4) — PARTIAL
- [x] Send actual client SETTINGS (not empty)
- [x] Reject duplicate settings
- [x] Reject settings on wrong stream type
- Files: `v3/client.v`, `v3/server_handlers.v`

### P3-5: QPACK Capacity Enforcement + Dynamic Table Resize (RFC 9204 §3.2.3/§5) — PARTIAL
- [x] Enforce encoder capacity <= peer's SETTINGS_QPACK_MAX_TABLE_CAPACITY
- [x] Resize dynamic table when peer sends new capacity
- Files: `v3/qpack_encoder.v`, `v3/qpack_decoder.v`, `v3/qpack_tables.v`

### P3-6: Request Cancel Wiring (RFC 9114 §4.1.1) — PARTIAL
- [x] Wire reset_stream()/stop_sending() to HTTP/3 request cancellation
- [x] Send H3_REQUEST_CANCELLED error code
- Files: `v3/client.v`

---

## Phase 4: QUIC Transport Improvements

### P4-1: QUIC Packet Matching by CID (RFC 9000 §5.2) — PARTIAL
- [x] Server: match packets by connection ID, not remote address
- Files: `v3/server.v`

### P4-2: QPACK Dynamic Table Capacity Negotiation — PARTIAL
- [x] Apply peer's QPACK_MAX_TABLE_CAPACITY to encoder
- [x] Send Set Dynamic Table Capacity instruction on change
- Files: `v3/qpack_encoder.v`, `v3/client.v`, `v3/server.v`

---

## Intentional Non-Implementation (Design Decisions)

These features are intentionally NOT implemented and documented:

| Feature                  | RFC Section              | Reason                                |
| ------------------------ | ------------------------ | ------------------------------------- |
| Server Push              | HTTP/2 §8.2, HTTP/3 §4.6 | ENABLE_PUSH=0, deprecated in browsers |
| CONNECT Method           | HTTP/2 §8.3, HTTP/3 §4.4 | Tunneling not in scope                |
| h2c Upgrade              | HTTP/2 §3.2              | Direct h2c supported instead          |
| Alt-Svc Discovery        | HTTP/3 §3.1              | Client assumes h3 support             |
| 421 Status Code          | HTTP/2 §9.1.2            | Connection reuse not implemented      |
| Connection Reuse/Pooling | HTTP/2 §9.1.1            | Future enhancement                    |

## ngtcp2-Delegated (External Library)

| Feature                             | RFC Section | Status                    |
| ----------------------------------- | ----------- | ------------------------- |
| CID-based demux                     | QUIC §5.2   | ngtcp2 handles internally |
| Congestion control during migration | QUIC §9.4   | ngtcp2 handles            |
| Idle timeout                        | QUIC §10.1  | ngtcp2 transport param    |
| Stateless reset token               | QUIC §10.3  | Generated in callback     |
| Key update                          | QUIC-TLS §6 | ngtcp2 callback           |

---

## Execution Plan

| Group  | Phase | Tasks                                             | Depends On |
| ------ | ----- | ------------------------------------------------- | ---------- |
| G1a    | P1    | P1-1, P1-5 (HTTP/3 files)                         | —          |
| G1b    | P1    | P1-2, P1-3, P1-4 (HTTP/2 files)                   | —          |
| G2a    | P2    | P2-1, P2-2, P2-4 (HTTP/2 files)                   | G1b        |
| G2b    | P2    | P2-3 (HPACK files)                                | —          |
| G3a    | P3    | P3-1, P3-2, P3-3, P3-4, P3-5, P3-6 (HTTP/3 files) | G1a        |
| G3b    | P3    | P4-1, P4-2 (QUIC files)                           | G3a        |
| CHECK  | —     | QA verification                                   | all        |
| SCRIBE | —     | Git commit + docs                                 | CHECK      |
