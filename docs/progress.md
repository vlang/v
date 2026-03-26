# Progress

Chronological record of completed work.

---

## 2026-03-26 — HTTP/2, HTTP/3, QUIC Implementation Complete

**Branch:** `http2_http3`  
**Commits:** `02a7110ab` through `00ae4c606` (6 commits)  
**PDCA Cycles:** 5 implementation phases + 3 CHECK/ACT iterations

### Completed

- [x] **Phase 1: Foundation refactoring** — deduplicated SETTINGS parsing, structured frame types, thread-safe Huffman trie (`sync.Once`), legacy code removal
- [x] **Phase 2: HTTP/2 feature completion** — sender flow control, WINDOW_UPDATE, server DATA accumulation, GOAWAY shutdown, stream multiplexing, PRIORITY frames, ConnectionPool, ALPN docs, optimized Huffman encoding
- [x] **Phase 3: HTTP/3 QPACK completion** — RIC/Delta Base, encoder/decoder stream instructions, Huffman outgoing compression, blocked streams handling
- [x] **Phase 4: QUIC integration** — server crypto key derivation (HKDF), connection migration, 0-RTT early data, cache synchronization, unidirectional streams, packet number normalization
- [x] **Phase 5: ACT iterations (3 rounds)** — 37 protocol fixes, security hardening (RAND_bytes, key validation, handshake abort, mutex), SRP file splits, function decomposition (all <=50 lines)

### Test Results

| Module                       | Test Files | Status           |
| ---------------------------- | ---------- | ---------------- |
| HTTP/2 (`vlib/net/http/v2/`) | 8          | All pass         |
| HTTP/3 (`vlib/net/http/v3/`) | 5          | All pass         |
| QUIC (`vlib/net/quic/`)      | 5          | All pass         |
| **Total**                    | **18**     | **18/18 (100%)** |

### Stats

- 78 files changed (full history), 47 files in final iteration
- 13,120 insertions / 3,593 deletions (full history)
- 17 new files created
- 76 new test functions
- 4 critical bugs found and fixed
- 4 high-severity bugs found and fixed

### Key Decisions

1. Used `sync.Once` for Huffman trie initialization (thread safety without performance cost)
2. Try-and-fail ALPN pattern due to V `net.ssl` limitation (no `get_alpn_selected()`)
3. Heap-allocated `QuicPathAddrs` for ngtcp2 C callbacks (stack lifetime issue)
4. Counter-based fallback for packet number extraction when header protection keys unavailable
5. SRP file splits over function-only extraction for files >300 lines

### Follow-up Items

- V `net.ssl` ALPN result inspection API (requires upstream V change)
- Server TLS type abstraction (`ServerConn` interface for SSLConn/TcpConn)
- DER certificate format support
- Full QPACK dynamic table eviction policy

---

## Architecture & Medium-Priority Fixes — 2026-03-26

### Completed
- [x] A1: encode_optimized O(n)→O(1) hashmap lookup
- [x] A2: SETTINGS ACK DRY — new_settings_ack_frame() factory
- [x] A3: max_frame_size validation on read (DoS prevention)
- [x] A4: SETTINGS range validation (RFC 7540 §6.5.2)
- [x] A5: QPACK DynamicTable ring buffer (O(1) eviction)
- [x] A6: PN extraction + header protection pipeline (RFC 9001 §5.4)
- [x] B1: Server TLS via ServerConn interface + SSLListener
- [x] B2: ALPN get_alpn_selected() — mbedtls + OpenSSL backends
- [x] B3: Server outbound flow control (WINDOW_UPDATE + enforcement)
- [x] C1+C2: Doc comments (Method enum, ServerConnection)

### Test Results
- HTTP/2: 9/9 pass
- HTTP/3: 5/5 pass
- QUIC: 5/5 pass

### QA Scores
- HTTP/3+QUIC: 86/100 (B) — APPROVED
- HTTP/2: 77.8/100 (C+) → ACT fixes applied (projected ~84)

### Remaining (tracked as follow-up)
- server.v 303 lines (borderline)
- run_frame_loop 56 lines (borderline)
- v3/client.v 348, v3/server.v 358 (pre-existing)

---

## Spec Compliance & Security Hardening — 2026-03-26

### Completed
- [x] HTTP/2 CONTINUATION flood protection (CVE-2024-27316 class)
- [x] HTTP/2 GOAWAY on protocol errors
- [x] HTTP/2 padding support (DATA + HEADERS)
- [x] HTTP/2 server window=0 returns error instead of dropping body
- [x] HTTP/2 PUSH_PROMISE explicit rejection + ENABLE_PUSH=0
- [x] HTTP/2 max_header_list_size enforcement in HPACK decoder
- [x] HTTP/2 HPACK dynamic table size update emission
- [x] HTTP/2 client MAX_CONCURRENT_STREAMS enforcement
- [x] HTTP/3 unidirectional stream integration
- [x] HTTP/3 QPACK encoder/decoder stream instruction buffering
- [x] HTTP/3 GOAWAY handling on receive (client + server)
- [x] HTTP/3 request body chunking (16KB frames)
- [x] HTTP/3 control stream setup per RFC 9114 §6.2.1
- [x] QUIC CONNECTION_CLOSE frame on close
- [x] QUIC flow control exposure (max_data_left, streams_left)
- [x] QUIC stream reset + stop_sending
- [x] QUIC migration path switch via ngtcp2
- [x] QUIC 0-RTT early data flush after handshake
- [x] AGENTS.md compliance: copy pasta dedup, doc comments, unsafe minimization

### Test Results
- HTTP/2: 9/9 passed
- HTTP/3: 5/5 passed
- QUIC: 5/5 passed

### QA Scores
- HTTP/2: 75.7 → HIGH fixes applied (server HEADERS, mid-stream SETTINGS, enable_push, file split)
- HTTP/3+QUIC: 78.2 → HIGH fixes applied (err_is_fatal bug, 4 function decompositions, race condition)

---

## Spec Compliance Implementation — 2026-03-26

### Phase 1: Critical RFC Violations (COMPLETE)
- P1-1: H3 error codes enum (17 codes 0x0100-0x0110) + str() + usage in close()/goaway()
- P1-2: Unknown frame/stream type handling — silently ignore per RFC 7540 §5.5 / RFC 9114 §6.2.3/§7.2.8
- P1-3: Malformed request detection — validate :method/:path, reject unknown pseudo-headers, ordering
- P1-4: Connection-specific header filtering — reject Connection/Keep-Alive/Proxy-Connection/Transfer-Encoding/Upgrade
- P1-5: Header field lowercase validation (HTTP/3)

### Phase 2: HTTP/2 Spec Compliance (COMPLETE)
- P2-1: Stream state machine enforcement (can_send/can_recv/next_on_send/next_on_recv)
- P2-2: Client-side INITIAL_WINDOW_SIZE adjustment (delta applied to all existing streams)
- P2-3: Cookie header compression (split/join per RFC 7540 §8.1.2.5)
- P2-4: Pseudo-header validation (request + response)

### Phase 3: HTTP/3 Spec Compliance (COMPLETE)
- P3-1: ALPN verification documented (CryptoContext limitation)
- P3-2: FIN/ordering enforcement (DATA before HEADERS → H3_FRAME_UNEXPECTED)
- P3-3: GOAWAY 2-phase shutdown (max stream ID first, then final)
- P3-4: SETTINGS expansion (3 params) + duplicate rejection
- P3-5: QPACK capacity enforcement + dynamic table resize
- P3-6: Request cancel wiring (cancel_request → reset_stream + H3_REQUEST_CANCELLED)

### Phase 4: QUIC Transport (COMPLETE)
- P4-1: CID-based packet matching (extract_dcid_from_packet + hex CID map key)
- P4-2: QPACK capacity negotiation (peer SETTINGS → encoder capacity → table resize)

### Test Results
- HTTP/2: 12 passed, 12 total
- HTTP/3: 6 passed, 6 total
- QUIC: 5 passed, 5 total
- **Total: 23 passed, 23 total**

### New Files Created
- `v2/validation.v` (73 lines) — request/response header validation
- `v2/validation_test.v` (335+ lines) — validation tests
- `v2/cookie.v` — Cookie header split/join
- `v2/cookie_test.v` — Cookie tests
- `v2/stream_state_test.v` (166 lines) — stream state machine tests
- `v3/errors.v` — H3ErrorCode enum
- `v3/errors_test.v` — H3 error code tests
