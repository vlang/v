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

| Module | Test Files | Status |
|--------|-----------|--------|
| HTTP/2 (`vlib/net/http/v2/`) | 8 | All pass |
| HTTP/3 (`vlib/net/http/v3/`) | 5 | All pass |
| QUIC (`vlib/net/quic/`) | 5 | All pass |
| **Total** | **18** | **18/18 (100%)** |

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
