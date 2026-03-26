# Lessons Learned

Format: `[DATE] CONTEXT — LESSON`

---

## 2026-03-26 — HTTP/2, HTTP/3, QUIC Implementation

### L1: V `net.ssl` has no ALPN result inspection
- **Context:** Attempted to read ALPN-negotiated protocol after TLS handshake.
- **Discovery:** V's `net.ssl` module wraps mbedTLS but exposes no `get_alpn_selected()` method.
- **Rule:** ALPN negotiation in V must use try-and-fail: attempt HTTP/2 connection, fall back to HTTP/1.1 on failure. Document this limitation for API consumers.

### L2: OpenSSL NID_hkdf is 1036, not 1087
- **Context:** HKDF key derivation silently produced wrong keys.
- **Discovery:** The NID constant was incorrectly set to 1087 (a different algorithm). Verified against OpenSSL source: `NID_hkdf = 1036`.
- **Rule:** Always verify OpenSSL NID constants against the actual header files (`obj_mac.h`), not documentation or examples.

### L3: `sync.Once` preferred over lazy `if len == 0` for V globals
- **Context:** Huffman trie global initialization used `if trie.children.len == 0 { build() }`, causing race conditions.
- **Discovery:** V's `sync.Once` provides thread-safe one-time initialization without manual locking.
- **Rule:** For any shared global that needs lazy init, use `sync.Once`. Never use mutable check-and-set patterns.

### L4: V structural typing enables interface abstraction without inheritance
- **Context:** Server needed to handle both `SSLConn` and `TcpConn` for TLS and plaintext connections.
- **Discovery:** V's structural typing means any type matching an interface's method signatures automatically satisfies it — no explicit `implements` keyword needed.
- **Rule:** Design `ServerConn` interfaces with minimal method sets; V structural typing handles the rest.

### L5: QPACK dynamic table requires absolute indexing with O(1) formula
- **Context:** QPACK header decoding produced wrong values for post-base references.
- **Discovery:** The correct lookup formula is `j = abs_index - (insert_count - entries.len)`, converting absolute indices to array positions in O(1).
- **Rule:** Derive index formulas directly from RFC 9204 Section 3.2.5. Do not improvise index arithmetic.

### L6: File extraction (not just function extraction) for SRP compliance
- **Context:** `crypto.v` exceeded 300 lines after function extraction; still violated file-size threshold.
- **Discovery:** Splitting into responsibility-aligned files (`crypto_keys.v`, `crypto_aead.v`, `crypto_hkdf.v`, `crypto_context.v`, `crypto.v`) resolved both SRP and file-size constraints.
- **Rule:** When a file exceeds 300 lines, first check if it has multiple responsibilities. If so, extract into separate files by responsibility, not just extract functions within the same file.

### L7: ngtcp2 C callbacks require heap-allocated structs
- **Context:** `QuicPathAddrs` allocated on stack was freed before ngtcp2 callback executed.
- **Discovery:** C callbacks in ngtcp2 may fire asynchronously after the V function returns. Stack-allocated structs are invalid by then.
- **Rule:** Any struct passed to ngtcp2 C callbacks must be heap-allocated. Ensure cleanup happens in the callback or via explicit deferred free.

### L8: Crypto functions >50 lines are industry-accepted but need documentation
- **Context:** AES-GCM encrypt/decrypt functions exceeded the 50-line function limit.
- **Discovery:** Crypto audit standards prefer contiguous, non-abstracted crypto code for easier review. Splitting crypto primitives across multiple functions can obscure the security-critical flow.
- **Rule:** Crypto functions may exceed 50 lines when auditability requires it. Add a comment explaining why, and keep them as close to 50 lines as feasible.

### L9: Subagent empty-text is a known failure mode — retry resolves it
- **Context:** Subagent tasks occasionally returned empty text with no error.
- **Discovery:** This is a transient failure in the multi-agent orchestration layer.
- **Rule:** Build retry logic (max 3 attempts) into orchestration for subagent tasks. Log the failure for pattern detection.

### L10: `extract_packet_number` requires header protection removal first
- **Context:** QUIC packet number extraction returned garbage values.
- **Discovery:** RFC 9001 Section 5.4 requires header protection to be removed before extracting the packet number. Without access to the header protection key, a counter-based fallback is necessary.
- **Rule:** Implement packet number extraction with a fallback path: prefer header-protection-removal, fall back to monotonic counter when keys are unavailable.

### L11: V Structural Typing for Interface Abstraction
- **Context:** Needed a `ServerConn` interface to abstract over both `TcpConn` and `SSLConn` for server TLS support.
- **Discovery:** V's structural typing means any struct with matching method signatures automatically satisfies an interface — no explicit `implements` needed. Used for ServerConn interface that works with both TcpConn and SSLConn.
- **Rule:** Define interfaces with minimal method sets (read/write/close). V structural typing handles polymorphism automatically.

### L12: OpenSSL ALPN Wire Format
- **Context:** Implementing `get_alpn_selected()` required understanding the ALPN protocol negotiation setup.
- **Discovery:** SSL_CTX_set_alpn_protos requires a specific wire format: length-prefixed protocol strings concatenated (e.g., `\x02h2\x08http/1.1`). This is different from the human-readable list.
- **Rule:** Always use wire format for ALPN protos: `[len][proto][len][proto]...`. Return value 0 = success in OpenSSL.

### L13: Ring Buffer for FIFO with O(1) Eviction
- **Context:** QPACK DynamicTable used `array.delete(0)` which is O(n) due to element shifting.
- **Discovery:** Replace array.delete(0) (O(n) shift) with ring buffer (head/count/modular arithmetic) for O(1) front eviction. Pre-allocate capacity to avoid resizing.
- **Rule:** For any FIFO structure requiring front-eviction, use ring buffer pattern: `entries[head % capacity]`, increment head on evict.

## L14: ACT Phase Is Decision, Not Implementation
- **Context**: PDCA workflow
- **Lesson**: ACT phase decides whether to proceed to SCRIBE or loop back to DO. It does NOT directly implement fixes. Implementation belongs in the DO phase.

## L15: CONTINUATION Flood (CVE-2024-27316)
- **Context**: HTTP/2 security
- **Lesson**: CONTINUATION frames without limits allow memory exhaustion. Always enforce frame count + total size limits. This is a known CVE class.

## L16: HeadersFrame.from_frame() Required Before Decode
- **Context**: HTTP/2 server HEADERS handling
- **Lesson**: Raw frame.payload includes PADDED and PRIORITY bytes. Must call HeadersFrame.from_frame() to strip them before HPACK decoding. Chrome always sets PRIORITY flag.

## L17: Mid-Stream SETTINGS Must Be Applied
- **Context**: HTTP/2 client
- **Lesson**: Settings received mid-connection must be applied (not just ACKed). Missing this causes the client to use stale max_frame_size, max_concurrent_streams etc.
