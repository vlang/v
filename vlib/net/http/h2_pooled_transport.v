// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.ssl
import sync
import time

// h2_pooled_io_timeout bounds how long read() may hold io_mu: it is the
// backend's configured read_timeout (set by new_h2_pooled_transport), so
// SSLConn.read()'s own internal WANT_READ retry loop gives up and returns a
// timeout error after at most this long. read() has no outer readiness poll
// (see its doc comment for why), so this is the ONLY bound on how long an
// idle reader can hold io_mu and make a writer wait — kept short for exactly
// that reason, not because a longer internal retry would be incorrect.
//
// Both mbedTLS and OpenSSL's write_ptr (vlib/net/mbedtls/ssl_connection.c.v,
// vlib/net/openssl/ssl_connection.c.v) reuse this SAME configured duration for
// their OWN internal WANT_READ/WANT_WRITE retry loop during conn.write() — it
// is not read-specific at the backend level. write() below widens it to
// h2_pooled_write_stall_limit for the duration of each conn.write() call and
// restores it immediately after, so a write in progress gets the intended
// budget instead of this short read-poll interval (Codex P2, vlang/v#27643
// pullrequestreview-4628439062, discussion 3522170277).
const h2_pooled_io_timeout = 50 * time.millisecond

// h2_pooled_poll_interval is the outer unlocked poll granularity: how long one
// wait_for_read/wait_for_write call blocks before reporting a timeout. Read
// timeouts are propagated to H2MuxConn's reader, which classifies them as
// benign wake-ups (is_transport_timeout_error in h2_mux_conn.v) and uses them
// to run its shutdown checks — so this is also the reader's retirement-check
// cadence.
const h2_pooled_poll_interval = 500 * time.millisecond

// h2_pooled_write_stall_limit caps how long write() keeps waiting for the
// socket to become writable before failing the connection. It is ALSO the
// duration write() installs on the wrapped conn for the span of each
// conn.write() call (see h2_pooled_io_timeout's doc comment): a peer that
// stops draining its receive buffer for this long is wedged, and the writer
// (which holds H2MuxConn's wmu) must not be parked forever, but must get this
// full budget rather than the much shorter read-poll interval.
const h2_pooled_write_stall_limit = 30 * time.second

// H2PooledTransport adapts a pooled ssl.SSLConn into the H2Transport interface
// (h2_conn.v) so H2MuxConn's background reader thread and its request-thread
// writers can safely share one TLS connection. A raw TLS context must not be
// read and written concurrently from two threads: mbedTLS provides no
// internal synchronization whatsoever for a single mbedtls_ssl_context's
// read/write reentrancy (its MBEDTLS_THREADING_C mutexes only ever protect
// unrelated shared-global resources — readdir, gmtime, PSA key-slots/RNG).
// OpenSSL's SSL_read/SSL_write do support one-reader-thread/one-writer-thread
// on a single SSL object natively, but this adapter is used uniformly for
// both backends rather than branching on `$if use_openssl`: OpenSSL's
// server-side TLS path in this codebase is a stub, so mbedTLS must keep
// working regardless, and a second, much-less-tested concurrency model is not
// worth the marginal mutex overhead it would save (real-world HTTP/2 latency
// is dominated by network RTT/HPACK/app processing, not TLS-call overhead).
//
// All actual TLS-library calls (read()/write() on the wrapped conn) are
// serialized behind io_mu. On the write side, waiting for socket readiness
// (wait_for_write — a raw select() on the socket handle that never touches
// the TLS context) stays OUTSIDE the lock, so a writer blocked on backpressure
// never holds up the reader. The read side has no equivalent outer wait (see
// read()'s doc comment: a socket-level poll cannot see TLS-buffered data), so
// an idle reader instead holds io_mu for up to h2_pooled_io_timeout per call —
// kept short precisely so a writer's wait for io_mu stays bounded and small.
//
// Error contract: timeout errors are part of the H2Transport read contract,
// not swallowed here. H2MuxConn.read_loop recognizes them
// (is_transport_timeout_error) as benign wake-ups — the TLS layer keeps its
// partial-record assembly across calls, so nothing is lost — and uses them to
// check reader_should_exit(), which is how a retiring connection's reader
// thread gets to exit. An adapter that looped on timeouts internally would
// starve that check and leak the reader of any shutting_down-but-unreleased
// connection. Write errors after conn.write() has been entered are always
// fatal and never retried here: on an internal write timeout the backend's
// sent count is indeterminate (a record may be partially flushed — see
// last_write_sent = -1 in vlib/net/mbedtls/ssl_connection.c.v write_ptr), so
// re-sending the buffer could duplicate bytes already on the wire and corrupt
// the h2 framing. H2MuxConn treats the failure as connection-fatal
// (note_write_failure), which is the only safe interpretation.
@[heap]
struct H2PooledTransport {
mut:
	conn   &ssl.SSLConn = unsafe { nil }
	io_mu  &sync.Mutex  = sync.new_mutex()
	closed bool
}

// new_h2_pooled_transport wraps an already-dialled, ALPN-negotiated (`h2`)
// ssl.SSLConn for use as an H2MuxConn's H2Transport. It sets a short read
// timeout on the connection so a locked read() call cannot block the writer
// for long. The caller must not use `conn` directly afterwards — the returned
// adapter owns it exclusively.
//
// `conn` is stored BY POINTER, never by value: SSLConn.dial() wires
// mbedtls_ssl_set_bio (and OpenSSL's equivalent SSL_set_bio) to a pointer
// into the struct's OWN address (e.g. mbedtls's `&s.server_fd`). A value copy
// here would leave the copy's internal TLS context pointing at the original
// object's fields — not its own — so later reads/writes and close() would
// operate on stale/orphaned state instead of the live connection (Codex P1,
// vlang/v#27643 pullrequestreview-4631763931, discussion 3525390891). This
// matches the established idiom for a pooled SSLConn elsewhere in this
// module: H1PooledConn.ssl (transport.v) is also `&ssl.SSLConn`.
fn new_h2_pooled_transport(mut conn ssl.SSLConn) &H2PooledTransport {
	conn.set_read_timeout(h2_pooled_io_timeout)
	return &H2PooledTransport{
		conn: conn
	}
}

// read implements H2Transport. Called only by H2MuxConn's single background
// reader thread (see read_loop in h2_mux_conn.v).
//
// Deliberately has NO outer readiness poll before calling the wrapped
// conn.read() (unlike write() below): a TLS library can already hold
// decrypted-but-unconsumed application data internally — mbedtls_ssl_read()/
// SSL_read() return at most the caller's buffer size per call, and any
// remainder from an already-received/decrypted TLS record (e.g. one
// underlying socket read pulled in several records at once) stays buffered
// for the NEXT call, needing no new socket bytes at all. A raw select()-based
// outer poll cannot see that buffered data; if it ran first and this method
// propagated its timeout, a poll that (correctly, from the socket's point of
// view) reports "nothing new" would mask data conn.read() would have
// returned immediately — and since H2MuxConn.read_loop treats a read()
// timeout as a benign wake-up and just retries, the buffered bytes would
// never be drained until unrelated new socket data arrived, or never (Codex
// P1, vlang/v#27643 pullrequestreview-4626225521, discussion 3520259789).
// conn.read() itself always tries a non-blocking-in-effect decode of
// buffered data FIRST, so this is not just correct but also the fast path in
// the common case; it only falls back to its own internal, bounded
// WANT_READ wait (h2_pooled_io_timeout) when there is genuinely nothing
// buffered.
fn (mut t H2PooledTransport) read(mut buf []u8) !int {
	t.io_mu.lock()
	if t.closed {
		t.io_mu.unlock()
		return error('h2 pooled transport: closed')
	}
	n := t.conn.read(mut buf) or {
		t.io_mu.unlock()
		return err
	}
	t.io_mu.unlock()
	return n
}

// write implements H2Transport. H2MuxConn serializes all its writers through
// its own wmu (see write_all_locked and its callers in h2_mux_conn.v), so at
// most one caller ever reaches this method at a time; io_mu here exists to
// mutually exclude that single writer against the reader and against close(),
// not to arbitrate between multiple concurrent writers.
//
// Only the unlocked readiness wait is retried on timeout — at that point
// nothing has entered the TLS stack, so waiting longer is provably safe. Once
// conn.write() has been called, any error (including its internal timeout) is
// propagated: the sent count is indeterminate by then, and retrying could
// duplicate bytes already on the wire (see the error contract above).
//
// conn.write() (write_ptr in both TLS backends) reuses the SAME `duration`
// field that set_read_timeout() configures for its own internal
// WANT_READ/WANT_WRITE retry loop during the call — it is not read-specific
// at the backend level. new_h2_pooled_transport leaves that field at the
// short h2_pooled_io_timeout (needed to bound how long read() holds io_mu), so
// without widening it here, a peer that goes quiet mid-write (backpressure
// stops draining after this call has already entered the TLS stack) would
// have its write fail after ~h2_pooled_io_timeout instead of the intended
// write stall budget — killing an otherwise-healthy, merely-slow connection.
// Widen it to h2_pooled_write_stall_limit for the span of this one call and
// restore it immediately after, on every exit path, so read() keeps its short
// budget the rest of the time.
fn (mut t H2PooledTransport) write(buf []u8) !int {
	deadline := time.now().add(h2_pooled_write_stall_limit)
	for {
		t.conn.wait_for_write(h2_pooled_poll_interval) or {
			if err.code() == net.err_timed_out_code {
				// Socket not writable yet; nothing has been handed to the TLS
				// stack, so keep waiting (bounded) unless the transport was
				// closed under us.
				t.io_mu.lock()
				is_closed := t.closed
				t.io_mu.unlock()
				if is_closed {
					return error('h2 pooled transport: closed')
				}
				if time.now() >= deadline {
					return error('h2 pooled transport: write stalled for ${h2_pooled_write_stall_limit}')
				}
				continue
			}
			return err
		}
		t.io_mu.lock()
		if t.closed {
			t.io_mu.unlock()
			return error('h2 pooled transport: closed')
		}
		t.conn.set_read_timeout(h2_pooled_write_stall_limit)
		n := t.conn.write(buf) or {
			t.conn.set_read_timeout(h2_pooled_io_timeout)
			t.io_mu.unlock()
			return err
		}
		t.conn.set_read_timeout(h2_pooled_io_timeout)
		t.io_mu.unlock()
		return n
	}
	// Dead code, just to satisfy the compiler: the loop above only exits via
	// an explicit return.
	return error('h2 pooled transport: unreachable')
}

// close tears the underlying TLS connection down, holding io_mu across the
// shutdown call so it cannot run concurrently with an in-flight locked
// read()/write() — which would free the TLS context out from under it (a
// use-after-free). A second call finds `closed` already set and returns
// immediately without touching conn again. SSLConn.shutdown() is itself
// idempotent (it returns an early error on a second call, having already set
// its own `opened = false` before freeing anything) — that error is expected
// here and ignored. shutdown() also net.close()s the socket handle, which is
// what wakes a reader or writer blocked in an unlocked wait_for_read/
// wait_for_write select() (net.shutdown alone does not wake select() on
// Windows; closing the handle does).
fn (mut t H2PooledTransport) close() {
	t.io_mu.lock()
	if t.closed {
		t.io_mu.unlock()
		return
	}
	t.closed = true
	t.conn.shutdown() or {}
	t.io_mu.unlock()
}
