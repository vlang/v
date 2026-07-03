// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.ssl
import sync
import time

// h2_pooled_io_timeout bounds how long a single locked call into the wrapped
// SSLConn's read() may internally retry (the backend's own WANT_READ loop on a
// partial TLS record) before giving up with the backend's timeout error. This
// is what bounds worst-case io_mu hold time on the read side: the outer
// unlocked poll below only avoids taking the lock when nothing is pending; a
// locked read can still internally retry for up to this long once entered,
// since SSLConn.read() loops on its configured read_timeout (set to this value
// by new_h2_pooled_transport).
const h2_pooled_io_timeout = 200 * time.millisecond

// h2_pooled_poll_interval is the outer unlocked poll granularity: how long one
// wait_for_read/wait_for_write call blocks before reporting a timeout. Read
// timeouts are propagated to H2MuxConn's reader, which classifies them as
// benign wake-ups (is_transport_timeout_error in h2_mux_conn.v) and uses them
// to run its shutdown checks — so this is also the reader's retirement-check
// cadence.
const h2_pooled_poll_interval = 500 * time.millisecond

// h2_pooled_write_stall_limit caps how long write() keeps waiting for the
// socket to become writable before failing the connection. Mirrors the
// backends' own overall write deadline (SSLConn duration, 30s default): a peer
// that stops draining its receive buffer for this long is wedged, and the
// writer (which holds H2MuxConn's wmu) must not be parked forever.
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
// serialized behind io_mu. Waiting for socket readiness
// (wait_for_read/wait_for_write — a raw select() on the socket handle that
// never touches the TLS context) stays OUTSIDE the lock, so an idle reader
// polling for data never blocks a writer, and vice versa.
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
	conn   ssl.SSLConn
	io_mu  &sync.Mutex = sync.new_mutex()
	closed bool
}

// new_h2_pooled_transport wraps an already-dialled, ALPN-negotiated (`h2`)
// ssl.SSLConn for use as an H2MuxConn's H2Transport. It sets a short read
// timeout on the connection so a locked read() call cannot block the writer
// for long. The caller must not use `conn` directly afterwards — the returned
// adapter owns it exclusively.
fn new_h2_pooled_transport(mut conn ssl.SSLConn) &H2PooledTransport {
	conn.set_read_timeout(h2_pooled_io_timeout)
	return &H2PooledTransport{
		conn: conn
	}
}

// read implements H2Transport. Called only by H2MuxConn's single background
// reader thread (see read_loop in h2_mux_conn.v). Both the unlocked
// readiness-wait timeout and the locked read's internal timeout (e.g. a
// partial TLS record stalling past read_timeout) are propagated — the mux
// reader classifies them as benign and re-polls (see the error contract on
// H2PooledTransport).
fn (mut t H2PooledTransport) read(mut buf []u8) !int {
	t.conn.wait_for_read(h2_pooled_poll_interval)!
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
		n := t.conn.write(buf) or {
			t.io_mu.unlock()
			return err
		}
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
