// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.ssl
import sync

// This file wires H2MuxConn (h2_mux_conn.v) into Transport (transport.v) as a
// pooled, multiplexed alternative to the one-shot HTTP/2 client (h2_conn.v):
// all concurrent requests to an h2-capable origin share one H2MuxConn instead
// of each dialing (and ALPN-negotiating) their own connection.

// h2_round_trip_attempts bounds how many times h2_round_trip may retry: once
// against the pooled connection (if usable), once against a freshly (re)dialed
// one. Unlike the h1 pool's per-key list, there is at most one h2_conns entry
// per key, so no larger bound is needed.
const h2_round_trip_attempts = 2

// H2DialCall singleflights the ALPN-probing dial for one pool key: concurrent
// requests to a not-yet-dialed (or not-yet-known-to-be-h2) origin share one
// dial and TLS handshake instead of each racing their own. This codebase has
// no channel-based singleflight pattern to reuse, so this follows the same
// mutex+condition-variable style already used for H2MuxStream (h2_mux_conn.v).
@[heap]
struct H2DialCall {
mut:
	mu       &sync.Mutex = sync.new_mutex()
	cv       &sync.Cond  = unsafe { nil }
	done     bool
	is_h2    bool
	conn     &H2MuxConn = unsafe { nil }
	err      string
	err_code int
}

fn new_h2_dial_call() &H2DialCall {
	mu := sync.new_mutex()
	return &H2DialCall{
		mu: mu
		cv: sync.new_cond(mu)
	}
}

// h2_round_trip performs one HTTP/2 request for an h2-enabled https request.
// It is called from round_trip only when the pool key is not already known to
// be http/1.1-only (key_proto[key] != 1). It prefers an existing pooled,
// multiplexed H2MuxConn; when none is usable it falls to a singleflight
// ALPN-probing dial (h2_dial_and_do), which itself falls back to the ordinary
// pooled http/1.1 path when the origin turns out not to speak h2 — so this
// always returns a complete result, never a "not handled, try h1" signal.
fn (mut t Transport) h2_round_trip(req &Request, key string, raw string, method Method, host string, port int, path string, data string, header Header) !Response {
	for _ in 0 .. h2_round_trip_attempts {
		t.mu.lock()
		mut conn := t.h2_conns[key] or { &H2MuxConn(unsafe { nil }) }
		t.mu.unlock()
		if conn != unsafe { nil } && conn.is_idle_expired(t.idle_timeout) {
			// Mirrors Transport.checkout's h1_idle expiry check: a pooled h2
			// connection that has sat idle past idle_timeout must be retired
			// even though it still has stream capacity, matching h1's own
			// documented idle_timeout contract (Codex P2, vlang/v#27643
			// pullrequestreview-4631763931, discussion 3525390896).
			t.mu.lock()
			existing := t.h2_conns[key] or { &H2MuxConn(unsafe { nil }) }
			won_removal := voidptr(existing) == voidptr(conn)
			if won_removal {
				t.h2_conns.delete(key)
				t.h2_dial_id.delete(key)
			}
			t.mu.unlock()
			if won_removal {
				// Only the caller that actually removed the entry owns retiring
				// it: a concurrent racer (another h2_round_trip call on the same
				// key, or Transport.close_idle()) that already claimed this
				// connection also calls shutdown_when_idle()+release() itself.
				// shutdown_when_idle() is idempotent, but release() is not --
				// calling it again here would over-decrement H2MuxConn.refs
				// (self-caught in review, vlang/v#27643
				// pullrequestreview-4636271901).
				conn.shutdown_when_idle()
				conn.release()
			}
			return t.h2_dial_and_do(req, key, raw, method, host, port, path, data, header)!
		}
		if conn != unsafe { nil } && conn.can_take_new_request() {
			// Fast path only: do()/do_on_stream re-check closed/goaway/
			// shutting_down/the stream limit under their own smu right before
			// registering the stream, closing the TOCTOU window between this
			// check and the actual send. A stale check here (e.g. GOAWAY lands in
			// the instant after can_take_new_request() returns) is caught there
			// and surfaces as h2_err_retryable_code below — nothing to fix here.
			resp := do_h2(req, mut conn, method, host, port, path, data, header) or {
				if err.code() == h2_err_retryable_code {
					// The request provably never reached the server on this
					// connection (h2_err_retryable_code's contract). Loop back:
					// the next iteration will find the connection no longer
					// usable (closed/goaway) and fall into the dial path below.
					continue
				}
				return err
			}
			return resp
		}
		return t.h2_dial_and_do(req, key, raw, method, host, port, path, data, header)!
	}
	return error('http.transport: h2 request failed after retrying on a fresh connection')
}

// h2_dial_and_do performs (or awaits) the singleflight ALPN-probing dial for
// `key`, then completes the original request:
//   - ALPN negotiates h2: registers a pooled H2MuxConn — releasing any
//     orphaned prior entry under the same key first — and runs the request
//     over it.
//   - ALPN negotiates http/1.1 (or the peer doesn't support ALPN): memoizes
//     key_proto[key] = 1, and completes the request over the just-dialed
//     connection directly (as an ordinary pooled H1PooledConn) rather than
//     discarding it and dialing again — a second dial would double every
//     origin's first handshake and break the existing accept-count contract
//     h1 pooling tests rely on. A waiter that arrived during this dial has no
//     connection of its own to reuse, so it falls back to its own independent
//     call to the existing pooled http/1.1 path.
//   - the dial itself fails outright: every waiter (this caller included)
//     observes that same failure.
fn (mut t Transport) h2_dial_and_do(req &Request, key string, raw string, method Method, host string, port int, path string, data string, header Header) !Response {
	t.mu.lock()
	if mut existing := t.dialing[key] {
		t.mu.unlock()
		return t.h2_await_dial(mut existing, req, key, raw, method, host, port, path, data, header)
	}
	// Recheck pooled state before committing to a new dial: h2_round_trip's
	// own check ran under a separate, already-released lock acquisition, so
	// a concurrent dial can have finished and populated h2_conns[key] (or
	// determined key_proto[key] == 1) in the window between that check and
	// this one. Racing straight into a new H2DialCall here would perform a
	// redundant TLS/ALPN handshake and orphan the connection that just won
	// (Codex P2, vlang/v#27643 pullrequestreview-4631763931, discussion
	// 3525390895).
	mut pooled := t.h2_conns[key] or { &H2MuxConn(unsafe { nil }) }
	proto := t.key_proto[key] or { 0 }
	t.mu.unlock()
	if pooled != unsafe { nil } && pooled.can_take_new_request() {
		mut pooled_retryable := false
		resp := do_h2(req, mut pooled, method, host, port, path, data, header) or {
			if err.code() != h2_err_retryable_code {
				return err
			}
			// The connection raced GOAWAY/closed between the check above and
			// do_on_stream's own registration -- mirrors h2_round_trip's own
			// identical retry for this exact TOCTOU (see its comment): fall
			// through to the dialing-state checks below instead of
			// propagating a spurious failure for a connection that never
			// actually took the request.
			pooled_retryable = true
			Response{}
		}
		if !pooled_retryable {
			return resp
		}
	}
	if proto == 1 {
		return t.h2_use_h1_pool_or_dial(req, key, raw, method, host, port, path, data, header)
	}

	t.mu.lock()
	if mut existing := t.dialing[key] {
		t.mu.unlock()
		return t.h2_await_dial(mut existing, req, key, raw, method, host, port, path, data, header)
	}
	mut call := new_h2_dial_call()
	t.dialing[key] = call
	t.mu.unlock()

	mut ssl_conn := ssl.new_ssl_conn(
		verify:                 req.verify
		cert:                   req.cert
		cert_key:               req.cert_key
		validate:               req.validate
		in_memory_verification: req.in_memory_verification
		alpn_protocols:         ['h2', 'http/1.1']
	) or { return t.h2_dial_failed(key, mut call, err) }
	ssl_conn.dial(host, port) or { return t.h2_dial_failed(key, mut call, err) }
	if req.read_timeout > 0 {
		ssl_conn.set_read_timeout(req.read_timeout)
	}

	if ssl_conn.negotiated_alpn() != 'h2' {
		t.mu.lock()
		t.key_proto[key] = 1
		t.dialing.delete(key)
		t.mu.unlock()
		// Wake waiters as soon as the protocol is known, BEFORE running this
		// caller's own exchange below. HTTP/1.1 has no safe way to share one
		// connection across two concurrent requests, so waiting for this
		// exchange to fully finish (as before) head-of-line-blocks every
		// other concurrent first-time caller to this origin behind however
		// long THIS one's response takes — exactly what RFC 9112 §9.4 says
		// multiple connections exist to avoid. Waiters now independently
		// race for the h1 pool via h2_use_h1_pool_or_dial, the same helper
		// h2_dial_and_do's own registration-time recheck uses below (Codex
		// P2, vlang/v#27643 pullrequestreview-4631763931, discussion
		// 3525390892).
		call.mu.lock()
		call.done = true
		call.mu.unlock()
		call.cv.broadcast()
		mut conn := &H1PooledConn{
			key: key
			ssl: ssl_conn
		}
		resp, reusable := conn.exchange(req, raw) or {
			conn.close_conn()
			// Past the TLS handshake the request bytes may have been (partially)
			// written; a non-idempotent method must not be replayed by the outer
			// retry loop, mirroring tls_fresh_round_trip's own error handling.
			if !transport_is_idempotent(method) {
				return error_with_code(err.msg(), transport_err_unsafe_retry)
			}
			return err
		}
		t.maybe_checkin(mut conn, header, reusable, resp)
		return resp
	}

	mut pt := new_h2_pooled_transport(mut ssl_conn)
	t.mu.lock()
	t.h2_dial_seq++
	dial_id := t.h2_dial_seq
	// A connection may already be registered under this key — e.g. it was
	// GOAWAY'd while it had zero in-flight streams, so its refcount never
	// dropped to trigger teardown on its own. Grab it here (under t.mu, where
	// h2_conns is read/written) but release it only after t.mu is released
	// below: release() can synchronously drive teardown_transport(), which
	// calls this closure's own close_transport, which itself takes t.mu —
	// calling it here would self-deadlock.
	mut orphan := t.h2_conns[key] or { &H2MuxConn(unsafe { nil }) }
	close_transport := fn [mut t, key, dial_id, mut pt] () {
		t.mu.lock()
		if t.h2_dial_id[key] or { 0 } == dial_id {
			t.h2_conns.delete(key)
			t.h2_dial_id.delete(key)
		}
		t.mu.unlock()
		pt.close()
	}
	mut mux := new_h2_mux_conn(pt, close_transport)
	t.h2_conns[key] = mux
	t.h2_dial_id[key] = dial_id
	t.key_proto[key] = 2
	t.dialing.delete(key)
	// Evicting for the cap happens in the same locked section as the
	// registration above, so the count it sees already includes this new
	// entry. `key` is passed so the scan excludes it explicitly: it would
	// otherwise look idle (active_streams is still 0 — do_h2 hasn't run yet)
	// and, if it is the only idle candidate found, get evicted as its own
	// registration's victim. The victim may come from either pool: a budget
	// already filled entirely by idle h1 connections must still be freeable
	// here, not just when h2_conns itself has another idle entry.
	mut evicted := t.evict_oldest_idle_locked(key)
	t.mu.unlock()

	call.mu.lock()
	call.done = true
	call.is_h2 = true
	call.conn = mux
	call.mu.unlock()
	call.cv.broadcast()
	if orphan != unsafe { nil } {
		orphan.release()
	}
	if evicted.h1 != unsafe { nil } {
		evicted.h1.close_conn()
	}
	if evicted.h2 != unsafe { nil } {
		evicted.h2.shutdown_when_idle()
		evicted.h2.release()
	}

	resp := do_h2(req, mut mux, method, host, port, path, data, header)!
	return resp
}

// h2_dial_failed untracks the in-flight singleflight dial and publishes the
// failure to any waiters, then returns it as the error for the dialer itself
// to propagate. Mirrors h2_retryable_error's pattern (h2_mux_conn.v) of
// returning a plain IError from within a `!`-returning call site.
//
// Preserves dial_err's code (not just its message): the outer retry loop
// (request.v's is_no_need_retry_error) decides whether to give up early on
// codes like net.err_connect_timed_out / net.err_timed_out_code — collapsing
// a coded dial failure to a plain error() would make a fundamentally hopeless
// failure retry up to max_retries times instead of failing fast, a regression
// from the one-shot h2 path's own dial-error propagation (Codex P2, vlang/v#27643
// pullrequestreview-4627654418, discussion 3521494715).
fn (mut t Transport) h2_dial_failed(key string, mut call H2DialCall, dial_err IError) IError {
	msg := dial_err.msg()
	code := dial_err.code()
	t.mu.lock()
	t.dialing.delete(key)
	t.mu.unlock()
	call.mu.lock()
	call.done = true
	call.err = msg
	call.err_code = code
	call.mu.unlock()
	call.cv.broadcast()
	if code != 0 {
		return error_with_code(msg, code)
	}
	return error(msg)
}

// h2_await_dial waits for an in-flight singleflight dial for `key` to finish,
// then completes the original request the same way the dialer itself would
// have: over the freshly pooled H2MuxConn, or via the existing h1 pool when
// the dial turned out not to negotiate h2. In the latter case the dialer has
// already completed its own probe connection and checked it into h1_idle
// (see h2_dial_and_do), so this checks that pool out first — reusing it costs
// nothing and avoids a wasted ALPN-probing handshake for whichever waiter
// gets there first; only once the pool is empty (no waiter got it, or there
// were none to give) does this fall back to its own independent dial (Codex
// P3, vlang/v#27643 pullrequestreview-4627654418, discussion 3521494719).
fn (mut t Transport) h2_await_dial(mut call H2DialCall, req &Request, key string, raw string, method Method, host string, port int, path string, data string, header Header) !Response {
	call.mu.lock()
	for !call.done {
		call.cv.wait()
	}
	is_h2 := call.is_h2
	mut conn := call.conn
	msg := call.err
	err_code := call.err_code
	call.mu.unlock()
	if msg != '' {
		if err_code != 0 {
			return error_with_code(msg, err_code)
		}
		return error(msg)
	}
	if !is_h2 {
		return t.h2_use_h1_pool_or_dial(req, key, raw, method, host, port, path, data, header)
	}
	return do_h2(req, mut conn, method, host, port, path, data, header)
}

// h2_use_h1_pool_or_dial completes a request already known to be against an
// h1-only origin (key_proto[key] == 1): reuses a pooled H1PooledConn when one
// is checked in, or falls back to an independent dial otherwise. Shared by
// h2_await_dial (a waiter behind a completed singleflight dial) and
// h2_dial_and_do's own registration-time recheck, so both paths treat a
// known-h1 origin identically (Codex P2, vlang/v#27643
// pullrequestreview-4631763931, discussion 3525390892).
fn (mut t Transport) h2_use_h1_pool_or_dial(req &Request, key string, raw string, method Method, host string, port int, path string, data string, header Header) !Response {
	mut h1conn := t.checkout(key)
	if h1conn == unsafe { nil } {
		return t.tls_fresh_round_trip(req, key, raw, method, host, port, path, data, header)
	}
	h1conn.refresh_timeouts(req)
	resp, reusable := h1conn.exchange(req, raw) or {
		h1conn.close_conn()
		// Mirrors round_trip's own stale-pooled-connection handling: a
		// reused connection that fails is drained by falling through to a
		// fresh dial, except a non-idempotent method must not be replayed.
		if !transport_is_idempotent(method) {
			return error_with_code(err.msg(), transport_err_unsafe_retry)
		}
		return t.tls_fresh_round_trip(req, key, raw, method, host, port, path, data, header)
	}
	t.maybe_checkin(mut h1conn, header, reusable, resp)
	return resp
}

// do_h2 runs one request over an established, pooled H2MuxConn and converts
// the result to a net.http Response — the pooled counterpart of h2_exchange
// (backend.c.v), which does the same for the one-shot H2Conn. Must mirror
// h2_exchange's streaming-callback wiring and its req.on_finish call on
// success; this is duplicated rather than shared because h2_exchange's
// signature is tied to the concrete H2Conn type.
fn do_h2(req &Request, mut conn H2MuxConn, method Method, host string, port int, path string, data string, header Header) !Response {
	base := req.to_h2_request(method, h2_authority(host, port), path, data, header)
	on_progress := req.on_progress
	on_progress_body := req.on_progress_body
	mut on_data := H2DataFn(unsafe { nil })
	if on_progress != unsafe { nil } || on_progress_body != unsafe { nil } {
		on_data = fn [req, on_progress, on_progress_body] (chunk []u8, body_so_far u64, body_expected u64, status int) ! {
			if on_progress != unsafe { nil } {
				on_progress(req, chunk, body_so_far)!
			}
			if on_progress_body != unsafe { nil } {
				on_progress_body(req, chunk, body_so_far, body_expected, status)!
			}
		}
	}
	h2req := H2ClientRequest{
		method:               base.method
		scheme:               base.scheme
		authority:            base.authority
		path:                 base.path
		headers:              base.headers
		body:                 base.body
		on_data:              on_data
		stop_copying_limit:   req.stop_copying_limit
		stop_receiving_limit: req.stop_receiving_limit
		read_timeout:         req.read_timeout
	}
	h2resp := conn.do(h2req)!
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(h2resp.body.len))!
	}
	return h2_response_to_http(h2resp)
}
