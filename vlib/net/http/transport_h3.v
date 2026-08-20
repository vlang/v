// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.quic
import os
import sync

// This file wires H3MuxConn (h3_mux_conn.v) into Transport (transport.v) as
// the pooled path for `enable_http3: true` requests -- Phase 12d, the final
// sub-phase of the HTTP/3 client-wiring work (vlang/v#27675). Unlike h2's
// own always-on ALPN-probing dial, h3 is never auto-detected: req.enable_
// http3's own doc comment states this is a hard per-request commitment,
// with no fallback to HTTP/1.1/2 if the QUIC dial or handshake fails. UDP
// has no fast-fail signal the way a closed TCP port does -- a firewalled or
// simply absent UDP/443 gives silence, so QUIC's own PTO/idle-timeout
// (multi-second) is the only detection mechanism, and racing every
// `https://` request against that by default would regress the common case
// of an origin that doesn't speak h3 yet. Real happy-eyeballs-style racing
// is a legitimately separate follow-up feature, not built here.

// h3_round_trip_attempts bounds how many times h3_round_trip may retry:
// once against the pooled connection (if usable), once against a freshly
// (re)dialed one. Mirrors h2_round_trip_attempts exactly -- at most one
// h3_conns entry per key, so no larger bound is needed.
const h3_round_trip_attempts = 2

// h3_fresh_conn_request_attempts bounds how many times a request against a
// just-(re)dialed H3MuxConn may be retried on h3_err_retryable_code before
// giving up. This is a DIFFERENT retry than h3_round_trip_attempts: that
// one retries against a *different* connection (the pooled one was dead,
// so dial a fresh one); this one retries the SAME connection, because the
// very first request admitted on a brand-new connection can race the QUIC
// handshake itself -- start_request's open_request_stream/send_request_
// headers calls fail (safely, retryably) until the peer's transport
// parameters and this connection's own QPACK encoder stream are both
// ready, which needs at least one real round trip after dial(). Each
// retry naturally re-enters do()'s own blocking wait, which is paced by
// driver_loop's own poll cadence (h3_driver_poll_interval), not a busy
// loop, so no explicit sleep is needed here.
const h3_fresh_conn_request_attempts = 20

// h3_default_transport_parameters returns this client's own OFFERED QUIC
// transport parameters for every h3 dial. Generous, fixed defaults (not
// derived from req.read_timeout or similar): the dialed QuicConn/H3MuxConn
// is POOLED and shared across many requests with potentially different
// per-request timeouts, so tying connection-level QUIC limits to any one
// request's own settings would be a category error -- matching how h2's
// own ALPN-probing dial similarly does not derive TLS/HTTP2-layer tuning
// from per-request fields beyond what is obviously necessary (verify/cert).
fn h3_default_transport_parameters() quic.QuicTransportParameters {
	return quic.QuicTransportParameters{
		max_idle_timeout:                    30_000
		initial_max_data:                    10_000_000
		initial_max_stream_data_bidi_local:  1_000_000
		initial_max_stream_data_bidi_remote: 1_000_000
		initial_max_stream_data_uni:         1_000_000
		initial_max_streams_bidi:            100
		initial_max_streams_uni:             100
	}
}

// H3DialCall singleflights the QUIC dial for one pool key: concurrent
// requests to a not-yet-dialed h3 origin share one dial and handshake
// instead of each racing their own. Mirrors H2DialCall (transport_h2.v)
// exactly, minus the is_h2/ssl_probe/vsc_probe fields H2DialCall needs for
// its own ALPN-probe-then-maybe-fall-back-to-h1 shape -- h3 has no such
// ambiguity: a dial either becomes a real H3MuxConn or fails outright.
@[heap]
struct H3DialCall {
mut:
	mu       &sync.Mutex = sync.new_mutex()
	cv       &sync.Cond  = unsafe { nil }
	done     bool
	conn     &H3MuxConn = unsafe { nil }
	err      string
	err_code int
}

fn new_h3_dial_call() &H3DialCall {
	mu := sync.new_mutex()
	return &H3DialCall{
		mu: mu
		cv: sync.new_cond(mu)
	}
}

// h3_round_trip performs one HTTP/3 request for an h3-enabled https
// request. Prefers an existing pooled, multiplexed H3MuxConn; when none is
// usable it falls to a singleflight dial (h3_dial_and_do). Always returns a
// complete result or a genuine error -- there is no "not handled, try
// something else" outcome, unlike h2_round_trip's own h1-fallback branches.
fn (mut t Transport) h3_round_trip(req &Request, key string, method Method, host string, port int, path string, data string, header Header) !Response {
	if req.cert != '' || req.cert_key != '' {
		// v1's TLS 1.3 stack rejects any CertificateRequest outright (no
		// client-certificate support yet) -- fail fast with a clear error
		// rather than silently ignoring the caller's mutual-TLS intent.
		return error('http.transport: HTTP/3 (enable_http3) does not support client certificates (req.cert/req.cert_key) in this version')
	}
	for _ in 0 .. h3_round_trip_attempts {
		t.mu.lock()
		mut conn := t.h3_conns[key] or { &H3MuxConn(unsafe { nil }) }
		t.mu.unlock()
		if conn != unsafe { nil } && conn.is_idle_expired(t.idle_timeout) {
			// Mirrors h2_round_trip's identical idle-expiry retirement: a
			// pooled h3 connection that has sat idle past idle_timeout must
			// be retired even though it still has capacity, matching h1's
			// own documented idle_timeout contract.
			t.mu.lock()
			existing := t.h3_conns[key] or { &H3MuxConn(unsafe { nil }) }
			won_removal := voidptr(existing) == voidptr(conn)
			if won_removal {
				t.h3_conns.delete(key)
				t.h3_dial_id.delete(key)
			}
			t.mu.unlock()
			if won_removal {
				// Only the caller that actually removed the entry owns
				// retiring it -- mirrors h2_round_trip's identical race
				// guard (a concurrent racer that already claimed this
				// connection also calls shutdown_when_idle()+release()
				// itself; release() is not idempotent against a double
				// call the way shutdown_when_idle() is).
				conn.shutdown_when_idle()
				conn.release()
			}
			return t.h3_dial_and_do(req, key, method, host, port, path, data, header)!
		}
		if conn != unsafe { nil } && conn.can_take_new_request() {
			resp := do_h3(req, mut conn, method, host, port, path, data, header) or {
				if err.code() == h3_err_retryable_code {
					// The connection provably never took this request (h3_
					// err_retryable_code's own contract, h3_mux_conn.v) --
					// it raced closed/goaway/shutting_down between the
					// check above and do()'s own admission check. Loop
					// back: the next iteration finds it no longer usable
					// and falls into the dial path below.
					continue
				}
				return err
			}
			return resp
		}
		return t.h3_dial_and_do(req, key, method, host, port, path, data, header)!
	}
	return error('http.transport: h3 request failed after retrying on a fresh connection')
}

// h3_dial_and_do performs (or awaits) the singleflight QUIC dial for `key`,
// then completes the original request. Mirrors h2_dial_and_do's overall
// shape (recheck-before-committing, singleflight registration, orphan
// release, idle-cap eviction in the same locked section as registration),
// minus every ALPN-probe-outcome branch h2's version needs: an h3 dial
// either succeeds into a real H3MuxConn or fails outright, with no
// "turned out to be h1" case to fall back through.
fn (mut t Transport) h3_dial_and_do(req &Request, key string, method Method, host string, port int, path string, data string, header Header) !Response {
	t.mu.lock()
	if mut existing := t.h3_dialing[key] {
		t.mu.unlock()
		return t.h3_await_dial(mut existing, req, method, host, port, path, data, header)
	}
	// Recheck pooled state before committing to a new dial: h3_round_trip's
	// own check ran under a separate, already-released lock acquisition, so
	// a concurrent dial can have finished and populated h3_conns[key] in the
	// window between that check and this one (Codex P2, vlang/v#27643
	// pullrequestreview-4631763931, discussion 3525390895 -- the same race
	// h2_dial_and_do's identical recheck was added for).
	mut pooled := t.h3_conns[key] or { &H3MuxConn(unsafe { nil }) }
	t.mu.unlock()
	if pooled != unsafe { nil } && pooled.can_take_new_request() {
		mut pooled_retryable := false
		resp := do_h3(req, mut pooled, method, host, port, path, data, header) or {
			if err.code() != h3_err_retryable_code {
				return err
			}
			pooled_retryable = true
			Response{}
		}
		if !pooled_retryable {
			return resp
		}
	}

	t.mu.lock()
	if mut existing := t.h3_dialing[key] {
		t.mu.unlock()
		return t.h3_await_dial(mut existing, req, method, host, port, path, data, header)
	}
	mut call := new_h3_dial_call()
	t.h3_dialing[key] = call
	t.mu.unlock()

	ca_pem := if req.in_memory_verification {
		req.verify
	} else if req.verify != '' {
		os.read_file(req.verify) or {
			return t.h3_dial_failed(key, mut call,
				error('http.transport: failed to read CA bundle ${req.verify}: ${err.msg()}'))
		}
	} else {
		''
	}

	transport, _, h3 := h3_dial_udp_and_open(host, host, port, ca_pem, ['h3'],
		h3_default_transport_parameters()) or { return t.h3_dial_failed(key, mut call, err) }

	t.mu.lock()
	t.h3_dial_seq++
	dial_id := t.h3_dial_seq
	// A connection may already be registered under this key -- e.g. it was
	// idle-expired and superseded between this dialer starting and now.
	// Grab it here (under t.mu, where h3_conns is read/written) but release
	// it only after t.mu is released below: release() never synchronously
	// drives teardown for H3MuxConn (unlike H2MuxConn.release), but the same
	// "don't call out while holding the pool lock" discipline is kept for
	// parity with h2_dial_and_do's identical structure.
	mut orphan := t.h3_conns[key] or { &H3MuxConn(unsafe { nil }) }
	// on_retired lets a self-terminated connection (idle QUIC max_idle_
	// timeout, a fatal UDP read/write/h3 error) remove itself from the pool
	// instead of leaving a dead, still-registered entry for evict_oldest_
	// idle_locked's h3 scan to mistake for a real idle candidate (its own
	// active_streams == 0 looks exactly like a healthy idle connection).
	// dial_id-guarded exactly like h2_dial_and_do's identical close_
	// transport closure a few lines above: without the guard, this
	// callback firing after a NEWER dial has already superseded this key
	// would wrongly delete the new entry instead of a stale one.
	on_retired := fn [mut t, key, dial_id] () {
		t.mu.lock()
		if t.h3_dial_id[key] or { 0 } == dial_id {
			t.h3_conns.delete(key)
			t.h3_dial_id.delete(key)
		}
		t.mu.unlock()
	}
	mut mux := new_h3_mux_conn(transport, h3, on_retired)
	t.h3_conns[key] = mux
	t.h3_dial_id[key] = dial_id
	t.h3_dialing.delete(key)
	// Evicting for the cap happens in the same locked section as the
	// registration above, excluding this brand-new entry from its own
	// candidacy (mirrors h2_dial_and_do's identical just_registered_h2_key
	// exclusion) -- do_h3 hasn't run yet, so its active_streams is still 0
	// and it would otherwise look idle enough to evict as its own victim.
	mut evicted := t.evict_oldest_idle_locked('', key)
	t.mu.unlock()

	call.mu.lock()
	call.done = true
	call.conn = mux
	call.mu.unlock()
	call.cv.broadcast()
	if orphan != unsafe { nil } {
		// shutdown_when_idle() is REQUIRED here, not just release(): unlike
		// H2MuxConn (where release() draining refs to 0 synchronously
		// drives teardown), H3MuxConn.release() is a documented no-op for
		// teardown -- without shutdown_when_idle(), this orphan's driver
		// thread never learns to retire and polls its UDP socket forever,
		// since it has also just been overwritten out of t.h3_conns and so
		// can never be reached by any later shutdown_when_idle() call
		// either.
		orphan.shutdown_when_idle()
		orphan.release()
	}
	if evicted.h1 != unsafe { nil } {
		evicted.h1.close_conn()
	}
	if evicted.h2 != unsafe { nil } {
		evicted.h2.shutdown_when_idle()
		evicted.h2.release()
	}
	if evicted.h3 != unsafe { nil } {
		evicted.h3.shutdown_when_idle()
		evicted.h3.release()
	}

	resp := h3_do_on_fresh_conn(req, mut mux, method, host, port, path, data, header)!
	return resp
}

// h3_dial_failed untracks the in-flight singleflight dial and publishes the
// failure to any waiters, then returns it as the error for the dialer
// itself to propagate. Mirrors h2_dial_failed exactly, including preserving
// dial_err's code (not just its message) so request.v's is_no_need_retry_
// error can still decide whether to give up early.
fn (mut t Transport) h3_dial_failed(key string, mut call H3DialCall, dial_err IError) IError {
	msg := dial_err.msg()
	code := dial_err.code()
	t.mu.lock()
	t.h3_dialing.delete(key)
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

// h3_await_dial waits for an in-flight singleflight dial to finish, then
// completes the original request over the freshly pooled H3MuxConn.
// Mirrors h2_await_dial, minus the is_h2-false/h1-fallback branch (and the
// `key` parameter that branch needs) h2's version requires.
fn (mut t Transport) h3_await_dial(mut call H3DialCall, req &Request, method Method, host string, port int, path string, data string, header Header) !Response {
	call.mu.lock()
	for !call.done {
		call.cv.wait()
	}
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
	// This request may be the very first one this singleflight dial's
	// connection has ever seen (or close to it) -- the same "connection
	// not ready yet" race h3_dial_and_do's own final call handles, see
	// h3_do_on_fresh_conn's own doc comment.
	return h3_do_on_fresh_conn(req, mut conn, method, host, port, path, data, header)
}

// h3_do_on_fresh_conn runs one request against a just-(re)dialed
// H3MuxConn, retrying up to h3_fresh_conn_request_attempts times on
// h3_err_retryable_code. Needed specifically for a fresh dial: the very
// first request(s) admitted on a brand-new connection can race the QUIC
// handshake itself (start_request's open_request_stream/send_request_
// headers calls fail -- safely, retryably -- until the peer's transport
// parameters and this connection's own QPACK encoder stream are both
// ready, which needs at least one real round trip after dial()).
// Deliberately distinct from h3_round_trip's own retry loop (which retries
// against a DIFFERENT, freshly redialed connection because the pooled one
// is dead): this one retries the SAME connection, because the failure here
// means "not ready yet", not "this connection is dead".
fn h3_do_on_fresh_conn(req &Request, mut conn H3MuxConn, method Method, host string, port int, path string, data string, header Header) !Response {
	for _ in 0 .. h3_fresh_conn_request_attempts {
		resp := do_h3(req, mut conn, method, host, port, path, data, header) or {
			if err.code() == h3_err_retryable_code {
				continue
			}
			return err
		}
		return resp
	}
	// This exhausted-retries outcome is exactly what h3_err_retryable_code
	// exists for: the request provably never reached the server (every
	// attempt failed before the connection was ever ready), so it must
	// stay classified as safe-to-retry for any OUTER retry logic (e.g.
	// request.v's max_retries loop) -- a bare codeless error() here would
	// silently downgrade it to a hard, non-retryable failure.
	return h3_retryable_error('request to a freshly dialed connection failed after ${h3_fresh_conn_request_attempts} attempts')
}

// do_h3 runs one request over an established, pooled H3MuxConn and converts
// the result to a net.http Response -- the h3 counterpart of do_h2
// (transport_h2.v). Reuses h2_client.v's h2_authority helper directly (RFC
// 9114 §4.1.1 mirrors RFC 9113 here, so :authority construction needs no
// h3-specific version).
fn do_h3(req &Request, mut conn H3MuxConn, method Method, host string, port int, path string, data string, header Header) !Response {
	h3req := req.to_h3_request(method, h2_authority(host, port), path, data, header)
	h3resp := conn.do(h3req)!
	if req.on_finish != unsafe { nil } {
		req.on_finish(req, u64(h3resp.body.len))!
	}
	return h3_response_to_http(h3resp)
}
