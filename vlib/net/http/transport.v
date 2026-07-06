// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module http

import net
import net.ssl
import sync
import time

// This file implements connection reuse for the HTTP client: a Transport owns
// a pool of keep-alive connections, keyed by origin and TLS configuration, and
// fetch()/Request.do() route through a process-global default Transport unless
// the request opts out (`disable_connection_reuse`) or uses a proxy. The first
// cut pools HTTP/1.1 connections (plain TCP and TLS); HTTP/2 requests run on
// the existing one-shot driver until the multiplexed H2 connection lands.

// transport_err_unsafe_retry tags a failed exchange on a transport connection
// for a non-idempotent method: the request bytes may have (partially) reached
// the server — the write helpers cannot prove otherwise — so it must not be
// replayed, neither by round_trip's inner loop nor the caller's outer retry
// loop. is_no_need_retry_error recognizes this code so the outer loop honors it.
const transport_err_unsafe_retry = -20013

// H1PooledConn is one keep-alive HTTP/1.1 connection in a Transport pool:
// either a plain TCP connection or a TLS one (exactly one of the two is set).
@[heap]
struct H1PooledConn {
mut:
	key        string
	tcp        &net.TcpConn = unsafe { nil }
	ssl        &ssl.SSLConn = unsafe { nil }
	idle_since time.Time
}

// close_conn shuts the underlying connection down and clears it.
fn (mut c H1PooledConn) close_conn() {
	if c.ssl != unsafe { nil } {
		c.ssl.shutdown() or {}
		c.ssl = unsafe { nil }
	}
	if c.tcp != unsafe { nil } {
		c.tcp.close() or {}
		c.tcp = unsafe { nil }
	}
}

// refresh_timeouts applies the current request's timeouts to a pooled
// connection, which may have been dialled by a request with different ones.
fn (mut c H1PooledConn) refresh_timeouts(req &Request) {
	if c.tcp != unsafe { nil } {
		c.tcp.set_read_timeout(req.read_timeout)
		c.tcp.set_write_timeout(req.write_timeout)
	} else if c.ssl != unsafe { nil } {
		if req.read_timeout > 0 {
			c.ssl.set_read_timeout(req.read_timeout)
		}
	}
}

// exchange runs one request/response over the pooled connection, leaving it
// open. The bool result reports whether the response was precisely framed, so
// the connection can safely carry another request.
fn (mut c H1PooledConn) exchange(req &Request, raw string) !(Response, bool) {
	if c.ssl != unsafe { nil } {
		return req.h1_exchange_ssl(mut c.ssl, raw)
	}
	return req.h1_exchange_tcp(mut c.tcp, raw)
}

// Transport holds the connection pools and reuse policy for HTTP requests. It
// is safe for concurrent use; fetch()/Request.do() share one process-global
// instance (see default_transport).
//
// Note: idle_timeout is enforced lazily on checkout of the same pool key; there
// is no background reaper, so a connection to an origin that is never revisited
// is reclaimed by the global max_idle_conns cap (or close_idle_connections())
// rather than on a timer. A time-based reaper is a planned follow-up.
@[heap]
pub struct Transport {
pub mut:
	// max_idle_conns_per_host caps the idle keep-alive connections retained per
	// pool key (origin + TLS configuration).
	max_idle_conns_per_host int = 4
	// max_idle_conns caps the total idle keep-alive connections kept across all
	// pool keys, bounding file-descriptor use when many distinct origins are
	// fetched. The least-recently-used idle connection is evicted on overflow.
	// 0 disables the global cap.
	max_idle_conns int = 100
	// idle_timeout is how long an idle connection may sit in the pool before it
	// is discarded instead of reused.
	idle_timeout time.Duration = 90 * time.second
mut:
	mu      &sync.Mutex = sync.new_mutex()
	h1_idle map[string][]&H1PooledConn
	// h2_conns holds the one pooled, multiplexed H2MuxConn per pool key (an h2
	// connection multiplexes all concurrent requests to an origin, unlike the
	// h1 pool's per-connection list). h2_dial_id tags which dial "owns" the
	// current h2_conns[key] entry, so a superseded connection's own teardown
	// closure can never evict a newer connection registered under the same
	// key (see h2_dial_and_do in transport_h2.v). dialing singleflights the
	// ALPN-probing dial itself: concurrent first requests to a fresh h2-enabled
	// origin share one dial rather than each racing their own. key_proto
	// memoizes the ALPN result (1 = http/1.1-only, 2 = h2) once known, so a
	// key already proven http/1.1-only skips the h2 path entirely.
	h2_conns    map[string]&H2MuxConn
	h2_dial_seq u64
	h2_dial_id  map[string]u64
	dialing     map[string]&H2DialCall
	key_proto   map[string]u8
}

// new_transport creates an empty Transport with default limits.
pub fn new_transport() &Transport {
	return &Transport{}
}

// Initialized eagerly in _vinit() (before main() and before any thread is
// spawned), so the shared default Transport is fully constructed without a
// concurrent-startup race. Lazy init via sync.Once is unsafe here: Once sets its
// done flag before running the callback, so a second caller can observe "done"
// and read this global while it is still nil (vlang/v#27456).
__global http_transport_default = new_transport()

// default_transport returns the process-global Transport that fetch() and
// Request.do() route through.
pub fn default_transport() &Transport {
	return http_transport_default
}

// close_idle_connections closes every idle pooled connection held by the
// default Transport. Useful before fork()-style process handoffs, or in tests.
pub fn close_idle_connections() {
	mut t := default_transport()
	t.close_idle()
}

// close_idle closes every idle pooled connection held by this Transport,
// unconditionally — including a pooled h2 connection that is currently
// serving requests, mirroring how it already treats h1: this is the pool-wide
// flush used before fork()-style process handoffs and, in tests, to unblock a
// server thread reading a kept-alive connection at teardown (neither can wait
// out idle_timeout). In-flight requests are unaffected: shutdown_when_idle()+
// release() only stop new admissions and drop the pool's own reference;
// drop_ref defers the actual teardown until any remaining in-flight streams
// finish (the same refcount mechanism H2MuxConn already relies on
// internally).
pub fn (mut t Transport) close_idle() {
	mut all := []&H1PooledConn{}
	mut h2_all := []&H2MuxConn{}
	t.mu.lock()
	for _, list in t.h1_idle {
		for c in list {
			all << c
		}
	}
	t.h1_idle.clear()
	for _, conn in t.h2_conns {
		h2_all << conn
	}
	// Untrack every h2 entry now, atomically with collecting it: a connection
	// with an in-flight stream survives one release() call below (its
	// refcount drops but doesn't reach zero, since drop_ref only tears down
	// on the last reference), so if it stayed in h2_conns, a second
	// close_idle() call — a concurrent caller, or simply calling it twice —
	// would find the same connection again and release() it a second time,
	// over-decrementing the refcount below the number of genuinely live
	// references (Codex P2, vlang/v#27643 pullrequestreview-4626225521,
	// discussion 3520259791). Clearing the map here, under the same lock as
	// the collection above, means no caller can ever observe — and therefore
	// never re-release — a connection this call has already claimed.
	t.h2_conns.clear()
	t.h2_dial_id.clear()
	t.mu.unlock()
	for mut c in all {
		c.close_conn()
	}
	for mut c in h2_all {
		// shutdown_when_idle()/release() must run outside t.mu: release() can
		// synchronously drive teardown_transport(), which calls this
		// connection's own close_transport closure, which itself takes t.mu —
		// calling it while still holding t.mu here would self-deadlock.
		c.shutdown_when_idle()
		c.release()
	}
}

// transport_pool_key builds the pool key for a request: connections are only
// shared between requests whose origin and TLS-relevant settings all match.
// enable_http2 is part of the key because it changes what the connection
// advertised via ALPN at dial time: a forced-HTTP/1.1 connection (no ALPN)
// must not satisfy an HTTP/2-enabled request, which would otherwise silently
// never negotiate h2 against that origin while the pooled connection lives.
fn transport_pool_key(req &Request, scheme string, host string, port int) string {
	// enable_http2 only affects https dials (plain http ignores it), so keep
	// the plain-http pool unsplit.
	h2 := scheme == 'https' && req.enable_http2
	// Length-prefix the free-form string fields (host and the TLS paths/PEM
	// blobs) so a value containing the '|' separator cannot collide with a
	// different field split — e.g. cert='a|b',cert_key='c' vs cert='a',
	// cert_key='b|c' — which would let a request reuse a connection dialed with
	// the wrong CA or client certificate. Bools and the int port cannot contain
	// '|', and scheme is a fixed literal, so they need no prefixing.
	return '${scheme}|${pk_part(host)}|${port}|${h2}|${req.validate}|${pk_part(req.verify)}|${pk_part(req.cert)}|${pk_part(req.cert_key)}|${req.in_memory_verification}'
}

// pk_part length-prefixes a string (`len:value`) so concatenated pool-key
// components stay unambiguous regardless of the bytes they contain.
fn pk_part(s string) string {
	return '${s.len}:${s}'
}

// transport_is_idempotent reports whether a request with this method can be
// replayed safely after a failure on a reused connection (RFC 7231 4.2.2).
fn transport_is_idempotent(method Method) bool {
	return method in [.get, .head, .options, .trace, .put, .delete]
}

// response_allows_reuse reports whether the server's response permits keeping
// the connection open for another request.
fn response_allows_reuse(resp &Response) bool {
	if resp.status_code < 200 {
		// 1xx handling stops at the headers; the connection state past them is
		// not tracked, so never reuse.
		return false
	}
	// A server may split Connection across repeated header lines; get() returns
	// only the first, so join all values before scanning for the close token.
	conn_tokens := resp.header.values(.connection).join(',').to_lower()
	if conn_tokens.contains('close') {
		return false
	}
	ver := resp.version()
	if ver == .v1_0 {
		// HTTP/1.0 defaults to close; reuse only with an explicit keep-alive.
		return conn_tokens.contains('keep-alive')
	}
	return ver == .v1_1
}

// checkout pops the most recently used idle connection for `key`, discarding
// any that sat idle past the timeout. Returns nil when the pool has none.
fn (mut t Transport) checkout(key string) &H1PooledConn {
	mut expired := []&H1PooledConn{}
	mut found := &H1PooledConn(unsafe { nil })
	t.mu.lock()
	for {
		mut list := t.h1_idle[key] or { break }
		if list.len == 0 {
			break
		}
		mut c := list.pop()
		t.h1_idle[key] = list
		if time.now() - c.idle_since > t.idle_timeout {
			expired << c
			continue
		}
		found = c
		break
	}
	t.mu.unlock()
	for mut c in expired {
		c.close_conn()
	}
	return found
}

// checkin returns a healthy connection to the idle pool, or closes it when the
// per-host pool is already at capacity. Adding it may push the total idle count
// over max_idle_conns, in which case the least-recently-used idle connection
// across all pools (h1 and h2) is evicted and closed.
fn (mut t Transport) checkin(mut conn H1PooledConn) {
	conn.idle_since = time.now()
	t.mu.lock()
	mut list := t.h1_idle[conn.key] or { []&H1PooledConn{} }
	if list.len >= t.max_idle_conns_per_host {
		t.mu.unlock()
		conn.close_conn()
		return
	}
	list << conn
	t.h1_idle[conn.key] = list
	mut evicted := t.evict_oldest_idle_locked('')
	t.mu.unlock()
	if evicted.h1 != unsafe { nil } {
		evicted.h1.close_conn()
	}
	if evicted.h2 != unsafe { nil } {
		evicted.h2.shutdown_when_idle()
		evicted.h2.release()
	}
}

// total_idle_locked sums the idle h1 connections across all pool keys. The
// caller must hold t.mu.
fn (t &Transport) total_idle_locked() int {
	mut n := 0
	for _, list in t.h1_idle {
		n += list.len
	}
	return n
}

// EvictedIdleConn carries the result of evict_oldest_idle_locked: at most one
// of h1/h2 is non-nil.
struct EvictedIdleConn {
mut:
	h1 &H1PooledConn = unsafe { nil }
	h2 &H2MuxConn    = unsafe { nil }
}

// evict_oldest_idle_locked evicts the single least-recently-idle connection
// across BOTH the h1 and h2 pools when the combined idle count exceeds
// max_idle_conns — a shared cap enforced from only one side lets the other
// protocol exceed it silently: checkin()'s own cap check used to count only
// h1_idle, and the h2 dial path's own eviction scan used to consider only
// h2_conns, so a budget already filled entirely by ONE protocol was never
// freed for the OTHER (Codex P2, vlang/v#27643 pullrequestreview-4630174759:
// with max_idle_conns filled by one idle h1 connection, a fresh h2 dial had
// no eligible h2 candidate to evict — its own new entry is excluded, and
// there was no other h2 connection — so neither it nor the h1 entry was ever
// freed, and the pool grew past the documented cap). The caller must hold
// t.mu; the returned connection's teardown (close_conn for h1,
// shutdown_when_idle+release for h2) must happen OUTSIDE t.mu — h2's
// release() can synchronously call this connection's own close_transport
// closure, which itself takes t.mu.
//
// `just_registered_h2_key`, when non-empty, excludes that h2 key from
// candidacy: the h2 dial path calls this in the same locked section that just
// registered a brand-new h2_conns entry under that key, whose active_streams
// is still 0 (do_h2 hasn't run yet), making it look idle when it is this very
// dial's own result (Codex P1, pullrequestreview-4628439062) — otherwise, if
// it were the only idle h2 candidate, it would be evicted before its own
// request ever runs on it. Only a connection with zero active streams (h2) or
// one already sitting in h1_idle (h1, always idle by construction) is
// eligible — a busy h2 connection is never evicted to make room, mirroring
// how a checked-out h1 connection (not in h1_idle) can't be evicted either.
fn (mut t Transport) evict_oldest_idle_locked(just_registered_h2_key string) EvictedIdleConn {
	if t.max_idle_conns <= 0 {
		return EvictedIdleConn{}
	}
	// Count only h2 entries with active_streams == 0, matching the eviction
	// scan below's own eligibility. Counting every h2_conns entry regardless
	// of activity made a pool with busy h2 connections and a single genuinely
	// idle h1 connection look over budget on its own, evicting that h1
	// connection even though the actual idle count was within the cap (Codex
	// P3, vlang/v#27643 pullrequestreview-4631763931, discussion 3525390899).
	//
	// just_registered_h2_key is deliberately NOT excluded here (unlike the
	// candidacy scan below): it still occupies a real pool slot the instant
	// it is registered, so it must count toward the cap even though it can
	// never be picked as its own eviction victim. Excluding it from this
	// count too under-counted the pool during sequential single-connection
	// dials (each new h2 registration's own active_streams is still 0 before
	// do_h2 runs), letting the combined count sit AT the cap instead of over
	// it and skipping an eviction that should have happened.
	mut idle_h2_count := 0
	for _, mut c in t.h2_conns {
		c.smu.lock()
		is_idle := c.active_streams == 0
		c.smu.unlock()
		if is_idle {
			idle_h2_count++
		}
	}
	if t.total_idle_locked() + idle_h2_count <= t.max_idle_conns {
		return EvictedIdleConn{}
	}
	mut found := false
	mut from_h2 := false
	mut oldest_since := time.now()
	mut h1_key := ''
	mut h1_idx := -1
	mut h2_key := ''
	mut h2_conn := &H2MuxConn(unsafe { nil })
	for k, list in t.h1_idle {
		for i, c in list {
			if !found || c.idle_since < oldest_since {
				found = true
				from_h2 = false
				h1_key = k
				h1_idx = i
				oldest_since = c.idle_since
			}
		}
	}
	for k, mut c in t.h2_conns {
		if k == just_registered_h2_key {
			continue
		}
		c.smu.lock()
		is_idle := c.active_streams == 0
		since := c.idle_since
		c.smu.unlock()
		if !is_idle {
			continue
		}
		if !found || since < oldest_since {
			found = true
			from_h2 = true
			h2_key = k
			h2_conn = c
			oldest_since = since
		}
	}
	if !found {
		return EvictedIdleConn{}
	}
	if from_h2 {
		t.h2_conns.delete(h2_key)
		t.h2_dial_id.delete(h2_key)
		return EvictedIdleConn{
			h2: h2_conn
		}
	}
	mut list := t.h1_idle[h1_key] or { return EvictedIdleConn{} }
	victim := list[h1_idx]
	list.delete(h1_idx)
	if list.len == 0 {
		t.h1_idle.delete(h1_key)
	} else {
		t.h1_idle[h1_key] = list
	}
	return EvictedIdleConn{
		h1: victim
	}
}

// maybe_checkin pools the connection when both the read framing and the
// response (and request) headers allow reuse; otherwise it closes it.
fn (mut t Transport) maybe_checkin(mut conn H1PooledConn, header Header, reusable bool, resp &Response) {
	if reusable && !header.contains(.connection) && response_allows_reuse(resp) {
		t.checkin(mut conn)
		return
	}
	conn.close_conn()
}

// round_trip performs one HTTP request through the connection pool: it reuses
// an idle connection for the request's pool key when one exists (transparently
// retrying once on a connection that turned out to be stale), dials otherwise,
// and returns healthy connections to the pool afterwards.
fn (mut t Transport) round_trip(req &Request, method Method, scheme string, host string, port int, path string, data string, header Header) !Response {
	$if windows && !no_vschannel ? {
		if scheme == 'https' {
			// The SChannel backend keeps its proven one-shot path until SChannel
			// pooling lands; plain-http pooling below already works on Windows.
			return req.ssl_do(port, method, host, path, data, header)
		}
	}
	raw := req.build_request_headers_opts(method, host, port, path, data, header, false)
	$if trace_http_request ? {
		eprint('> ')
		eprint(raw)
		eprintln('')
	}
	key := transport_pool_key(req, scheme, host, port)
	if scheme == 'https' && req.enable_http2 {
		t.mu.lock()
		proto := t.key_proto[key] or { 0 }
		t.mu.unlock()
		if proto != 1 {
			// Unknown or known-h2: try the pooled, multiplexed h2 path. It falls
			// back to the ordinary h1 path itself (and memoizes key_proto[key] = 1)
			// the first time ALPN turns out not to be h2 for this key — see
			// transport_h2.v.
			return t.h2_round_trip(req, key, raw, method, host, port, path, data, header)
		}
		// key_proto[key] == 1: this origin is already known http/1.1-only; fall
		// through to the ordinary pooled path below unchanged.
	}
	// A stale pooled connection (closed by the server while idle) fails the
	// exchange; drain through the pool, then dial fresh.
	for _ in 0 .. t.max_idle_conns_per_host + 1 {
		mut conn := t.checkout(key)
		mut reused := true
		if conn == unsafe { nil } {
			reused = false
			if scheme == 'https' {
				return t.tls_fresh_round_trip(req, key, raw, method, host, port, path, data, header)
			}
			conn = t.dial_h1_tcp(req, key, host, port)!
		} else {
			conn.refresh_timeouts(req)
		}
		resp, reusable := conn.exchange(req, raw) or {
			conn.close_conn()
			// The write helpers do partial writes and cannot report how many
			// bytes reached the server, so a failed exchange may have delivered
			// part of the request. Only idempotent methods are safe to replay;
			// a non-idempotent one is tagged so neither this loop nor the
			// caller's outer retry loop re-sends it (avoiding duplicate side
			// effects). A pre-write dial failure, by contrast, propagates before
			// this point and stays freely retryable.
			if !transport_is_idempotent(method) {
				return error_with_code(err.msg(), transport_err_unsafe_retry)
			}
			if reused {
				// Drain stale pooled connections, then fall through to a fresh dial.
				continue
			}
			return err
		}
		t.maybe_checkin(mut conn, header, reusable, resp)
		return resp
	}
	return error('http.transport: request failed after retrying on a fresh connection')
}

// dial_h1_tcp opens a fresh plain-TCP connection for `key`.
fn (mut t Transport) dial_h1_tcp(req &Request, key string, host string, port int) !&H1PooledConn {
	mut client := net.dial_tcp('${host}:${port}')!
	client.set_read_timeout(req.read_timeout)
	client.set_write_timeout(req.write_timeout)
	return &H1PooledConn{
		key: key
		tcp: client
	}
}

// tls_fresh_round_trip dials a fresh TLS connection (advertising ALPN h2 when
// HTTP/2 is enabled) and completes the request on it. When the server selects
// h2, the request runs on the existing one-shot HTTP/2 driver and the
// connection is not pooled yet; an http/1.1 connection is pooled afterwards
// like any other.
fn (mut t Transport) tls_fresh_round_trip(req &Request, key string, raw string, method Method, host string, port int, path string, data string, header Header) !Response {
	alpn := if req.enable_http2 { ['h2', 'http/1.1'] } else { []string{} }
	mut ssl_conn := ssl.new_ssl_conn(
		verify:                 req.verify
		cert:                   req.cert
		cert_key:               req.cert_key
		validate:               req.validate
		in_memory_verification: req.in_memory_verification
		alpn_protocols:         alpn
	)!
	ssl_conn.dial(host, port)!
	if req.read_timeout > 0 {
		ssl_conn.set_read_timeout(req.read_timeout)
	}
	if req.enable_http2 && ssl_conn.negotiated_alpn() == 'h2' {
		return req.h2_do(mut ssl_conn, method, host, port, path, data, header)
	}
	mut conn := &H1PooledConn{
		key: key
		ssl: ssl_conn
	}
	resp, reusable := conn.exchange(req, raw) or {
		conn.close_conn()
		// Past the TLS handshake the request bytes may have been (partially)
		// written; a non-idempotent method must not be replayed by the outer
		// retry loop. (A dial/handshake failure above propagates before this and
		// stays retryable, since no request byte was sent.)
		if !transport_is_idempotent(method) {
			return error_with_code(err.msg(), transport_err_unsafe_retry)
		}
		return err
	}
	t.maybe_checkin(mut conn, header, reusable, resp)
	return resp
}
