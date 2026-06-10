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

// transport_err_stale_write tags request-write failures on a transport
// connection: the request bytes never reached the server, so the request is
// always safe to retry on a fresh connection.
const transport_err_stale_write = -20011

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
@[heap]
pub struct Transport {
pub mut:
	// max_idle_conns_per_host caps the idle keep-alive connections retained per
	// pool key (origin + TLS configuration).
	max_idle_conns_per_host int = 4
	// idle_timeout is how long an idle connection may sit in the pool before it
	// is discarded instead of reused.
	idle_timeout time.Duration = 90 * time.second
mut:
	mu      &sync.Mutex = sync.new_mutex()
	h1_idle map[string][]&H1PooledConn
}

// new_transport creates an empty Transport with default limits.
pub fn new_transport() &Transport {
	return &Transport{}
}

__global http_transport_default = &Transport(unsafe { nil })
__global http_transport_once = sync.new_once()

// default_transport returns the process-global Transport that fetch() and
// Request.do() route through, creating it on first use.
pub fn default_transport() &Transport {
	mut once := http_transport_once
	once.do_with_param(fn (_ voidptr) {
		http_transport_default = new_transport()
	}, unsafe { nil })
	return http_transport_default
}

// close_idle_connections closes every idle pooled connection held by the
// default Transport. Useful before fork()-style process handoffs, or in tests.
pub fn close_idle_connections() {
	mut t := default_transport()
	t.close_idle()
}

// close_idle closes every idle pooled connection held by this Transport.
// In-flight requests are unaffected.
pub fn (mut t Transport) close_idle() {
	mut all := []&H1PooledConn{}
	t.mu.lock()
	for _, list in t.h1_idle {
		for c in list {
			all << c
		}
	}
	t.h1_idle.clear()
	t.mu.unlock()
	for mut c in all {
		c.close_conn()
	}
}

// transport_pool_key builds the pool key for a request: connections are only
// shared between requests whose origin and TLS-relevant settings all match.
fn transport_pool_key(req &Request, scheme string, host string, port int) string {
	return '${scheme}|${host}|${port}|${req.validate}|${req.verify}|${req.cert}|${req.cert_key}|${req.in_memory_verification}'
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
	conn_tokens := (resp.header.get(.connection) or { '' }).to_lower()
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
// pool for its key is already at capacity.
fn (mut t Transport) checkin(mut conn H1PooledConn) {
	conn.idle_since = time.now()
	t.mu.lock()
	mut list := t.h1_idle[conn.key] or { []&H1PooledConn{} }
	if list.len < t.max_idle_conns_per_host {
		list << conn
		t.h1_idle[conn.key] = list
		t.mu.unlock()
		return
	}
	t.mu.unlock()
	conn.close_conn()
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
			if reused
				&& (err.code() == transport_err_stale_write || transport_is_idempotent(method)) {
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
		return err
	}
	t.maybe_checkin(mut conn, header, reusable, resp)
	return resp
}
