module http

// Tests for the connection-pooling Transport (transport.v): keep-alive reuse
// over plain TCP and TLS, no-reuse on `Connection: close` / truncated reads,
// transparent retry on stale pooled connections, idle eviction, and the
// `disable_connection_reuse` opt-out. Each test runs its own loopback server
// with an accept counter: the accept count is the proof of (non-)reuse.
import net
import net.mbedtls
import sync
import time

const tls_test_cert_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const tls_test_key_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'

@[heap]
struct KaSrv {
mut:
	mu      &sync.Mutex = sync.new_mutex()
	accepts int
	// connection_close: respond with `Connection: close` and close afterwards.
	connection_close bool
	// close_after_each: close after each response WITHOUT advertising it — the
	// classic stale-pooled-connection scenario.
	close_after_each bool
	// split_connection_close: emit the close token in a SECOND, repeated
	// `Connection` header (`keep-alive` first, then `close`) while keeping the
	// socket open — a server that says "do not reuse" via a split field. The
	// client must honor it and not pool, even though the connection stays usable.
	split_connection_close bool
	// drop_on_post: after reading a POST request head, close the connection
	// without responding — a reused-connection failure after the bytes were
	// written, for a non-idempotent method.
	drop_on_post bool
	posts        int // POST request heads actually read by the server
	body         string = 'hello'
}

fn (mut s KaSrv) bump_accepts() {
	s.mu.lock()
	s.accepts++
	s.mu.unlock()
}

fn (mut s KaSrv) accept_count() int {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.accepts
}

fn (mut s KaSrv) post_count() int {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.posts
}

// ka_read_request_head reads from `conn` until a full request head (terminated
// by a blank line) has arrived, returning it. The test requests carry no bodies.
fn ka_read_request_head(mut conn net.TcpConn) !string {
	mut buf := []u8{len: 4096}
	mut sofar := []u8{}
	for {
		n := conn.read(mut buf)!
		if n <= 0 {
			return error('closed')
		}
		sofar << buf[..n]
		if sofar.bytestr().contains('\r\n\r\n') {
			return sofar.bytestr()
		}
	}
	return error('closed')
}

fn ka_srv_serve_conn(mut s KaSrv, mut conn net.TcpConn) {
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(10 * time.second)
	for {
		head := ka_read_request_head(mut conn) or { return }
		if s.drop_on_post && head.starts_with('POST ') {
			s.mu.lock()
			s.posts++
			s.mu.unlock()
			// Drop the connection without responding: the request bytes were
			// written, so this is not a stale-write.
			return
		}
		mut resp := 'HTTP/1.1 200 OK\r\nContent-Length: ${s.body.len}\r\n'
		if s.connection_close {
			resp += 'Connection: close\r\n'
		}
		if s.split_connection_close {
			resp += 'Connection: keep-alive\r\nConnection: close\r\n'
		}
		resp += '\r\n' + s.body
		conn.write(resp.bytes()) or { return }
		if s.connection_close || s.close_after_each {
			return
		}
	}
}

fn ka_srv_loop(mut s KaSrv, mut listener net.TcpListener) {
	for {
		mut conn := listener.accept() or { return }
		s.bump_accepts()
		ka_srv_serve_conn(mut s, mut conn)
	}
}

// start_ka_srv starts a keep-alive capable loopback HTTP server, returning its
// port, listener and server thread.
fn start_ka_srv(mut s KaSrv) !(int, &net.TcpListener, thread) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	th := spawn ka_srv_loop(mut s, mut listener)
	return port, listener, th
}

// stop_ka_srv tears a test server down fully before the test returns: closing
// the idle pool unblocks a server thread reading a kept-alive connection,
// closing the listener aborts its accept, and the join guarantees the thread
// is gone before the next test creates sockets (otherwise the OS can recycle
// this listener's handle for the next test's listener while the old thread is
// still calling accept on it, stealing its connections).
fn stop_ka_srv(mut listener net.TcpListener, th thread) {
	close_idle_connections()
	listener.close() or {}
	th.wait()
}

// The pool key must isolate distinct TLS configurations even when a field value
// contains the '|' separator, or a request could reuse a connection dialed with
// the wrong cert/CA. cert='a|b',cert_key='c' and cert='a',cert_key='b|c' must
// not collide.
fn test_transport_pool_key_no_delimiter_collision() {
	a := Request{
		cert:     'a|b'
		cert_key: 'c'
	}
	b := Request{
		cert:     'a'
		cert_key: 'b|c'
	}
	assert transport_pool_key(a, 'https', 'h', 443) != transport_pool_key(b, 'https', 'h', 443)
	// Identical configs still share a key.
	a2 := Request{
		cert:     'a|b'
		cert_key: 'c'
	}
	assert transport_pool_key(a, 'https', 'h', 443) == transport_pool_key(a2, 'https', 'h', 443)
	// A host containing '|' must not collide with a different host/port split.
	assert transport_pool_key(Request{}, 'https', 'h|x', 443) != transport_pool_key(Request{},
		'https', 'h', 443)
}

fn test_h1_plain_reuse() {
	mut srv := &KaSrv{}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	for i in 0 .. 3 {
		resp := fetch(url: 'http://127.0.0.1:${port}/r${i}') or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
		assert resp.body == 'hello'
	}
	// All three requests must have shared one connection.
	assert srv.accept_count() == 1
	stop_ka_srv(mut listener, th)
}

fn test_h1_no_reuse_on_connection_close_response() {
	mut srv := &KaSrv{
		connection_close: true
	}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	for i in 0 .. 2 {
		resp := fetch(url: 'http://127.0.0.1:${port}/r${i}') or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
	}
	// `Connection: close` responses must not be pooled.
	assert srv.accept_count() == 2
	stop_ka_srv(mut listener, th)
}

// A close token carried in a repeated `Connection` header (after a keep-alive
// one) must still be honored: parse_headers stores repeats separately and get()
// returns only the first, so response_allows_reuse joins all values. The server
// keeps the socket open, so a wrongly-pooled connection would be reused (1
// accept); honoring the close dials fresh each time (2 accepts).
fn test_h1_no_reuse_on_split_connection_close() {
	mut srv := &KaSrv{
		split_connection_close: true
	}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	for i in 0 .. 2 {
		resp := fetch(url: 'http://127.0.0.1:${port}/r${i}') or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
	}
	assert srv.accept_count() == 2
	stop_ka_srv(mut listener, th)
}

// The total idle pool is bounded by max_idle_conns across all pool keys, not
// just per host: checking in more distinct-keyed connections than the cap
// evicts the least-recently-used ones (here k0, k1) and keeps the newest.
fn test_transport_global_idle_cap() {
	mut t := new_transport()
	t.max_idle_conns = 3
	t.max_idle_conns_per_host = 10 // keep the per-host cap out of the way
	for i in 0 .. 5 {
		mut c := &H1PooledConn{
			key: 'k${i}'
		}
		t.checkin(mut c)
	}
	mut total := 0
	for _, list in t.h1_idle {
		total += list.len
	}
	assert total == 3, 'global idle cap not enforced: ${total}'
	assert 'k0' !in t.h1_idle
	assert 'k1' !in t.h1_idle
	assert 'k2' in t.h1_idle
	assert 'k3' in t.h1_idle
	assert 'k4' in t.h1_idle
}

fn test_h1_stale_pooled_connection_is_retried() {
	mut srv := &KaSrv{
		close_after_each: true
	}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	// First request succeeds and the connection is pooled (the response did not
	// advertise the close). The server then closes it. The second request picks
	// up the stale connection, fails, and must transparently retry on a fresh
	// one.
	for i in 0 .. 2 {
		resp := fetch(url: 'http://127.0.0.1:${port}/r${i}') or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
		assert resp.body == 'hello'
	}
	assert srv.accept_count() == 2
	stop_ka_srv(mut listener, th)
}

// A non-idempotent request that fails on a reused keep-alive connection after
// its bytes were written must NOT be replayed: doing so could duplicate side
// effects. The server reads the POST then drops the connection; the client must
// surface the error without retrying on a fresh connection.
fn test_h1_unsafe_pooled_post_is_not_retried() {
	mut srv := &KaSrv{
		drop_on_post: true
	}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	// First, a GET to establish and pool a keep-alive connection.
	resp := fetch(url: 'http://127.0.0.1:${port}/warmup') or {
		assert false, 'warmup fetch: ${err}'
		return
	}
	assert resp.status_code == 200
	// The POST reuses that connection; the server reads it and drops the
	// connection. The request must fail rather than be re-sent.
	fetch(method: .post, url: 'http://127.0.0.1:${port}/submit', data: 'payload') or {
		// expected: the POST failed and was not retried.
		assert srv.post_count() == 1, 'POST was replayed ${srv.post_count()} times'
		assert srv.accept_count() == 1, 'a fresh connection was opened to retry the POST'
		stop_ka_srv(mut listener, th)
		return
	}
	assert false, 'the POST unexpectedly succeeded'
	stop_ka_srv(mut listener, th)
}

fn test_h1_opt_out_disables_reuse() {
	mut srv := &KaSrv{}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	for i in 0 .. 2 {
		resp := fetch(
			url:                      'http://127.0.0.1:${port}/r${i}'
			disable_connection_reuse: true
		) or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
	}
	// Opt-out requests open one connection each.
	assert srv.accept_count() == 2
	stop_ka_srv(mut listener, th)
}

fn test_h1_truncated_read_poisons_connection() {
	mut srv := &KaSrv{
		// Larger than the 64KB read buffer, so the body needs several reads and
		// the stop limit actually interrupts the transfer mid-stream.
		body: 'x'.repeat(200 * 1024)
	}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	// A stop_receiving_limit read leaves unread response bytes on the wire, so
	// that connection must not be reused.
	resp1 := fetch(url: 'http://127.0.0.1:${port}/big', stop_receiving_limit: 1000) or {
		assert false, 'fetch 1: ${err}'
		return
	}
	assert resp1.status_code == 200
	resp2 := fetch(url: 'http://127.0.0.1:${port}/after') or {
		assert false, 'fetch 2: ${err}'
		return
	}
	assert resp2.status_code == 200
	assert srv.accept_count() == 2
	stop_ka_srv(mut listener, th)
}

fn test_h1_idle_eviction() {
	mut srv := &KaSrv{}
	port, mut listener, th := start_ka_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	mut t := new_transport()
	t.idle_timeout = 50 * time.millisecond
	req := prepare(url: 'http://127.0.0.1:${port}/') or {
		assert false, 'prepare: ${err}'
		return
	}
	r1 := t.round_trip(req, .get, 'http', '127.0.0.1', port, '/', '', req.header) or {
		assert false, 'round_trip 1: ${err}'
		return
	}
	assert r1.status_code == 200
	time.sleep(150 * time.millisecond)
	// The pooled connection has sat idle past the timeout: it must be evicted
	// and a fresh one dialled.
	r2 := t.round_trip(req, .get, 'http', '127.0.0.1', port, '/', '', req.header) or {
		assert false, 'round_trip 2: ${err}'
		return
	}
	assert r2.status_code == 200
	assert srv.accept_count() == 2
	// This test pools in its own private Transport, so flush that one before
	// the joint teardown.
	t.close_idle()
	stop_ka_srv(mut listener, th)
}

// --- TLS (h1 over mbedtls) reuse ---

@[heap]
struct TlsKaSrv {
mut:
	mu      &sync.Mutex = sync.new_mutex()
	accepts int
}

fn (mut s TlsKaSrv) bump_accepts() {
	s.mu.lock()
	s.accepts++
	s.mu.unlock()
}

fn (mut s TlsKaSrv) accept_count() int {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.accepts
}

fn tls_ka_srv_serve_conn(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	body := 'tls hello'
	for {
		mut buf := []u8{len: 4096}
		mut sofar := []u8{}
		for !sofar.bytestr().contains('\r\n\r\n') {
			n := conn.read(mut buf) or { return }
			if n <= 0 {
				return
			}
			sofar << buf[..n]
		}
		conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: ${body.len}\r\n\r\n${body}') or {
			return
		}
	}
}

fn tls_ka_srv_loop(mut s TlsKaSrv, mut listener mbedtls.SSLListener) {
	for {
		mut conn := listener.accept() or { return }
		s.bump_accepts()
		// Serve each connection on its own thread: a pooled idle connection
		// keeps its serve loop parked in read, which must not block accepting
		// the next connection. (The serve threads exit when the test closes
		// the pooled connections via close_idle_connections.)
		spawn tls_ka_srv_serve_conn(mut conn)
	}
}

fn test_h1_tls_reuse() {
	$if windows && !no_vschannel ? {
		// The default Windows TLS backend (SChannel) keeps its one-shot path
		// until SChannel pooling lands.
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or {
		assert false, 'port: ${err}'
		return
	}
	port := port_listener.addr() or {
		assert false, 'addr: ${err}'
		return
	}.port() or {
		assert false, 'port: ${err}'
		return
	}
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     tls_test_cert_path
		cert_key: tls_test_key_path
		validate: false
	}) or {
		assert false, 'listener: ${err}'
		return
	}
	mut srv := &TlsKaSrv{}
	th := spawn tls_ka_srv_loop(mut srv, mut listener)
	for i in 0 .. 2 {
		resp := fetch(url: 'https://127.0.0.1:${port}/r${i}', validate: false) or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
		assert resp.body == 'tls hello'
	}
	// Both https requests must have shared one TLS connection.
	assert srv.accept_count() == 1
	// Free the pooled TLS connection (unblocking the server read), then abort
	// the accept and join the server thread before the test returns.
	close_idle_connections()
	listener.shutdown() or {}
	th.wait()
}

// A TLS connection dialled with HTTP/2 disabled (no ALPN) must not satisfy an
// HTTP/2-enabled request to the same origin — the ALPN preference is part of
// the pool key. Forced-h1 requests still share among themselves.
fn test_h1_tls_no_reuse_across_alpn_preference() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or {
		assert false, 'port: ${err}'
		return
	}
	port := port_listener.addr() or {
		assert false, 'addr: ${err}'
		return
	}.port() or {
		assert false, 'port: ${err}'
		return
	}
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     tls_test_cert_path
		cert_key: tls_test_key_path
		validate: false
	}) or {
		assert false, 'listener: ${err}'
		return
	}
	mut srv := &TlsKaSrv{}
	th := spawn tls_ka_srv_loop(mut srv, mut listener)
	// 1. Forced-HTTP/1.1 request: dialled without ALPN, pooled under its key.
	r1 := fetch(url: 'https://127.0.0.1:${port}/h1', validate: false, enable_http2: false) or {
		assert false, 'fetch 1: ${err}'
		return
	}
	assert r1.status_code == 200
	// 2. Default (HTTP/2-enabled) request: must NOT reuse the no-ALPN
	// connection; it dials fresh and advertises h2.
	r2 := fetch(url: 'https://127.0.0.1:${port}/h2pref', validate: false) or {
		assert false, 'fetch 2: ${err}'
		return
	}
	assert r2.status_code == 200
	assert srv.accept_count() == 2
	// 3. Another forced-HTTP/1.1 request: reuses connection 1.
	r3 := fetch(url: 'https://127.0.0.1:${port}/h1again', validate: false, enable_http2: false) or {
		assert false, 'fetch 3: ${err}'
		return
	}
	assert r3.status_code == 200
	assert srv.accept_count() == 2
	close_idle_connections()
	listener.shutdown() or {}
	th.wait()
}
