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
	body             string = 'hello'
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

// ka_read_request_head reads from `conn` until a full request head (terminated
// by a blank line) has arrived. The test requests carry no bodies.
fn ka_read_request_head(mut conn net.TcpConn) ! {
	mut buf := []u8{len: 4096}
	mut sofar := []u8{}
	for {
		n := conn.read(mut buf)!
		if n <= 0 {
			return error('closed')
		}
		sofar << buf[..n]
		if sofar.bytestr().contains('\r\n\r\n') {
			return
		}
	}
}

fn ka_srv_serve_conn(mut s KaSrv, mut conn net.TcpConn) {
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(10 * time.second)
	for {
		ka_read_request_head(mut conn) or { return }
		mut resp := 'HTTP/1.1 200 OK\r\nContent-Length: ${s.body.len}\r\n'
		if s.connection_close {
			resp += 'Connection: close\r\n'
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
		tls_ka_srv_serve_conn(mut conn)
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
