module http

// Tests for the pooled, multiplexed HTTP/2 client path (transport_h2.v +
// h2_pooled_transport.v): ALPN dial singleflighting, connection reuse across
// requests, transparent redial after GOAWAY, close_idle_connections() flushing
// the h2 pool, the disable_connection_reuse opt-out, and the concurrency
// contract of the H2PooledTransport adapter itself. Each test runs its own
// loopback TLS server with an accept counter: the accept count is the proof of
// (non-)reuse, mirroring transport_test.v's idiom for h1.
//
// The server side is a small scripted frame-level h2 driver rather than
// h2_server.v's serve_h2_conn: the real server advertises (and enforces)
// SETTINGS_MAX_CONCURRENT_STREAMS = 1, so concurrent pooled requests would hit
// the client-side stream limit and legitimately redial, making accept counts
// nondeterministic. The scripted server advertises a high limit instead, and
// can inject a GOAWAY mid-session, which serve_h2_conn cannot.
import net
import net.mbedtls
import net.ssl
import sync
import time

const h2t_cert_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const h2t_key_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'

const h2t_body = 'h2 pool hello'

@[heap]
struct H2PoolSrv {
mut:
	mu      &sync.Mutex = sync.new_mutex()
	accepts int
	// conns_done counts serve threads that have exited — the observable proof
	// that a client-side teardown actually closed the socket.
	conns_done int
	// goaway_after_first: send GOAWAY(last_stream_id = first stream) on each
	// connection right before responding to its first request, so the pooled
	// client connection is retired while otherwise fully healthy.
	goaway_after_first bool
	// respond_delay: sleep this long after receiving a request's HEADERS
	// before sending the response, so a test can observe the client-side
	// stream while it is still in flight (refs held above the pool's own
	// baseline reference).
	respond_delay time.Duration
}

fn (mut s H2PoolSrv) bump_accepts() {
	s.mu.lock()
	s.accepts++
	s.mu.unlock()
}

fn (mut s H2PoolSrv) accept_count() int {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.accepts
}

fn (mut s H2PoolSrv) bump_done() {
	s.mu.lock()
	s.conns_done++
	s.mu.unlock()
}

fn (mut s H2PoolSrv) done_count() int {
	s.mu.lock()
	defer {
		s.mu.unlock()
	}
	return s.conns_done
}

// H2PoolSrvConn buffers reads from one server-side connection so h2 frames can
// be consumed at frame granularity (mirrors H2Conn.fill_at_least/read_frame).
struct H2PoolSrvConn {
mut:
	rbuf []u8
}

fn (mut st H2PoolSrvConn) fill(mut conn mbedtls.SSLConn, n int) ! {
	for st.rbuf.len < n {
		mut tmp := []u8{len: 16384}
		got := conn.read(mut tmp)!
		if got <= 0 {
			return error('closed')
		}
		st.rbuf << tmp[..got]
	}
}

fn (mut st H2PoolSrvConn) read_frame(mut conn mbedtls.SSLConn) !H2Frame {
	st.fill(mut conn, h2_frame_header_len)!
	header := h2_parse_frame_header(st.rbuf)!
	total := h2_frame_header_len + int(header.length)
	st.fill(mut conn, total)!
	frame := h2_parse_frame(header, st.rbuf[h2_frame_header_len..total])!
	st.rbuf = st.rbuf[total..].clone()
	return frame
}

fn h2_pool_srv_write(mut conn mbedtls.SSLConn, data []u8) ! {
	mut sent := 0
	for sent < data.len {
		n := conn.write(data[sent..])!
		if n <= 0 {
			return error('write returned ${n}')
		}
		sent += n
	}
}

fn h2_pool_srv_serve(mut s H2PoolSrv, mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
		s.bump_done()
	}
	// Bound every read so a wedged connection fails the test's assertions
	// instead of hanging the test binary.
	conn.set_read_timeout(10 * time.second)
	// Server connection preface: a SETTINGS frame (RFC 9113 §3.4), advertising
	// a stream limit high enough that concurrent pooled requests never hit it.
	h2_pool_srv_write(mut conn, H2Frame(H2SettingsFrame{
		settings: [
			H2Setting{h2_settings_max_concurrent_streams, 100},
		]
	}).encode()) or { return }
	mut st := H2PoolSrvConn{}
	st.fill(mut conn, h2_client_preface.len) or { return }
	if st.rbuf[..h2_client_preface.len].bytestr() != h2_client_preface {
		return
	}
	st.rbuf = st.rbuf[h2_client_preface.len..].clone()
	mut enc := H2HpackEncoder{}
	mut first_stream := u32(0)
	for {
		frame := st.read_frame(mut conn) or { return }
		match frame {
			H2SettingsFrame {
				if !frame.ack {
					h2_pool_srv_write(mut conn, H2Frame(H2SettingsFrame{
						ack: true
					}).encode()) or { return }
				}
			}
			H2PingFrame {
				if !frame.ack {
					h2_pool_srv_write(mut conn, H2Frame(H2PingFrame{
						ack:  true
						data: frame.data
					}).encode()) or { return }
				}
			}
			H2HeadersFrame {
				sid := frame.stream_id
				if s.goaway_after_first && first_stream == 0 {
					first_stream = sid
					// GOAWAY before the response: the client processes it while
					// (or before) the response arrives, so by the time the fetch
					// returns, the pooled connection is deterministically marked
					// goaway and the next request must redial.
					h2_pool_srv_write(mut conn, H2Frame(H2GoawayFrame{
						last_stream_id: sid
					}).encode()) or { return }
				}
				if s.goaway_after_first && sid > first_stream {
					// Declined by the GOAWAY above; the client fails it as
					// retryable and replays it on a fresh connection.
					continue
				}
				if s.respond_delay > 0 {
					time.sleep(s.respond_delay)
				}
				block := enc.encode([
					H2HeaderField{':status', '200'},
					H2HeaderField{'content-length', h2t_body.len.str()},
				])
				h2_pool_srv_write(mut conn, H2Frame(H2HeadersFrame{
					stream_id:   sid
					fragment:    block
					end_headers: true
				}).encode()) or { return }
				h2_pool_srv_write(mut conn, H2Frame(H2DataFrame{
					stream_id:  sid
					data:       h2t_body.bytes()
					end_stream: true
				}).encode()) or { return }
			}
			H2GoawayFrame {
				return
			}
			else {
				// DATA / WINDOW_UPDATE / RST_STREAM / PRIORITY: nothing to do for
				// the GET-only exchanges these tests drive.
			}
		}
	}
}

fn h2_pool_srv_loop(mut s H2PoolSrv, mut listener mbedtls.SSLListener) {
	for {
		mut conn := listener.accept() or { return }
		s.bump_accepts()
		spawn h2_pool_srv_serve(mut s, mut conn)
	}
}

// start_h2_pool_srv starts a loopback TLS listener that only offers `h2` via
// ALPN and drives the scripted h2 server on each accepted connection.
fn start_h2_pool_srv(mut s H2PoolSrv) !(int, &mbedtls.SSLListener, thread) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:           h2t_cert_path
		cert_key:       h2t_key_path
		validate:       false
		alpn_protocols: ['h2']
	})!
	th := spawn h2_pool_srv_loop(mut s, mut listener)
	return port, listener, th
}

// stop_h2_pool_srv flushes the pooled client connections (unblocking the serve
// threads' reads), aborts the accept loop, and joins it.
fn stop_h2_pool_srv(mut listener mbedtls.SSLListener, th thread) {
	close_idle_connections()
	listener.shutdown() or {}
	th.wait()
}

fn h2t_fetch_worker(port int, path string) int {
	resp := fetch(url: 'https://127.0.0.1:${port}${path}', validate: false) or { return -1 }
	if resp.body != h2t_body {
		return -2
	}
	return resp.status_code
}

// Concurrent first requests to a fresh h2 origin must share one ALPN-probing
// dial (the H2DialCall singleflight) and then multiplex on the one resulting
// connection: exactly one server accept for all of them.
fn test_h2_pool_dial_dedup_race() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	mut workers := []thread int{}
	for i in 0 .. 8 {
		workers << spawn h2t_fetch_worker(port, '/race${i}')
	}
	for w in workers {
		status := w.wait()
		assert status == 200, 'concurrent fetch failed with ${status}'
	}
	assert srv.accept_count() == 1, 'expected one deduped dial, got ${srv.accept_count()} accepts'
	stop_h2_pool_srv(mut listener, th)
}

// Sequential requests to the same h2 origin must reuse the pooled multiplexed
// connection (fast path), not redial.
fn test_h2_pool_sequential_reuse() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	for i in 0 .. 3 {
		resp := fetch(url: 'https://127.0.0.1:${port}/seq${i}', validate: false) or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
		assert resp.body == h2t_body
		assert resp.version() == .v2_0
	}
	assert srv.accept_count() == 1, 'expected one shared connection, got ${srv.accept_count()} accepts'
	stop_h2_pool_srv(mut listener, th)
}

// After the server retires the pooled connection with GOAWAY, the next request
// must transparently dial a fresh connection and succeed — the caller sees two
// clean 200s, the server sees exactly two accepts. This also exercises the
// orphan-release path in h2_dial_and_do: the GOAWAY'd connection is still
// registered in h2_conns (its refcount never hit zero) when the redial
// replaces it, and the old connection's own close_transport (captured stale
// dial_id) must not evict the new pool entry.
fn test_h2_pool_goaway_redial() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{
		goaway_after_first: true
	}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	r1 := fetch(url: 'https://127.0.0.1:${port}/first', validate: false) or {
		assert false, 'fetch 1: ${err}'
		return
	}
	assert r1.status_code == 200
	assert r1.body == h2t_body
	r2 := fetch(url: 'https://127.0.0.1:${port}/second', validate: false) or {
		assert false, 'fetch 2: ${err}'
		return
	}
	assert r2.status_code == 200
	assert r2.body == h2t_body
	assert srv.accept_count() == 2, 'expected a redial after GOAWAY, got ${srv.accept_count()} accepts'
	stop_h2_pool_srv(mut listener, th)
}

// close_idle_connections() must not release a pooled h2 connection's pool
// reference more than once. A connection with an in-flight stream survives
// one close_idle() call (its refcount drops but doesn't reach zero, so
// nothing removes it from the pool), so a second close_idle() call — a
// concurrent caller, or simply calling it twice — must not find the same
// connection again and re-release it: that would over-decrement the
// refcount below the number of genuinely live references (Codex P2,
// vlang/v#27643 pullrequestreview-4626225521, discussion 3520259791).
// Asserted directly on the refcount invariant (refs must stay >= 1 while a
// stream is still active) rather than inferred indirectly through the
// in-flight fetch's eventual outcome, which depends on unrelated timing
// between the teardown and the reader thread noticing it.
fn test_h2_pool_close_idle_release_is_idempotent() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{
		respond_delay: 800 * time.millisecond
	}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	mut t := default_transport()
	status := spawn h2t_fetch_worker(port, '/slow')
	// Wait until the pooled h2 connection is actually registered (dial +
	// ALPN negotiation + stream admission all complete) before racing
	// close_idle against the in-flight stream — a blind sleep is fragile
	// under scheduling jitter, and racing too early would just find nothing
	// to release on either call.
	mut conn := &H2MuxConn(unsafe { nil })
	for _ in 0 .. 500 {
		t.mu.lock()
		for _, mut c in t.h2_conns {
			conn = c
		}
		t.mu.unlock()
		if conn != unsafe { nil } {
			break
		}
		time.sleep(5 * time.millisecond)
	}
	assert conn != unsafe { nil }, 'pooled h2 connection was never registered before the delayed response arrived'
	conn.smu.lock()
	active := conn.active_streams
	conn.smu.unlock()
	assert active > 0, 'stream was not yet admitted — test raced ahead of do()'
	// Two close_idle calls while the stream is still in flight: on buggy code
	// the second one finds the connection still registered and re-releases
	// it, over-decrementing refs below the number of genuinely live
	// references (the pool's own + the active stream's).
	close_idle_connections()
	close_idle_connections()
	conn.smu.lock()
	refs_after := conn.refs
	active_after := conn.active_streams
	conn.smu.unlock()
	assert refs_after >= active_after, 'close_idle_connections() released the pool reference ${refs_after - active_after} time(s) too many while ${active_after} stream(s) were still active (refs=${refs_after})'
	status.wait()
	stop_h2_pool_srv(mut listener, th)
}

// close_idle_connections() must tear the pooled h2 connection all the way
// down — proven by the server-side serve thread exiting, not merely by the
// call returning — and leave the pool usable for subsequent requests.
fn test_h2_pool_close_idle_flushes() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	r1 := fetch(url: 'https://127.0.0.1:${port}/one', validate: false) or {
		assert false, 'fetch 1: ${err}'
		return
	}
	assert r1.status_code == 200
	assert srv.accept_count() == 1
	close_idle_connections()
	// The flush closes the client-side transport, which must unblock the
	// server's read and let its serve thread exit.
	mut flushed := false
	for _ in 0 .. 200 {
		if srv.done_count() == 1 {
			flushed = true
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert flushed, 'server serve thread did not exit after close_idle_connections()'
	// The pool must remain functional afterwards: a fresh dial, not an error.
	r2 := fetch(url: 'https://127.0.0.1:${port}/two', validate: false) or {
		assert false, 'fetch 2: ${err}'
		return
	}
	assert r2.status_code == 200
	assert srv.accept_count() == 2
	stop_h2_pool_srv(mut listener, th)
}

// disable_connection_reuse must keep its pre-pooling behavior on an h2-capable
// origin: every request dials its own connection and nothing is pooled.
fn test_h2_pool_disable_reuse_opt_out() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	for i in 0 .. 2 {
		resp := fetch(
			url:                      'https://127.0.0.1:${port}/noreuse${i}'
			validate:                 false
			disable_connection_reuse: true
		) or {
			assert false, 'fetch ${i}: ${err}'
			return
		}
		assert resp.status_code == 200
		assert resp.body == h2t_body
	}
	assert srv.accept_count() == 2, 'opted-out requests must not share a connection'
	stop_h2_pool_srv(mut listener, th)
}

// --- H2PooledTransport adapter-level concurrency ---

fn h2t_echo_conn(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(10 * time.second)
	for {
		mut buf := []u8{len: 4096}
		n := conn.read(mut buf) or { return }
		if n <= 0 {
			return
		}
		conn.write(buf[..n]) or { return }
	}
}

fn h2t_echo_loop(mut listener mbedtls.SSLListener) {
	for {
		mut conn := listener.accept() or { return }
		spawn h2t_echo_conn(mut conn)
	}
}

fn h2t_hammer_reader(mut pt H2PooledTransport) int {
	mut total := 0
	for {
		mut buf := []u8{len: 2048}
		n := pt.read(mut buf) or {
			// Mirror the real consumer (H2MuxConn.read_loop): timeout errors
			// are benign wake-ups to re-poll on; anything else ends the read
			// loop — after close() that is the socket error that lets this
			// thread exit.
			if is_transport_timeout_error(err) {
				continue
			}
			return total
		}
		total += n
	}
	return total
}

fn h2t_hammer_writer(mut pt H2PooledTransport) int {
	payload := []u8{len: 512, init: u8(0xab)}
	mut writes := 0
	for _ in 0 .. 500 {
		pt.write(payload) or { return writes }
		writes++
	}
	return writes
}

// One thread reads, one writes, and a third closes mid-flight: the adapter
// must serialize all TLS-context access so this never crashes or hangs (this
// codebase tests concurrency by repetition, matching h2_mux_conn_test.v).
// Both hammer threads returning at all — rather than crashing or blocking
// forever past the harness timeout — is the property under test.
fn test_h2_pooled_transport_concurrent_read_write_close() {
	for round in 0 .. 3 {
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
			cert:     h2t_cert_path
			cert_key: h2t_key_path
			validate: false
		}) or {
			assert false, 'listener: ${err}'
			return
		}
		th := spawn h2t_echo_loop(mut listener)
		mut sconn := ssl.new_ssl_conn(validate: false) or {
			assert false, 'ssl conn: ${err}'
			return
		}
		sconn.dial('127.0.0.1', port) or {
			assert false, 'dial: ${err}'
			return
		}
		mut pt := new_h2_pooled_transport(mut sconn)
		rt := spawn h2t_hammer_reader(mut pt)
		wt := spawn h2t_hammer_writer(mut pt)
		time.sleep(200 * time.millisecond)
		pt.close()
		writes := wt.wait()
		reads := rt.wait()
		assert writes >= 0 && reads >= 0, 'round ${round}: hammer threads must terminate cleanly'
		// A second close must be a no-op, not a double-free.
		pt.close()
		listener.shutdown() or {}
		th.wait()
	}
}
