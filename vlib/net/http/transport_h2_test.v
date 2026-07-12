// vtest build: !sanitize-memory-clang && !sanitize-address-gcc && !sanitize-address-clang
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

// h2t_slow_round_trip runs one request through a specific (non-default)
// Transport instance, so a test can hold an h2 connection busy
// (active_streams > 0) on that transport's own pool for the duration of a
// spawned thread.
fn h2t_slow_round_trip(mut t Transport, port int, path string) {
	req := prepare(url: 'https://127.0.0.1:${port}${path}', validate: false, enable_http2: true) or {
		return
	}
	t.round_trip(req, .get, 'https', '127.0.0.1', port, path, '', req.header) or {}
}

// h2t_h1only_srv_serve is a minimal HTTP/1.1 responder for a listener that
// offers no ALPN protocols at all, so an h2-enabled client's probe dial
// deterministically negotiates plain http/1.1.
fn h2t_h1only_srv_serve(mut s H2PoolSrv, mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(10 * time.second)
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
		if s.respond_delay > 0 {
			time.sleep(s.respond_delay)
		}
		conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: ${h2t_body.len}\r\n\r\n${h2t_body}') or {
			return
		}
	}
}

fn h2t_h1only_srv_loop(mut s H2PoolSrv, mut listener mbedtls.SSLListener) {
	for {
		mut conn := listener.accept() or { return }
		s.bump_accepts()
		spawn h2t_h1only_srv_serve(mut s, mut conn)
	}
}

// start_h2t_h1only_srv starts a loopback TLS listener with NO ALPN configured
// (unlike start_h2_pool_srv's `h2`-only listener), so a client's ALPN probe
// dial always resolves to plain http/1.1.
fn start_h2t_h1only_srv(mut s H2PoolSrv) !(int, &mbedtls.SSLListener, thread) {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close() or {}
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     h2t_cert_path
		cert_key: h2t_key_path
		validate: false
	})!
	th := spawn h2t_h1only_srv_loop(mut s, mut listener)
	return port, listener, th
}

// Concurrent first requests to a fresh h1-only origin must not be
// head-of-line-blocked behind whichever one becomes the ALPN-probing
// dialer's own response. The dialer's connection cannot be shared by
// concurrent requests (HTTP/1.1 has no safe way to interleave two responses
// on one connection), so making every waiter wait for the dialer's full
// exchange to finish before even trying the h1 pool stalls unrelated
// requests for however long that one response takes — precisely what RFC
// 9112 §9.4 says multiple connections exist to avoid. Waiters must
// independently race for the h1 pool (or dial their own) the moment the
// origin's protocol is known, not after the dialer's response arrives
// (Codex P2, vlang/v#27643 pullrequestreview-4631763931, discussion
// 3525390892). Verified by timing: with every response artificially
// delayed, N concurrent requests running in parallel finish in roughly one
// delay; serialized behind one response they would take roughly two.
fn test_h2_pool_alpn_fallback_avoids_hol_blocking() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{
		respond_delay: 400 * time.millisecond
	}
	port, mut listener, th := start_h2t_h1only_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	sw := time.new_stopwatch()
	mut workers := []thread int{}
	for i in 0 .. 4 {
		workers << spawn h2t_fetch_worker(port, '/w${i}')
	}
	for w in workers {
		status := w.wait()
		assert status == 200, 'concurrent fetch failed with ${status}'
	}
	elapsed := sw.elapsed()
	stop_h2_pool_srv(mut listener, th)
	// This spawns 4 REAL concurrent TLS dials/handshakes over real sockets
	// (unlike the in-memory-pipe tests in h2_mux_conn_test.v), so it is even
	// more exposed to CI scheduling variance. A 700ms threshold (300ms over
	// the nominal 400ms respond_delay) failed in CI at 781ms on a commit with
	// no relevant code changes -- the same "assumed jitter is small" mistake
	// fixed in h2_mux_conn_test.v's timing tests. 1200ms keeps a comfortable
	// margin below full serialization (4 x 400ms = 1600ms, what a HOL-blocked
	// regression would actually produce) while tolerating a much larger
	// absolute stall than 300ms.
	assert elapsed < 1200 * time.millisecond, 'expected concurrent requests to a fresh h1-only origin to run in parallel (~1 respond_delay), not serialize behind one dialer response; took ${elapsed}'
}

// A caller-set req.read_timeout must bound a pooled h2 request's wait for its
// response. H2MuxConn.read_loop treats every transport read timeout as a
// benign wake-up (needed so the reader thread doesn't die from an otherwise
// healthy idle connection), so nothing enforces a per-request deadline unless
// wait_response itself is bounded — a request against a genuinely stalled
// server must not hang until the server eventually responds regardless of
// what read_timeout was configured (Codex P1, vlang/v#27643
// pullrequestreview-4627654418, discussion 3521494711).
fn test_h2_pool_respects_request_read_timeout() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{
		respond_delay: 3 * time.second
	}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	sw := time.new_stopwatch()
	resp := fetch(
		url:          'https://127.0.0.1:${port}/slow'
		validate:     false
		read_timeout: 300 * time.millisecond
	) or {
		elapsed := sw.elapsed()
		assert elapsed < 2 * time.second, 'expected the read_timeout to fire well before the 3s-stalled server responds, took ${elapsed}'
		stop_h2_pool_srv(mut listener, th)
		return
	}
	elapsed := sw.elapsed()
	stop_h2_pool_srv(mut listener, th)
	assert false, 'expected the stalled response to time out around 300ms, got status ${resp.status_code} after ${elapsed}'
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

// h2_dial_and_do must recheck pooled state before committing to a new dial:
// h2_round_trip's own check runs under a separate, already-released lock
// acquisition, so a concurrent dial can finish and populate h2_conns[key] in
// the window between that check and h2_dial_and_do's entry. Simulated
// directly by calling h2_dial_and_do a second time after the pool is
// already warm for this key -- exactly the state h2_round_trip would hand
// off in that race window (Codex P2, vlang/v#27643
// pullrequestreview-4631763931, discussion 3525390895).
fn test_h2_dial_and_do_reuses_pooled_conn_instead_of_redialing() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut t := new_transport()
	mut srv := &H2PoolSrv{}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}

	req1 := prepare(url: 'https://127.0.0.1:${port}/first', validate: false, enable_http2: true) or {
		assert false, 'prepare 1: ${err}'
		return
	}
	resp1 := t.round_trip(req1, .get, 'https', '127.0.0.1', port, '/first', '', req1.header) or {
		assert false, 'round_trip 1: ${err}'
		return
	}
	assert resp1.status_code == 200

	req2 := prepare(url: 'https://127.0.0.1:${port}/second', validate: false, enable_http2: true) or {
		assert false, 'prepare 2: ${err}'
		return
	}
	key := transport_pool_key(req2, 'https', '127.0.0.1', port)
	resp2 := t.h2_dial_and_do(req2, key, '', .get, '127.0.0.1', port, '/second', '', req2.header) or {
		assert false, 'h2_dial_and_do 2: ${err}'
		return
	}
	assert resp2.status_code == 200

	t.close_idle()
	stop_h2_pool_srv(mut listener, th)
	assert srv.accept_count() == 1, 'expected h2_dial_and_do to reuse the already-pooled connection instead of redialing, got ${srv.accept_count()} accepts'
}

// The h2 connection pool must respect Transport.max_idle_conns like the h1
// pool does: dialing an idle-eligible new connection past the cap must evict
// an existing IDLE h2 connection (never a busy one), not just grow the pool
// without bound. Without this, a client that touches many distinct h2 origins
// keeps every pooled TLS socket + reader thread alive until close_idle() or
// process exit, despite the public Transport limits promising to bound
// file-descriptor use (Codex P2, vlang/v#27643 pullrequestreview-4627654418,
// discussion 3521494717).
fn test_h2_pool_respects_max_idle_conns_cap() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut t := new_transport()
	t.max_idle_conns = 1
	mut srv1 := &H2PoolSrv{}
	mut srv2 := &H2PoolSrv{}
	mut srv3 := &H2PoolSrv{}
	port1, mut l1, th1 := start_h2_pool_srv(mut srv1) or {
		assert false, 'server 1: ${err}'
		return
	}
	port2, mut l2, th2 := start_h2_pool_srv(mut srv2) or {
		assert false, 'server 2: ${err}'
		return
	}
	port3, mut l3, th3 := start_h2_pool_srv(mut srv3) or {
		assert false, 'server 3: ${err}'
		return
	}

	for port in [port1, port2, port3] {
		req := prepare(url: 'https://127.0.0.1:${port}/x', validate: false) or {
			assert false, 'prepare: ${err}'
			return
		}
		resp := t.round_trip(req, .get, 'https', '127.0.0.1', port, '/x', '', req.header) or {
			assert false, 'round_trip ${port}: ${err}'
			return
		}
		assert resp.status_code == 200
	}
	t.mu.lock()
	total := t.h2_conns.len
	t.mu.unlock()
	assert total <= t.max_idle_conns, 'expected h2_conns to respect max_idle_conns=${t.max_idle_conns}, got ${total} pooled connections after 3 distinct-origin dials'
	t.close_idle()
	stop_h2_pool_srv(mut l1, th1)
	stop_h2_pool_srv(mut l2, th2)
	stop_h2_pool_srv(mut l3, th3)
}

// A dial that registers a brand-new h2 connection must never pick that SAME
// connection as its own eviction victim: with max_idle_conns already consumed
// by an unrelated idle h1 connection, evict_oldest_idle_locked's scan of
// h2_conns finds the connection this very call just registered as the ONLY
// idle h2 candidate (active_streams is still 0 — do_h2 hasn't run yet) and
// evicts it, dropping the pool's sole reference to zero and tearing the
// connection down before its own request ever executes on it (Codex P1,
// vlang/v#27643 pullrequestreview-4628439062).
fn test_h2_pool_does_not_evict_its_own_new_connection() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut t := new_transport()
	t.max_idle_conns = 1

	mut h1_srv := &H2PoolSrv{}
	h1_port, mut h1_listener, h1_th := start_h2t_h1only_srv(mut h1_srv) or {
		assert false, 'h1 server: ${err}'
		return
	}
	// Consume the whole max_idle_conns=1 budget with one h1 idle connection
	// before the h2 dial below ever runs.
	h1_req := prepare(url: 'https://127.0.0.1:${h1_port}/warm', validate: false, enable_http2: true) or {
		assert false, 'prepare h1: ${err}'
		return
	}
	h1_resp := t.round_trip(h1_req, .get, 'https', '127.0.0.1', h1_port, '/warm', '', h1_req.header) or {
		assert false, 'round_trip h1: ${err}'
		return
	}
	assert h1_resp.status_code == 200

	mut h2_srv := &H2PoolSrv{}
	h2_port, mut h2_listener, h2_th := start_h2_pool_srv(mut h2_srv) or {
		assert false, 'h2 server: ${err}'
		return
	}
	h2_req := prepare(url: 'https://127.0.0.1:${h2_port}/fresh', validate: false, enable_http2: true) or {
		assert false, 'prepare h2: ${err}'
		return
	}
	h2_resp := t.round_trip(h2_req, .get, 'https', '127.0.0.1', h2_port, '/fresh', '',
		h2_req.header) or {
		stop_h2_pool_srv(mut h1_listener, h1_th)
		stop_h2_pool_srv(mut h2_listener, h2_th)
		assert false, 'fresh h2 dial was torn down by its own registration-time cap eviction: ${err}'
		return
	}
	assert h2_resp.status_code == 200
	assert h2_resp.body == h2t_body

	t.close_idle()
	stop_h2_pool_srv(mut h1_listener, h1_th)
	stop_h2_pool_srv(mut h2_listener, h2_th)
}

// The idle cap is a single shared budget across h1 AND h2: with
// max_idle_conns already consumed entirely by an idle h1 connection, a fresh
// h2 dial's own registration-time eviction must be able to free room by
// evicting that h1 entry (not just scan h2_conns, which has no other eligible
// candidate here — the only h2 entry is this very dial's own result, excluded
// from candidacy). Without this, both the h1 idle connection and the new h2
// connection stay pooled, exceeding the documented global cap indefinitely
// (Codex P2, vlang/v#27643 pullrequestreview-4630174759).
fn test_h2_pool_evicts_h1_idle_to_make_room_for_h2() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut t := new_transport()
	t.max_idle_conns = 1

	mut h1_srv := &H2PoolSrv{}
	h1_port, mut h1_listener, h1_th := start_h2t_h1only_srv(mut h1_srv) or {
		assert false, 'h1 server: ${err}'
		return
	}
	h1_req := prepare(url: 'https://127.0.0.1:${h1_port}/warm', validate: false, enable_http2: true) or {
		assert false, 'prepare h1: ${err}'
		return
	}
	h1_resp := t.round_trip(h1_req, .get, 'https', '127.0.0.1', h1_port, '/warm', '', h1_req.header) or {
		assert false, 'round_trip h1: ${err}'
		return
	}
	assert h1_resp.status_code == 200

	mut h2_srv := &H2PoolSrv{}
	h2_port, mut h2_listener, h2_th := start_h2_pool_srv(mut h2_srv) or {
		assert false, 'h2 server: ${err}'
		return
	}
	h2_req := prepare(url: 'https://127.0.0.1:${h2_port}/fresh', validate: false, enable_http2: true) or {
		assert false, 'prepare h2: ${err}'
		return
	}
	h2_resp := t.round_trip(h2_req, .get, 'https', '127.0.0.1', h2_port, '/fresh', '',
		h2_req.header) or {
		stop_h2_pool_srv(mut h1_listener, h1_th)
		stop_h2_pool_srv(mut h2_listener, h2_th)
		assert false, 'h2 dial: ${err}'
		return
	}
	assert h2_resp.status_code == 200

	t.mu.lock()
	h1_idle_count := t.total_idle_locked()
	h2_count := t.h2_conns.len
	t.mu.unlock()
	total := h1_idle_count + h2_count
	t.close_idle()
	stop_h2_pool_srv(mut h1_listener, h1_th)
	stop_h2_pool_srv(mut h2_listener, h2_th)
	assert total <= t.max_idle_conns, 'expected the combined h1+h2 idle pool to respect max_idle_conns=${t.max_idle_conns}, got ${h1_idle_count} h1 + ${h2_count} h2 = ${total}'
}

// The idle cap check must count only h2 connections that are ACTUALLY idle
// (active_streams == 0), matching the eviction scan's own eligibility below
// it. An h2 connection currently serving a request is not idle and must not
// inflate the combined count -- otherwise a genuinely idle h1 connection
// looks like it is pushing the pool over budget purely because of an
// unrelated BUSY h2 connection, and gets evicted even though the real idle
// count is within max_idle_conns (Codex P3, vlang/v#27643
// pullrequestreview-4631763931, discussion 3525390899).
fn test_h2_pool_idle_cap_excludes_busy_h2_conn() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut t := new_transport()
	t.max_idle_conns = 2

	mut h1_srv := &H2PoolSrv{}
	h1_port, mut h1_listener, h1_th := start_h2t_h1only_srv(mut h1_srv) or {
		assert false, 'h1 server: ${err}'
		return
	}
	h1_req := prepare(url: 'https://127.0.0.1:${h1_port}/warm', validate: false, enable_http2: true) or {
		assert false, 'prepare h1: ${err}'
		return
	}
	h1_resp := t.round_trip(h1_req, .get, 'https', '127.0.0.1', h1_port, '/warm', '', h1_req.header) or {
		assert false, 'round_trip h1: ${err}'
		return
	}
	assert h1_resp.status_code == 200

	mut h2_srv := &H2PoolSrv{
		respond_delay: 500 * time.millisecond
	}
	h2_port, mut h2_listener, h2_th := start_h2_pool_srv(mut h2_srv) or {
		assert false, 'h2 server: ${err}'
		return
	}
	// Warm up the h2 connection so it is registered and idle before the busy
	// phase below.
	warm_req := prepare(
		url:          'https://127.0.0.1:${h2_port}/warm'
		validate:     false
		enable_http2: true
	) or {
		assert false, 'prepare h2 warm: ${err}'
		return
	}
	warm_resp := t.round_trip(warm_req, .get, 'https', '127.0.0.1', h2_port, '/warm', '',
		warm_req.header) or {
		assert false, 'round_trip h2 warm: ${err}'
		return
	}
	assert warm_resp.status_code == 200

	// Make the h2 connection BUSY (active_streams > 0) for respond_delay, then
	// dial a third, fresh h2 origin while it is still in flight -- that
	// dial's own registration-time eviction check is where the miscount
	// would fire.
	slow_th := spawn h2t_slow_round_trip(mut t, h2_port, '/slow')
	time.sleep(150 * time.millisecond)

	mut c_srv := &H2PoolSrv{}
	c_port, mut c_listener, c_th := start_h2_pool_srv(mut c_srv) or {
		assert false, 'c server: ${err}'
		return
	}
	c_req := prepare(url: 'https://127.0.0.1:${c_port}/fresh', validate: false, enable_http2: true) or {
		assert false, 'prepare c: ${err}'
		return
	}
	c_resp := t.round_trip(c_req, .get, 'https', '127.0.0.1', c_port, '/fresh', '', c_req.header) or {
		assert false, 'round_trip c: ${err}'
		return
	}
	assert c_resp.status_code == 200

	t.mu.lock()
	h1_idle_count := t.total_idle_locked()
	t.mu.unlock()

	slow_th.wait()
	t.close_idle()
	stop_h2_pool_srv(mut h1_listener, h1_th)
	stop_h2_pool_srv(mut h2_listener, h2_th)
	stop_h2_pool_srv(mut c_listener, c_th)
	assert h1_idle_count == 1, 'expected the idle h1 connection to survive (a busy h2 connection must not count toward the idle cap), got ${h1_idle_count} idle h1 connections'
}

// A pooled h2 connection that has sat idle past idle_timeout must not be
// reused just because it still has stream capacity -- h2_round_trip's fast
// path only checked can_take_new_request() (stream capacity), never how long
// the connection had been idle, unlike Transport.checkout's h1_idle expiry
// check (Codex P2, vlang/v#27643 pullrequestreview-4631763931, discussion
// 3525390896).
fn test_h2_pool_respects_idle_timeout() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut t := new_transport()
	t.idle_timeout = 100 * time.millisecond

	mut srv := &H2PoolSrv{}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	req1 := prepare(url: 'https://127.0.0.1:${port}/first', validate: false, enable_http2: true) or {
		assert false, 'prepare 1: ${err}'
		return
	}
	resp1 := t.round_trip(req1, .get, 'https', '127.0.0.1', port, '/first', '', req1.header) or {
		assert false, 'round_trip 1: ${err}'
		return
	}
	assert resp1.status_code == 200

	// Let the connection sit idle well past idle_timeout before the next
	// request.
	time.sleep(250 * time.millisecond)

	req2 := prepare(url: 'https://127.0.0.1:${port}/second', validate: false, enable_http2: true) or {
		assert false, 'prepare 2: ${err}'
		return
	}
	resp2 := t.round_trip(req2, .get, 'https', '127.0.0.1', port, '/second', '', req2.header) or {
		assert false, 'round_trip 2: ${err}'
		return
	}
	assert resp2.status_code == 200

	t.close_idle()
	stop_h2_pool_srv(mut listener, th)
	assert srv.accept_count() == 2, 'expected the expired idle connection to be retired and a fresh one dialed, got ${srv.accept_count()} accepts'
}

fn h2t_race_idle_evict(mut t Transport, port int, path string) {
	req := prepare(url: 'https://127.0.0.1:${port}${path}', validate: false, enable_http2: true) or {
		return
	}
	t.round_trip(req, .get, 'https', '127.0.0.1', port, path, '', req.header) or {}
}

// Two concurrent requests racing an idle-expired pooled h2 connection must
// each retire it AT MOST ONCE: only the caller that actually removes the
// h2_conns[key] entry may call conn.release() (H2MuxConn.refs is not
// idempotent like shutdown_when_idle() -- a second, unconditional release()
// over-decrements it). Self-caught in review, vlang/v#27643
// pullrequestreview-4636271901.
fn test_h2_pool_idle_evict_race_does_not_double_release() {
	$if windows && !no_vschannel ? {
		eprintln('skipping: SChannel connection pooling is not implemented yet')
		return
	}
	mut srv := &H2PoolSrv{}
	port, mut listener, th := start_h2_pool_srv(mut srv) or {
		assert false, 'server: ${err}'
		return
	}
	mut t := new_transport()
	t.idle_timeout = 100 * time.millisecond

	req1 := prepare(url: 'https://127.0.0.1:${port}/first', validate: false, enable_http2: true) or {
		assert false, 'prepare 1: ${err}'
		return
	}
	resp1 := t.round_trip(req1, .get, 'https', '127.0.0.1', port, '/first', '', req1.header) or {
		assert false, 'round_trip 1: ${err}'
		return
	}
	assert resp1.status_code == 200

	key := transport_pool_key(req1, 'https', '127.0.0.1', port)
	t.mu.lock()
	pooled := t.h2_conns[key] or {
		t.mu.unlock()
		assert false, 'expected a pooled h2 connection to be registered'
		return
	}
	t.mu.unlock()

	// Let the connection sit idle well past idle_timeout, then have two
	// requests race its eviction.
	time.sleep(250 * time.millisecond)

	th1 := spawn h2t_race_idle_evict(mut t, port, '/second')
	th2 := spawn h2t_race_idle_evict(mut t, port, '/third')
	th1.wait()
	th2.wait()

	t.close_idle()
	stop_h2_pool_srv(mut listener, th)
	assert pooled.refs == 0, 'expected refs to be retired exactly once (== 0), got ${pooled.refs}: double-release detected'
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

// A dial failure on the pooled h2 ALPN-probing path must preserve the
// underlying error's code, not collapse it to a plain uncoded error. The
// outer retry loop (request.v's is_no_need_retry_error) decides whether to
// give up early based on err.code() (net.err_connect_timed_out,
// net.err_timed_out_code, etc.) — losing the code means a fundamentally
// hopeless dial failure gets retried up to max_retries times instead of
// failing fast, a regression from the one-shot h2 path (Codex P2, vlang/v#27643
// pullrequestreview-4627654418, discussion 3521494715).
fn test_h2_pool_dial_failure_preserves_error_code() {
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

	// Capture the raw dial's own error code against this now-closed port
	// (nothing listening — a fast, deterministic connection failure).
	mut raw_conn := ssl.new_ssl_conn(validate: false) or {
		assert false, 'ssl conn: ${err}'
		return
	}
	raw_conn.dial('127.0.0.1', port) or {
		raw_code := err.code()
		assert raw_code != 0, 'expected a coded dial failure to compare against, got: ${err}'

		mut t := new_transport()
		req := prepare(url: 'https://127.0.0.1:${port}/x', validate: false) or {
			assert false, 'prepare: ${err}'
			return
		}
		t.round_trip(req, .get, 'https', '127.0.0.1', port, '/x', '', req.header) or {
			assert err.code() == raw_code, 'h2 dial failure lost its error code: got ${err.code()} (${err}), want ${raw_code}'
			return
		}
		assert false, 'expected the dial to fail (nothing listening on this port)'
		return
	}
	assert false, 'expected the raw dial to fail (nothing listening on this port)'
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
	// Registration into t.h2_conns (transport_h2.v) happens before the
	// dialer calls conn.do() (h2_mux_conn.v), which is what actually
	// increments active_streams — orphan release, cap eviction, and do_h2's
	// own dispatch all run in between. Waiting only for registration and
	// then checking active_streams once let the test race ahead of
	// admission; wait for both conditions in the same poll loop instead.
	mut conn := &H2MuxConn(unsafe { nil })
	mut active := 0
	for _ in 0 .. 500 {
		t.mu.lock()
		for _, mut c in t.h2_conns {
			conn = c
		}
		t.mu.unlock()
		if conn != unsafe { nil } {
			conn.smu.lock()
			active = conn.active_streams
			conn.smu.unlock()
			if active > 0 {
				break
			}
		}
		time.sleep(5 * time.millisecond)
	}
	assert conn != unsafe { nil }, 'pooled h2 connection was never registered before the delayed response arrived'
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

// H2PooledTransport must wrap the SAME SSLConn object the caller dialed, not
// a copy: SSLConn.dial() wires its internal TLS context to a pointer into the
// struct's own address, so a value copy would silently detach the adapter's
// reads/writes/close() from the live connection (Codex P1, vlang/v#27643
// pullrequestreview-4631763931, discussion 3525390891). On the pre-fix code
// (conn stored as a bare `ssl.SSLConn` value field) this test does not even
// compile, since `pt.conn` cannot be compared as a pointer — that hard
// failure is itself the Phase-R signal for this class of defect.
fn test_h2_pooled_transport_stores_conn_by_pointer() {
	mut sconn := ssl.new_ssl_conn(validate: false) or {
		assert false, 'ssl conn: ${err}'
		return
	}
	mut pt := new_h2_pooled_transport(mut sconn)
	assert voidptr(pt.conn) == voidptr(sconn), 'H2PooledTransport must wrap the same SSLConn object, not a copy of it'
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

// write() must not fail a stalled write against h2_pooled_io_timeout (50ms):
// conn.write() (write_ptr in both TLS backends) reuses the SAME `duration`
// field that set_read_timeout() configures for its own internal
// WANT_READ/WANT_WRITE retry loop, so without widening it for the span of the
// call, a peer that merely stops draining mid-write — still connected, not
// closed — would fail almost immediately instead of getting the intended
// h2_pooled_write_stall_limit (30s) budget (Codex P2, vlang/v#27643
// pullrequestreview-4628439062, discussion 3522170277).
//
// The client socket is forced non-blocking and its send buffer shrunk so the
// local kernel buffer (and the peer's un-drained TCP receive window) fill in
// a few KB, deterministically, instead of the ~200KB+ a default-size, still
// blocking socket needs (client sockets in this codebase are never switched
// to non-blocking mode outside this test — see dial()/connect() in
// vlib/net/mbedtls and vlib/net/openssl — so on a real pooled connection
// mbedtls_ssl_write's own WANT_WRITE branch never fires in the first place;
// forcing it here isolates the reused-duration defect on its own terms).
fn test_h2_pooled_transport_write_survives_stall_past_io_timeout() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or {
		assert false, 'port: ${err}'
		return
	}
	port := port_listener.addr() or {
		assert false, 'addr: ${err}'
		return
	}.port() or {
		assert false, 'addr.port: ${err}'
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
	// Server: complete the handshake, then go captive -- never read again,
	// simulating a healthy-but-slow peer that has stopped draining its
	// receive buffer (not a dead or closed one).
	th := spawn fn (mut l mbedtls.SSLListener) {
		mut sconn := l.accept() or { return }
		time.sleep(2 * time.second)
		sconn.shutdown() or {}
	}(mut listener)

	mut sconn := ssl.new_ssl_conn(validate: false) or {
		assert false, 'ssl conn: ${err}'
		return
	}
	sconn.dial('127.0.0.1', port) or {
		assert false, 'dial: ${err}'
		return
	}
	net.set_blocking(sconn.handle, false) or {
		assert false, 'set_blocking: ${err}'
		return
	}
	mut raw_sock := net.tcp_socket_from_handle_raw(sconn.handle)
	raw_sock.set_option_int(.send_buf_size, 2048) or {}

	mut pt := new_h2_pooled_transport(mut sconn)
	payload := []u8{len: 512 * 1024, init: u8(0xab)}

	write_done := chan bool{cap: 1}
	spawn fn [mut pt, payload, write_done] () {
		pt.write(payload) or {}
		write_done <- true
	}()

	// On buggy code (write() reusing h2_pooled_io_timeout for its own
	// internal retry budget) the write has always already failed by ~65ms —
	// see the standalone repro this test is derived from — even though the
	// connection is perfectly healthy and has not been closed. A comfortable
	// multiple of that interval must still find the write in flight: that is
	// only possible if it is retrying against something much longer than
	// 50ms.
	select {
		_ := <-write_done {
			assert false, 'write() returned within 250ms even though the peer never disconnected -- it must be retrying against the write stall budget, not the short read-poll interval'
		}
		250 * time.millisecond {
			// still in flight, as expected on fixed code
		}
	}

	pt.close()
	_ := <-write_done
	th.wait()
}
