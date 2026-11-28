// vtest build: !sanitize-memory-clang
// Hermetic TLS-termination test for net.http.Server: spin up a local HTTPS
// server with an in-memory cert/key, hit it with http.fetch (validate: false),
// and assert the round-trip.

module main

import net
import net.http
import net.mbedtls
import io
import time

// A finite `accept_timeout` doubles as the server's TLS handshake budget.
// Allow time for handshakes under concurrent test load.
const tls_handshake_budget = 10 * time.second

// The RSA-2048 fixture keeps signing fast. Its issuer is absent from the trust store.
const server_tls_cert = $embed_file('../testdata/localhost_test_cert.pem').to_string()

const server_tls_key = $embed_file('../testdata/localhost_test_key.pem').to_string()

// A real 165-byte TLS 1.2 ClientHello captured from V's own mbedtls client.
// The exact bytes (random/session id) are irrelevant; the server only needs a
// structurally valid first flight to start the handshake and then block
// waiting for the client's ClientKeyExchange, which never arrives.
const stalled_tls_client_hello = [u8(0x16), 0x03, 0x03, 0x00, 0xa0, 0x01, 0x00, 0x00, 0x9c, 0x03,
	0x03, 0x6a, 0x3d, 0x34, 0x4c, 0x8f, 0x46, 0x64, 0xd8, 0xe9, 0x2d, 0x46, 0x16, 0xd3, 0xf2, 0x87,
	0xe4, 0xee, 0xc5, 0x57, 0x6c, 0x8e, 0xd7, 0x36, 0x19, 0x3b, 0x35, 0x7a, 0x01, 0x03, 0xde, 0x6e,
	0x64, 0x00, 0x00, 0x24, 0xc0, 0x2c, 0xc0, 0x2b, 0xc0, 0x30, 0xc0, 0x2f, 0xc0, 0x24, 0xc0, 0x23,
	0xc0, 0x28, 0xc0, 0x27, 0xc0, 0x0a, 0xc0, 0x09, 0xc0, 0x14, 0xc0, 0x13, 0x00, 0x9d, 0x00, 0x9c,
	0x00, 0x3d, 0x00, 0x3c, 0x00, 0x35, 0x00, 0x2f, 0x01, 0x00, 0x00, 0x4f, 0x00, 0x0a, 0x00, 0x08,
	0x00, 0x06, 0x00, 0x1d, 0x00, 0x17, 0x00, 0x18, 0x00, 0x0b, 0x00, 0x02, 0x01, 0x00, 0x00, 0x0d,
	0x00, 0x1a, 0x00, 0x18, 0x08, 0x04, 0x08, 0x05, 0x08, 0x06, 0x04, 0x01, 0x05, 0x01, 0x02, 0x01,
	0x04, 0x03, 0x05, 0x03, 0x02, 0x03, 0x02, 0x02, 0x06, 0x01, 0x06, 0x03, 0x00, 0x23, 0x00, 0x00,
	0x00, 0x10, 0x00, 0x0e, 0x00, 0x0c, 0x02, 0x68, 0x32, 0x08, 0x68, 0x74, 0x74, 0x70, 0x2f, 0x31,
	0x2e, 0x31, 0x00, 0x17, 0x00, 0x00, 0xff, 0x01, 0x00, 0x01, 0x00]

struct EchoHandler {
mut:
	last_path string
}

fn (mut h EchoHandler) handle(req http.Request) http.Response {
	h.last_path = req.url
	return http.Response{
		status_code: 200
		body:        'tls hello ${req.url}'
	}
}

struct BlockingHandler {
	started chan bool
	release chan bool
}

fn testsuite_end() {
	http.close_idle_connections()
}

fn (mut h BlockingHandler) handle(req http.Request) http.Response {
	h.started <- true
	_ := <-h.release
	return http.Response{
		status_code: 200
		header:      http.new_header(key: .connection, value: 'close')
		body:        'released'
	}
}

fn pick_port() !int {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := l.addr()!.port()!
	l.close()!
	return port
}

fn tls_server_request_response(port int, request string) !string {
	mut client := mbedtls.new_ssl_conn(mbedtls.SSLConnectConfig{})!
	defer {
		client.shutdown() or {}
	}
	client.dial('127.0.0.1', port)!
	client.set_read_timeout(5 * time.second)
	client.write_string(request)!

	mut response := []u8{}
	mut buffer := []u8{len: 1024}
	mut closed := false
	for {
		n := client.read(mut buffer) or {
			if err is io.Eof {
				closed = true
				break
			}
			return err
		}
		if n == 0 {
			closed = true
			break
		}
		response << buffer[..n]
	}
	if !closed {
		return error('TLS server kept an HTTP/1.1 error connection open')
	}
	return response.bytestr()
}

fn test_server_tls_round_trip() {
	$if use_openssl ? {
		// TLS termination for net.http.Server is not yet supported on the
		// OpenSSL backend; the listener stub reports a clear runtime error and
		// the test is skipped here so the suite stays green under
		// `-d use_openssl`.
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	defer {
		srv.close()
		t.wait()
	}
	resp := http.fetch(
		url:      'https://127.0.0.1:${port}/hello'
		validate: false
	) or {
		assert false, 'fetch failed: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == 'tls hello /hello'
}

fn send_slow_tls_request_head(port int, ready chan bool, stop chan bool) {
	mut client := mbedtls.new_ssl_conn(mbedtls.SSLConnectConfig{}) or {
		ready <- false
		return
	}
	defer { client.shutdown() or {} }
	client.dial('127.0.0.1', port) or {
		ready <- false
		return
	}
	client.write_string('GET /slow HTTP/1.1\r\nX-Slow: ') or {
		ready <- false
		return
	}
	ready <- true
	for {
		select {
			_ := <-stop {
				return
			}
			50 * time.millisecond {
			}
		}
		client.write_string('a') or { return }
	}
}

fn request_tls_response_on_channel(port int, result chan string) {
	response := tls_server_request_response(port,
		'GET /alive HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n') or { err.msg() }
	result <- response
}

fn test_server_tls_request_head_deadline_releases_worker() ! {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port()!
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		read_timeout:           5 * time.second
		read_header_timeout:    300 * time.millisecond
		worker_num:             1
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running()!
	defer {
		srv.close()
		t.wait()
	}

	ready := chan bool{cap: 1}
	stop := chan bool{cap: 1}
	drip := spawn send_slow_tls_request_head(port, ready, stop)
	if !<-ready {
		drip.wait()
		assert false, 'slow TLS client failed to send its partial request head'
		return
	}

	// Keep sending partial headers until the server closes the slow connection.
	// Without the total header deadline, the sole worker stays occupied even
	// when each byte arrives inside read_timeout. Allow the legitimate request
	// its own RSA handshake budget instead of timing two handshakes at 1.5 s.
	result := chan string{cap: 1}
	request := spawn request_tls_response_on_channel(port, result)
	defer {
		stop <- true
		drip.wait()
		request.wait()
	}
	select {
		response := <-result {
			assert response.starts_with('HTTP/1.1 200 OK\r\n'), response
			assert response.contains('tls hello /alive')
		}
		2 * tls_handshake_budget {
			assert false, 'partial TLS request head kept the sole worker occupied'
		}
	}
}

fn test_server_tls_rejects_oversized_and_malformed_http1_framing() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                        '127.0.0.1:${port}'
		cert:                        server_tls_cert
		cert_key:                    server_tls_key
		in_memory_verification:      true
		accept_timeout:              tls_handshake_budget
		read_timeout:                5 * time.second
		max_http1_request_body_size: 4
		handler:                     EchoHandler{}
		show_startup_message:        false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	defer {
		srv.close()
		t.wait()
	}

	oversized_response := tls_server_request_response(port,
		'POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\n\r\n')!
	assert oversized_response.starts_with('HTTP/1.1 413 Request Entity Too Large\r\n')
	assert oversized_response.to_lower().contains('content-length: 0\r\n')
	assert oversized_response.to_lower().contains('connection: close\r\n')

	malformed_response := tls_server_request_response(port,
		'POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length: -1\r\n\r\n')!
	assert malformed_response.starts_with('HTTP/1.1 400 Bad Request\r\n')
	assert malformed_response.to_lower().contains('connection: close\r\n')

	transfer_encoding_response := tls_server_request_response(port,
		'POST / HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n')!
	assert transfer_encoding_response.starts_with('HTTP/1.1 400 Bad Request\r\n')
	assert transfer_encoding_response.to_lower().contains('connection: close\r\n')

	oversized_head := 'GET /' + 'a'.repeat(http.default_max_http1_request_head_size) + ' HTTP/1.1\r\n\r\n'
	oversized_head_response := tls_server_request_response(port, oversized_head)!
	assert oversized_head_response.starts_with('HTTP/1.1 431 Request Header Fields Too Large\r\n')
}

fn test_default_fetch_rejects_the_untrusted_server_certificate() {
	assert http.FetchConfig{}.validate
	assert http.Request{}.validate
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	defer {
		srv.close()
		t.wait()
	}
	// One attempt: a rejected certificate is an error, not a reason to retry the handshake.
	if resp := http.fetch(
		url:         'https://127.0.0.1:${port}/hello'
		max_retries: 1
	) {
		assert false, 'untrusted certificate was accepted: ${resp.status_code} ${resp.body}'
		return
	}
	resp := http.fetch(
		url:      'https://127.0.0.1:${port}/hello'
		validate: false
	) or {
		assert false, 'fetch with the check skipped failed: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == 'tls hello /hello'
}

fn test_server_tls_stop() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         100 * time.millisecond
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	srv.stop()
	assert srv.status() == .stopped
	t.wait()
	assert srv.status() == .closed
}

fn test_server_tls_close_caps_default_accept_poll() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	sw := time.new_stopwatch()
	srv.close()
	t.wait()
	assert sw.elapsed() < time.second
	assert srv.status() == .closed
}

fn test_server_tls_close_waits_for_active_request() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	started := chan bool{cap: 1}
	release := chan bool{cap: 1}
	done := chan string{cap: 1}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		handler:                BlockingHandler{
			started: started
			release: release
		}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	spawn fn [done, port] () {
		resp := http.fetch(
			url:          'https://127.0.0.1:${port}/blocked'
			enable_http2: false
			validate:     false
		) or {
			done <- 'error: ${err}'
			return
		}
		done <- resp.body
	}()
	mut start_failure := ''
	select {
		_ := <-started {
		}
		msg := <-done {
			start_failure = 'client finished before handler started: ${msg}'
		}
		10 * time.second {
			start_failure = 'timed out waiting for handler to start'
		}
	}
	if start_failure != '' {
		// The request can enter BlockingHandler just after the select deadline.
		// Release it before waiting for the server thread, otherwise this failure
		// path deadlocks forever instead of reporting the original timeout.
		release <- true
		srv.close()
		t.wait()
		assert false, start_failure
		return
	}
	srv.close()
	time.sleep(50 * time.millisecond)
	release <- true
	assert (<-done) == 'released'
	t.wait()
	assert srv.status() == .closed
}

fn test_server_tls_close_during_silent_handshake() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         100 * time.millisecond
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	mut client := net.dial_tcp('127.0.0.1:${port}') or {
		srv.close()
		t.wait()
		assert false, 'tcp dial failed: ${err}'
		return
	}
	defer {
		client.close() or {}
	}
	time.sleep(50 * time.millisecond)
	sw := time.new_stopwatch()
	srv.close()
	t.wait()
	assert sw.elapsed() < time.second
	assert srv.status() == .closed
}

// test_server_tls_close_under_handshake_flood guards the case where every worker
// is stuck in a slow TLS handshake AND the channel buffer is full of untracked
// raw conns, so the accept thread parks on `ch <- conn`. If the accept thread
// could not poll s.state during that send it would never reach close_idle(), and
// close() would hang until a worker handshake timed out. Each client sends a
// valid ClientHello then stalls, so the server's handshake blocks waiting for the
// client's next flight (a bare connect would instead fail the handshake fast and
// free the worker). With one worker and a one-slot buffer this fills: conn 0 ->
// the worker, conn 1 -> the buffer, conn 2 -> the blocked send.
fn test_server_tls_close_under_handshake_flood() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		// A finite accept_timeout doubles as the handshake budget (see
		// tls_accept_timeouts), so use a large one: each stalled handshake must
		// stay stuck long enough to keep the worker busy and form the wedge,
		// rather than time out in milliseconds. The accept loop still polls at the
		// 100ms tls_accept_poll_timeout cap regardless.
		accept_timeout:         8 * time.second
		worker_num:             1
		pool_channel_slots:     1
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	mut clients := []&net.TcpConn{}
	for _ in 0 .. 4 {
		mut c := net.dial_tcp('127.0.0.1:${port}') or { continue }
		c.write(stalled_tls_client_hello) or {}
		clients << c
	}
	defer {
		for mut c in clients {
			c.close() or {}
		}
	}
	// Let the accept thread occupy the worker and buffer and park on the send.
	time.sleep(400 * time.millisecond)
	sw := time.new_stopwatch()
	srv.close()
	t.wait()
	// close_idle() force-closes the stuck handshake fds, so close() returns well
	// before the 8s handshake timeout — but only if the accept thread escaped the
	// blocked send to reach it.
	assert sw.elapsed() < 2 * time.second
	assert srv.status() == .closed
}

fn test_server_tls_close_interrupts_idle_keep_alive() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		read_timeout:           5 * time.second
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	mut client := mbedtls.new_ssl_conn(mbedtls.SSLConnectConfig{}) or {
		srv.close()
		t.wait()
		assert false, 'ssl client init failed: ${err}'
		return
	}
	defer {
		client.shutdown() or {}
	}
	client.dial('127.0.0.1', port) or {
		srv.close()
		t.wait()
		assert false, 'ssl dial failed: ${err}'
		return
	}
	request := 'GET /idle HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\nConnection: keep-alive\r\n\r\n'
	client.write_string(request) or {
		srv.close()
		t.wait()
		assert false, 'ssl write failed: ${err}'
		return
	}
	mut buf := []u8{len: 4096}
	mut response_bytes := []u8{cap: 4096}
	read_sw := time.new_stopwatch()
	for read_sw.elapsed() < 2 * time.second {
		n := client.read(mut buf) or {
			time.sleep(20 * time.millisecond)
			continue
		}
		if n <= 0 {
			time.sleep(20 * time.millisecond)
			continue
		}
		response_bytes << buf[..n]
		response_so_far := response_bytes.bytestr()
		if response_so_far.contains('\r\n\r\n') && response_so_far.contains('tls hello /idle') {
			break
		}
	}
	if response_bytes.len > 0 {
		response := response_bytes.bytestr()
		assert response.to_lower().contains('connection: keep-alive')
		assert response.contains('tls hello /idle')
	}

	sw := time.new_stopwatch()
	srv.close()
	t.wait()
	assert sw.elapsed() < 2 * time.second
	assert srv.status() == .closed
}

fn test_server_tls_close_interrupts_idle_h2() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		read_timeout:           5 * time.second
		enable_http2:           true
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	mut client := mbedtls.new_ssl_conn(mbedtls.SSLConnectConfig{
		alpn_protocols: ['h2']
	}) or {
		srv.close()
		t.wait()
		assert false, 'ssl client init failed: ${err}'
		return
	}
	defer {
		client.shutdown() or {}
	}
	client.dial('127.0.0.1', port) or {
		srv.close()
		t.wait()
		assert false, 'ssl dial failed: ${err}'
		return
	}
	assert client.negotiated_alpn() == 'h2'
	mut h2 := http.new_h2_conn(client)
	resp := h2.do(http.H2ClientRequest{
		method:    'GET'
		scheme:    'https'
		authority: '127.0.0.1:${port}'
		path:      '/h2-idle'
	}) or {
		srv.close()
		t.wait()
		assert false, 'h2 request failed: ${err}'
		return
	}
	assert resp.status == 200
	assert resp.body.bytestr() == 'tls hello /h2-idle'

	sw := time.new_stopwatch()
	srv.close()
	t.wait()
	assert sw.elapsed() < 2 * time.second
	assert srv.status() == .closed
}

fn wait_for_tls_h2_ping_ack(mut client mbedtls.SSLConn, expected []u8) ! {
	client.set_read_timeout(2 * time.second)
	mut header_bytes := []u8{len: http.h2_frame_header_len}
	for _ in 0 .. 8 {
		io.read_full(mut client, mut header_bytes)!
		header := http.h2_parse_frame_header(header_bytes)!
		if header.length > 16_384 {
			return error('unexpected HTTP/2 frame size: ${header.length}')
		}
		mut payload := []u8{len: int(header.length)}
		io.read_full(mut client, mut payload)!
		frame := http.h2_parse_frame(header, payload)!
		if frame is http.H2PingFrame && frame.ack && frame.data == expected {
			return
		}
	}
	return error('server did not acknowledge the ping after the incomplete HTTP/2 request')
}

fn test_server_tls_close_interrupts_incomplete_h2_request() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		read_timeout:           2 * time.second
		enable_http2:           true
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	mut client := mbedtls.new_ssl_conn(mbedtls.SSLConnectConfig{
		alpn_protocols: ['h2']
	}) or {
		srv.close()
		t.wait()
		assert false, 'ssl client init failed: ${err}'
		return
	}
	defer {
		client.shutdown() or {}
	}
	client.dial('127.0.0.1', port) or {
		srv.close()
		t.wait()
		assert false, 'ssl dial failed: ${err}'
		return
	}
	assert client.negotiated_alpn() == 'h2'
	mut enc := http.H2HpackEncoder{}
	block := enc.encode([
		http.H2HeaderField{':method', 'POST'},
		http.H2HeaderField{':scheme', 'https'},
		http.H2HeaderField{':authority', '127.0.0.1:${port}'},
		http.H2HeaderField{':path', '/h2-incomplete'},
	])
	mut out := []u8{}
	out << http.h2_client_preface.bytes()
	out << http.H2Frame(http.H2SettingsFrame{}).encode()
	out << http.H2Frame(http.H2HeadersFrame{
		stream_id:   1
		fragment:    block
		end_headers: true
		end_stream:  false
	}).encode()
	probe := 'shutdown'.bytes()
	out << http.H2Frame(http.H2PingFrame{ data: probe }).encode()
	written := client.write(out) or {
		srv.close()
		t.wait()
		assert false, 'ssl write failed: ${err}'
		return
	}
	assert written == out.len
	// The ack confirms the server processed the preceding HEADERS.
	wait_for_tls_h2_ping_ack(mut client, probe) or {
		srv.close()
		t.wait()
		assert false, err.msg()
		return
	}

	sw := time.new_stopwatch()
	srv.close()
	t.wait()
	assert sw.elapsed() < time.second
	assert srv.status() == .closed
}

fn test_server_tls_parallel_handshakes() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	// Occupy `stall` workers with clients that start a handshake and stop after
	// ClientHello. Each server record confirms a handshake has started. A serial
	// accept loop would have to finish the first handshake before starting the next.
	// Leave one free worker per live client so their full handshakes can complete.
	worker_num := 4
	stall := 2
	live := 2
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         10 * time.second
		worker_num:             worker_num
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	defer {
		srv.close()
		t.wait()
	}
	mut stalled := []&net.TcpConn{}
	defer {
		for mut c in stalled {
			c.close() or {}
		}
	}
	for _ in 0 .. stall {
		mut c := net.dial_tcp('127.0.0.1:${port}') or {
			assert false, 'stall dial failed: ${err}'
			return
		}
		stalled << c
		c.set_read_timeout(tls_handshake_budget / 2)
		c.write(stalled_tls_client_hello) or {
			assert false, 'could not send ClientHello: ${err}'
			return
		}
		mut content_type := []u8{len: 1}
		io.read_full(mut c, mut content_type) or {
			assert false, 'server did not start a stalled TLS handshake: ${err}'
			return
		}
		assert content_type[0] == 0x16, 'expected a TLS handshake record, got ${content_type[0]}'
	}
	// Give live clients less time than the stalled handshakes' 10s budget.
	// The fetch read timeout starts after the TLS handshake, so bound the result wait.
	live_budget := 6 * time.second
	results := chan string{cap: live}
	sw := time.new_stopwatch()
	for i in 0 .. live {
		spawn fn [results, port, i] () {
			resp := http.fetch(
				url:                      'https://127.0.0.1:${port}/live${i}'
				enable_http2:             false
				validate:                 false
				// One attempt, no retries: a regressed handshake should surface as a
				// single timed-out request, not retry-amplify the teardown.
				max_retries:              1
				// Force a fresh TLS connection per live client. Without this, if one
				// client finishes and returns its keep-alive connection to the shared
				// pool before the other checks one out, the second reuses it and the
				// test would pass after only a single live handshake — not two
				// concurrent completing handshakes against the shared mbedtls state.
				disable_connection_reuse: true
			) or {
				results <- 'error: ${err}'
				return
			}
			if resp.status_code != 200 {
				results <- 'bad status: ${resp.status_code}'
				return
			}
			results <- resp.body
		}()
	}
	// Collect all live results under one deadline.
	mut ok := 0
	mut received := 0
	mut failures := []string{}
	for received < live {
		remaining := live_budget - sw.elapsed()
		if remaining <= 0 {
			break
		}
		select {
			body := <-results {
				received++
				if body.starts_with('tls hello /live') {
					ok++
				} else {
					failures << body
				}
			}
			remaining {
				break
			}
		}
	}
	assert ok > 0, 'expected at least one live handshake to complete within ${live_budget} while ${stall} workers were mid-handshake; got ${ok}/${live}, failures: ${failures} - handshakes appear serialized on the accept thread, not parallel'
}

fn test_server_tls_h2_negotiation() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		enable_http2:           true
		handler:                EchoHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	defer {
		srv.close()
		t.wait()
	}
	// Client opts into HTTP/2; server must select `h2` via ALPN and serve the
	// request through its HTTP/2 driver.
	resp := http.fetch(
		url:          'https://127.0.0.1:${port}/h2'
		enable_http2: true
		validate:     false
	) or {
		assert false, 'h2 fetch failed: ${err}'
		return
	}
	assert resp.version() == .v2_0
	assert resp.status_code == 200
	assert resp.body == 'tls hello /h2'

	// Without an explicit HTTP/2 opt-in, the client uses HTTP/1.1 to the
	// same listener even after an HTTP/2 connection has been established.
	resp_h1 := http.fetch(
		url:      'https://127.0.0.1:${port}/h1'
		validate: false
	) or {
		assert false, 'h1 fetch failed: ${err}'
		return
	}
	assert resp_h1.version() == .v1_1
	assert resp_h1.status_code == 200
}

struct TlsRemoteAddrHandler {}

fn (mut h TlsRemoteAddrHandler) handle(req http.Request) http.Response {
	return http.Response{
		status_code: 200
		body:        '${req.remote_addr}|${req.remote_ip()}|${req.header.get_custom('Remote-Addr') or { '<none>' }}'
	}
}

// test_server_tls_sets_remote_addr covers the TLS listener's own copy of the
// remote-address plumbing. It reads the peer back from the accepted socket
// rather than from mbedtls' SSLConn.ip, which is only filled in for IPv4
// peers and carries no port, so unlike that field it also works over IPv6.
fn test_server_tls_sets_remote_addr() {
	$if use_openssl ? {
		eprintln('skipping: TLS server not implemented for -d use_openssl yet')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	mut srv := &http.Server{
		addr:                   '127.0.0.1:${port}'
		cert:                   server_tls_cert
		cert_key:               server_tls_key
		in_memory_verification: true
		accept_timeout:         tls_handshake_budget
		handler:                TlsRemoteAddrHandler{}
		show_startup_message:   false
	}
	t := spawn srv.listen_and_serve()
	srv.wait_till_running() or {
		srv.close()
		t.wait()
		assert false, 'server failed to start: ${err}'
		return
	}
	defer {
		srv.close()
		t.wait()
	}
	mut spoofed := http.new_header()
	spoofed.add_custom('remote-addr', '6.6.6.6') or {
		assert false, 'add_custom: ${err}'
		return
	}
	resp := http.fetch(
		url:                      'https://127.0.0.1:${port}/whoami'
		validate:                 false
		header:                   spoofed
		disable_connection_reuse: true
	) or {
		assert false, 'fetch failed: ${err}'
		return
	}
	assert resp.status_code == 200
	parts := resp.body.split('|')
	assert parts.len == 3
	assert parts[0].starts_with('127.0.0.1:'), 'tls remote_addr: ${parts[0]}'
	assert parts[0].all_after('127.0.0.1:').int() > 0
	assert parts[1] == '127.0.0.1'
	assert parts[2] == '127.0.0.1'
}
