// vtest build: !sanitize-memory-clang
// Hermetic TLS-termination test for net.http.Server: spin up a local HTTPS
// server with an in-memory cert/key, hit it with http.fetch (validate: false),
// and assert the round-trip.

module main

import net
import net.http
import net.mbedtls
import time

const server_tls_cert = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

const server_tls_key = '-----BEGIN RSA PRIVATE KEY-----\n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\n-----END RSA PRIVATE KEY-----\n'

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
		accept_timeout:         time.second
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
	// Give the listener a beat to come up.
	time.sleep(50 * time.millisecond)

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
		accept_timeout:         time.second
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
		_ := <-started {}
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
	// A real 165-byte TLS 1.2 ClientHello captured from V's own mbedtls client.
	// The exact bytes (random/session id) are irrelevant; the server only needs a
	// structurally valid first flight to start the handshake and then block
	// waiting for the client's ClientKeyExchange, which never arrives.
	client_hello := [u8(0x16), 0x03, 0x03, 0x00, 0xa0, 0x01, 0x00, 0x00, 0x9c, 0x03, 0x03, 0x6a,
		0x3d, 0x34, 0x4c, 0x8f, 0x46, 0x64, 0xd8, 0xe9, 0x2d, 0x46, 0x16, 0xd3, 0xf2, 0x87, 0xe4,
		0xee, 0xc5, 0x57, 0x6c, 0x8e, 0xd7, 0x36, 0x19, 0x3b, 0x35, 0x7a, 0x01, 0x03, 0xde, 0x6e,
		0x64, 0x00, 0x00, 0x24, 0xc0, 0x2c, 0xc0, 0x2b, 0xc0, 0x30, 0xc0, 0x2f, 0xc0, 0x24, 0xc0,
		0x23, 0xc0, 0x28, 0xc0, 0x27, 0xc0, 0x0a, 0xc0, 0x09, 0xc0, 0x14, 0xc0, 0x13, 0x00, 0x9d,
		0x00, 0x9c, 0x00, 0x3d, 0x00, 0x3c, 0x00, 0x35, 0x00, 0x2f, 0x01, 0x00, 0x00, 0x4f, 0x00,
		0x0a, 0x00, 0x08, 0x00, 0x06, 0x00, 0x1d, 0x00, 0x17, 0x00, 0x18, 0x00, 0x0b, 0x00, 0x02,
		0x01, 0x00, 0x00, 0x0d, 0x00, 0x1a, 0x00, 0x18, 0x08, 0x04, 0x08, 0x05, 0x08, 0x06, 0x04,
		0x01, 0x05, 0x01, 0x02, 0x01, 0x04, 0x03, 0x05, 0x03, 0x02, 0x03, 0x02, 0x02, 0x06, 0x01,
		0x06, 0x03, 0x00, 0x23, 0x00, 0x00, 0x00, 0x10, 0x00, 0x0e, 0x00, 0x0c, 0x02, 0x68, 0x32,
		0x08, 0x68, 0x74, 0x74, 0x70, 0x2f, 0x31, 0x2e, 0x31, 0x00, 0x17, 0x00, 0x00, 0xff, 0x01,
		0x00, 0x01, 0x00]
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
		accept_timeout:       8 * time.second
		worker_num:           1
		pool_channel_slots:   1
		handler:              EchoHandler{}
		show_startup_message: false
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
		c.write(client_hello) or {}
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
		accept_timeout:         time.second
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
		accept_timeout:         time.second
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
		accept_timeout:         time.second
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
	written := client.write(out) or {
		srv.close()
		t.wait()
		assert false, 'ssl write failed: ${err}'
		return
	}
	assert written == out.len
	time.sleep(100 * time.millisecond)

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
	$if macos {
		eprintln('skipping: macOS mbedTLS rejects the silent TCP probes')
		return
	}
	port := pick_port() or {
		assert false, 'pick_port: ${err}'
		return
	}
	// Prove handshakes run in parallel on the worker threads, not serially on the
	// accept thread. The discriminator is placed at the HANDSHAKE layer, not at the
	// request handler: a barrier inside the handler only proves two *handlers*
	// overlapped, which a serial-accept-then-queue design also produces (the accept
	// thread can complete two handshakes one after another and hand both completed
	// connections to workers that then park together). Counting overlapping handlers
	// would therefore miss exactly the regression we care about.
	//
	// Instead, occupy `stall` workers with raw TCP connections that complete the TCP
	// connect but never send a ClientHello, so each one wedges a worker *inside*
	// complete_handshake (blocked reading the handshake). Then fire `live` real HTTPS
	// clients. If handshakes run per-worker in parallel, the remaining free workers
	// service the live clients concurrently and they succeed. If the implementation
	// regresses to handshaking serially on the accept thread, the very first stalled
	// connection wedges that thread mid-handshake and no later connection — stalled or
	// live — is ever accepted/serviced, so every live fetch fails. worker_num must be
	// >= stall + live so a free worker exists for each live client.
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
	// Wedge `stall` workers mid-handshake with silent TCP connections.
	mut stalled := []&net.TcpConn{}
	for _ in 0 .. stall {
		c := net.dial_tcp('127.0.0.1:${port}') or {
			assert false, 'stall dial failed: ${err}'
			return
		}
		stalled << c
	}
	// Give the accept loop time to hand each stalled connection to a worker (and, in
	// a regressed serial-accept design, to become wedged on the first one) before the
	// live clients connect.
	time.sleep(500 * time.millisecond)
	// Fire live clients while the stalled sockets occupy workers. With parallel
	// per-worker handshakes, at least one free worker services a live client in
	// well under a second. A transient TLS client-side handshake error is not, by
	// itself, proof that the accept thread regressed to serial handshaking; the
	// discriminator is that no live request can complete while the accept thread is
	// wedged inside a stalled handshake.
	//
	// The discriminator is time-bounded on purpose: a serial-accept regression does
	// eventually service the live clients — but only after the stalled handshakes hit
	// their budget (handshake_timeout == accept_timeout == 10s here, x2 stalled), so
	// without a bound the test would merely run slower, not fail. The bound is on the
	// WAIT itself (a select deadline below), not the client: http.fetch cannot bound
	// this for us because Request.ssl_do dials and completes the TLS handshake before
	// req.read_timeout applies (and retries on socket errors), so a read_timeout would
	// not cap a regressed handshake. The select makes the regression fail promptly at
	// ~6s; a real parallel handshake completes in well under a second.
	live_budget := 6 * time.second
	results := chan string{cap: live}
	sw := time.new_stopwatch()
	for i in 0 .. live {
		spawn fn [results, port, i] () {
			resp := http.fetch(
				url:          'https://127.0.0.1:${port}/live${i}'
				enable_http2: false
				validate:     false
				// One attempt, no retries: a regressed handshake should surface as a
				// single timed-out request, not retry-amplify the teardown.
				max_retries: 1
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
	// Collect live results within a single `live_budget`, bounding the wait so a
	// serialized-handshake regression fails at ~6s instead of hanging on the
	// client's own handshake timeout.
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
	// Release the stalled sockets; srv.close() in the defer also force-closes any the
	// worker is still blocked on via the idle tracker.
	for mut c in stalled {
		c.close() or {}
	}
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
		accept_timeout:         time.second
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
	time.sleep(50 * time.millisecond)

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

	// With HTTP/2 disabled on the client, the server must keep speaking
	// HTTP/1.1 to the same listener. (enable_http2 defaults to true since
	// vlang/v#27384, so it must be opted out of explicitly here.)
	resp_h1 := http.fetch(
		url:          'https://127.0.0.1:${port}/h1'
		enable_http2: false
		validate:     false
	) or {
		assert false, 'h1 fetch failed: ${err}'
		return
	}
	assert resp_h1.version() == .v1_1
	assert resp_h1.status_code == 200
}
