// vtest flaky: true
// vtest retry: 3
module main

import io
import net
import net.html
import net.http
import net.mbedtls
import sync
import sync.pool
import time

const concurrent_large_response_requests = 6
const concurrent_large_response_workers = 3
const concurrent_large_response_links = 2_000
const concurrent_large_response_chunk_size = 8_192
const concurrent_large_response_test_cert = $embed_file('../testdata/localhost_test_cert.pem').to_string()
const concurrent_large_response_test_key = $embed_file('../testdata/localhost_test_key.pem').to_string()

struct ConcurrentLargeResponseContext {
	expected_body       string
	expected_link_count int
mut:
	start &sync.Barrier
}

struct ConcurrentLargeResponseResult {
	status_code  int
	body_matches bool
	link_count   int
	err          string
}

fn test_https_large_bodies_can_be_processed_concurrently() {
	// Watchdog thread to prevent the test from hanging indefinitely on CI.
	spawn fn () {
		time.sleep(120 * time.second)
		eprintln('mbedtls concurrent large response test: timeout reached, exiting.')
		exit(1)
	}()

	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!

	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:                   concurrent_large_response_test_cert
		cert_key:               concurrent_large_response_test_key
		validate:               false
		in_memory_verification: true
	})!
	expected_body := concurrent_large_response_body()
	server := spawn serve_large_html_responses(mut listener, expected_body)

	mut urls := []string{cap: concurrent_large_response_requests}
	for i in 0 .. concurrent_large_response_requests {
		urls << 'https://127.0.0.1:${port}/?req=${i}'
	}
	mut pp := pool.new_pool_processor(callback: concurrent_large_response_worker)
	pp.set_max_jobs(concurrent_large_response_workers)
	pp.set_shared_context(&ConcurrentLargeResponseContext{
		expected_body:       expected_body
		expected_link_count: concurrent_large_response_links
		start:               sync.new_barrier(concurrent_large_response_workers)
	})
	pp.work_on_items(urls)
	server.wait()

	for result in pp.get_results_ref[ConcurrentLargeResponseResult]() {
		assert result.err == ''
		assert result.status_code == 200
		assert result.body_matches
		assert result.link_count == concurrent_large_response_links
	}
}

fn test_https_request_timeout_closes_the_connection() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!

	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:                   concurrent_large_response_test_cert
		cert_key:               concurrent_large_response_test_key
		validate:               false
		in_memory_verification: true
	})!
	server := spawn serve_incomplete_https_response(mut listener)

	mut req := http.new_request(.get, 'https://127.0.0.1:${port}', '')
	req.read_timeout = 250 * time.millisecond
	req.validate = false
	req.do() or {
		assert server.wait()
		return
	}
	panic('expected the HTTPS request to time out')
}

fn test_https_chunked_body_is_returned_by_http_get_text() {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!

	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:                   concurrent_large_response_test_cert
		cert_key:               concurrent_large_response_test_key
		validate:               false
		in_memory_verification: true
	})!
	expected_body := concurrent_large_response_body()
	server := spawn serve_chunked_html_response(mut listener, expected_body)

	resp := http.fetch(
		url:      'https://127.0.0.1:${port}/chunked'
		validate: false
	) or {
		server.wait()
		assert false, 'fetch failed: ${err}'
		return
	}
	server.wait()

	assert resp.body == expected_body
}

fn concurrent_large_response_body() string {
	return '<html><body>' +
		'<article><a href="/item">payload</a></article>'.repeat(concurrent_large_response_links) +
		'</body></html>'
}

// Every TLS backend, including native Windows SChannel, reuses the ALPN
// probe connection for an h1-only response. Single-request test servers
// therefore need exactly one connection.
const clr_test_max_accepts = 1

fn serve_incomplete_https_response(mut listener mbedtls.SSLListener) bool {
	defer {
		listener.shutdown() or {}
	}
	for _ in 0 .. clr_test_max_accepts {
		mut conn := listener.accept() or { panic(err) }
		mut request_buf := []u8{len: 2048}
		n := conn.read(mut request_buf) or {
			conn.shutdown() or {}
			continue
		}
		if n <= 0 {
			conn.shutdown() or {}
			continue
		}
		defer {
			conn.shutdown() or {}
		}
		conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: 2\r\n') or { return false }
		conn.set_read_timeout(time.second)
		mut drain_buf := []u8{len: 128}
		// TLS close_notify returns zero; a socket EOF returns io.Eof.
		// Other errors, including mbedTLS timeouts, do not prove peer closure.
		read := conn.read(mut drain_buf) or { return err is io.Eof }
		return read == 0
	}
	return false
}

fn serve_large_html_responses(mut listener mbedtls.SSLListener, body string) {
	defer {
		listener.shutdown() or {}
	}
	// Each request uses one connection, including the reused ALPN probe.
	mut handlers := []thread{cap: concurrent_large_response_requests}
	for _ in 0 .. concurrent_large_response_requests {
		mut conn := listener.accept() or { panic(err) }
		handlers << spawn write_large_html_response(mut conn, body)
	}
	handlers.wait()
}

fn serve_chunked_html_response(mut listener mbedtls.SSLListener, body string) {
	defer {
		listener.shutdown() or {}
	}
	for _ in 0 .. clr_test_max_accepts {
		mut conn := listener.accept() or { panic(err) }
		if !write_chunked_html_response(mut conn, body) {
			// The peer closed without sending a request.
			continue
		}
		return
	}
}

fn write_large_html_response(mut conn mbedtls.SSLConn, body string) {
	defer {
		conn.shutdown() or {}
	}
	mut request_buf := []u8{len: 2048}
	n := conn.read(mut request_buf) or { return }
	if n <= 0 {
		// The peer closed without sending a request: nothing to respond to.
		return
	}
	header := 'HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: ${body.len}\r\nConnection: close\r\n\r\n'
	conn.write_string(header) or { return }
	mut start := 0
	for start < body.len {
		end := if start + concurrent_large_response_chunk_size > body.len {
			body.len
		} else {
			start + concurrent_large_response_chunk_size
		}
		conn.write_string(body[start..end]) or { return }
		start = end
	}
}

// write_chunked_html_response serves one scripted chunked response. Returns
// whether a request actually arrived on this connection. A clean close_notify
// EOF or read error before the request returns false.
fn write_chunked_html_response(mut conn mbedtls.SSLConn, body string) bool {
	defer {
		conn.shutdown() or {}
	}
	mut request_buf := []u8{len: 2048}
	n := conn.read(mut request_buf) or { return false }
	if n <= 0 {
		return false
	}
	header := 'HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n'
	conn.write_string(header) or { return true }
	mut start := 0
	for start < body.len {
		end := if start + concurrent_large_response_chunk_size > body.len {
			body.len
		} else {
			start + concurrent_large_response_chunk_size
		}
		chunk := body[start..end]
		conn.write_string('${chunk.len:x}\r\n${chunk}\r\n') or { return true }
		start = end
	}
	conn.write_string('0\r\n\r\n') or { return true }
	return true
}

fn concurrent_large_response_worker(mut pp pool.PoolProcessor, idx int, _wid int) &ConcurrentLargeResponseResult {
	url := pp.get_item[string](idx)
	mut ctx := unsafe { &ConcurrentLargeResponseContext(pp.get_shared_context()) }
	if idx < concurrent_large_response_workers {
		// Let every worker start its first request together.
		ctx.start.wait()
	}
	resp := http.fetch(
		method:   .get
		url:      url
		validate: false
	) or {
		return &ConcurrentLargeResponseResult{
			err: '${err}'
		}
	}
	doc := html.parse(resp.body)
	return &ConcurrentLargeResponseResult{
		status_code:  resp.status_code
		body_matches: resp.body == ctx.expected_body
		link_count:   doc.get_tags(name: 'a').len
	}
}
