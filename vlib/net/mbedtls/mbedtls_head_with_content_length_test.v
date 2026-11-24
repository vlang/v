module main

import net
import net.http
import net.mbedtls

// HTTPS counterpart to vlib/net/http/head_with_content_length_test.v.
// This is the exact path that emitted `received 0 of N bytes` against
// real-world servers (mbedtls `PEER_CLOSE_NOTIFY` → `socket_read_into_ptr`
// returns 0 → `len <= 0` branch in receive_all_data_from_cb_in_builder).

const head_cl_test_cert = $embed_file('../testdata/localhost_test_cert.pem').to_string()

const head_cl_test_key = $embed_file('../testdata/localhost_test_key.pem').to_string()

// head_cl_test_max_accepts bounds how many connections the test server
// accepts, one at a time, before returning (and shutting the listener down
// via defer) -- identical to a plain single accept() when the max is 1,
// which is what -d no_vschannel and every non-Windows platform need.
//
// Only native Windows (without -d no_vschannel) ever needs 2: an h2-enabled
// https request (fetch's default) to an origin that turns out not to speak
// h2 makes TWO separate connections there -- net.http's ALPN probe (closes
// immediately once it sees h2 wasn't selected, without sending a request)
// and the real request from the one-shot SChannel path's own independent
// dial. Every other configuration reuses the probe connection directly,
// needing only one. With a single accept, the probe consumed it, the
// deferred shutdown ran, and the real request got ECONNREFUSED.
const head_cl_test_max_accepts = $if windows && !no_vschannel ? { 2 } $else { 1 }

fn serve_https_head_response(mut listener mbedtls.SSLListener, response string) {
	defer {
		listener.shutdown() or {}
	}
	for _ in 0 .. head_cl_test_max_accepts {
		mut conn := listener.accept() or { return }
		serve_one_https_head_conn(mut conn, response)
	}
}

fn serve_one_https_head_conn(mut conn mbedtls.SSLConn, response string) {
	defer {
		conn.shutdown() or {}
	}
	mut request_buf := []u8{len: 2048}
	n := conn.read(mut request_buf) or { return }
	if n <= 0 {
		// 0 bytes = the peer closed without sending a request (the ALPN
		// probe's clean close_notify EOF) -- nothing to respond to.
		return
	}
	conn.write_string(response) or { return }
}

fn start_https_server(response string) !int {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:                   head_cl_test_cert
		cert_key:               head_cl_test_key
		validate:               false
		in_memory_verification: true
	})!
	spawn serve_https_head_response(mut listener, response)
	return port
}

fn test_https_head_does_not_wait_for_body_when_content_length_is_set() {
	port := start_https_server('HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 1234\r\nConnection: close\r\n\r\n') or {
		assert false, 'failed to start TLS server: ${err}'
		return
	}
	resp := http.fetch(
		method:   .head
		url:      'https://127.0.0.1:${port}/'
		validate: false
	) or {
		assert false, 'HTTPS HEAD should not error, got: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == ''
	cl := resp.header.get(.content_length) or { '' }
	assert cl == '1234'
}

fn test_https_get_204_with_content_length_returns_empty_body() {
	port := start_https_server('HTTP/1.1 204 No Content\r\nContent-Length: 5\r\nConnection: close\r\n\r\n') or {
		assert false, 'failed to start TLS server: ${err}'
		return
	}
	resp := http.fetch(
		method:   .get
		url:      'https://127.0.0.1:${port}/'
		validate: false
	) or {
		assert false, 'HTTPS GET 204 should not error, got: ${err}'
		return
	}
	assert resp.status_code == 204
	assert resp.body == ''
}
