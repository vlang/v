module http

import net
import net.mbedtls
import time

fn serve_response_buffer_growth(mut listener mbedtls.SSLListener, body []u8) bool {
	defer {
		listener.shutdown() or {}
	}
	mut conn := listener.accept_with_timeout(5 * time.second) or { return false }
	defer {
		conn.shutdown() or {}
	}
	conn.set_read_timeout(5 * time.second)
	mut request := []u8{}
	mut buf := []u8{len: 2048}
	for !request.bytestr().contains('\r\n\r\n') {
		n := conn.read(mut buf) or { return false }
		if n <= 0 || request.len + n > 8192 {
			return false
		}
		request << buf[..n]
	}
	header := 'HTTP/1.1 200 OK\r\nContent-Length: ${body.len}\r\nConnection: close\r\n\r\n'
	conn.write_string(header) or { return false }
	mut offset := 0
	for offset < body.len {
		end := if offset + 4096 < body.len { offset + 4096 } else { body.len }
		n := conn.write(body[offset..end]) or { return false }
		if n <= 0 {
			return false
		}
		offset += n
	}
	return true
}

fn check_response_buffer_growth(enable_http2 bool) ! {
	mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := port_listener.addr()!.port()!
	port_listener.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:           @VEXEROOT + '/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
		cert_key:       @VEXEROOT + '/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'
		validate:       false
		alpn_protocols: ['http/1.1']
	})!
	// Exceed SChannel's initial 44000-byte buffer and include all byte values.
	body := []u8{len: 128 * 1024 + 17, init: u8(index % 256)}
	server := spawn serve_response_buffer_growth(mut listener, body)
	defer {
		assert server.wait()
	}
	req := Request{
		validate:      false
		enable_http2:  enable_http2
		read_timeout:  5 * time.second
		write_timeout: 5 * time.second
		max_retries:   1
	}
	// Exercise the one-shot backend and its ALPN-to-HTTP/1.1 fallback directly.
	resp := req.ssl_do(port, .get, '127.0.0.1', '/', '', Header{})!
	assert resp.status_code == 200
	assert resp.body.bytes() == body
}

fn test_response_buffer_growth_http1() ! {
	check_response_buffer_growth(false)!
}

fn test_response_buffer_growth_http1_alpn_fallback() ! {
	check_response_buffer_growth(true)!
}
