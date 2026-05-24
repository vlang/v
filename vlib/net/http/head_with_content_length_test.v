module http

import net

// These tests cover RFC 7230 §3.3.3 / RFC 9112 §6.2: HEAD responses, and
// 1xx/204/304 status responses, must not carry a body. Before the fix, the
// receive loop in `receive_all_data_from_cb_in_builder` waited for
// `Content-Length` body bytes that the server never sends, then surfaced
// either `response body ended early` (when the connection closed) or a
// read timeout (when the server kept the socket open).

const head_resp = 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 1234\r\nConnection: close\r\n\r\n'

const status_204_resp = 'HTTP/1.1 204 No Content\r\nContent-Length: 5\r\nConnection: close\r\n\r\n'

const status_304_resp = 'HTTP/1.1 304 Not Modified\r\nContent-Length: 100\r\nConnection: close\r\n\r\n'

fn serve_once(mut listener net.TcpListener, response string) {
	mut conn := listener.accept() or { return }
	defer {
		conn.close() or {}
	}
	mut buf := []u8{len: 4096}
	for {
		n := conn.read(mut buf) or { return }
		if n <= 0 {
			return
		}
		if buf[..n].bytestr().contains('\r\n\r\n') {
			break
		}
	}
	conn.write(response.bytes()) or {}
}

fn start_server(response string) !int {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	addr := listener.addr()!
	port := addr.port()!
	spawn fn [mut listener, response] () {
		serve_once(mut listener, response)
	}()
	return port
}

fn test_head_does_not_wait_for_body_when_content_length_is_set() {
	port := start_server(head_resp) or {
		assert false, 'failed to start server: ${err}'
		return
	}
	resp := head('http://127.0.0.1:${port}/') or {
		assert false, 'HEAD should not error, got: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == ''
	cl := resp.header.get(.content_length) or { '' }
	assert cl == '1234', 'Content-Length header should still be exposed to the caller'
}

fn test_get_204_no_content_does_not_wait_for_body() {
	port := start_server(status_204_resp) or {
		assert false, 'failed to start server: ${err}'
		return
	}
	resp := get('http://127.0.0.1:${port}/') or {
		assert false, 'GET 204 should not error, got: ${err}'
		return
	}
	assert resp.status_code == 204
	assert resp.body == ''
}

fn test_get_304_not_modified_does_not_wait_for_body() {
	port := start_server(status_304_resp) or {
		assert false, 'failed to start server: ${err}'
		return
	}
	resp := get('http://127.0.0.1:${port}/') or {
		assert false, 'GET 304 should not error, got: ${err}'
		return
	}
	assert resp.status_code == 304
	assert resp.body == ''
}
