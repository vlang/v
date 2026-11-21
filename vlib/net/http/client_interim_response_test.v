import net
import net.http
import time

// A server may send interim (1xx) responses, such as `100 Continue` or
// `103 Early Hints`, before the final response to a request (RFC 9110 §15.2).
// The HTTP/1.1 client skips them and returns the final response. `101
// Switching Protocols` ends HTTP/1.1 on the connection, so it is final.

const final_response = 'HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello'

// serve_once answers one request with `parts`, written one by one with a pause
// between them, so that a response can arrive split across reads.
fn serve_once(mut listener net.TcpListener, parts []string) {
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
	for i, part in parts {
		if i > 0 {
			time.sleep(50 * time.millisecond)
		}
		conn.write_string(part) or { return }
	}
}

fn get_from(parts []string) !http.Response {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	spawn serve_once(mut listener, parts)
	return http.get('http://127.0.0.1:${port}/')
}

fn test_early_hints_are_skipped_for_the_final_response() {
	resp := get_from(['HTTP/1.1 103 Early Hints\r\nLink: </style.css>; rel=preload\r\n\r\n' +
		final_response])!
	assert resp.status_code == 200
	assert resp.body == 'hello'
	assert resp.header.get_custom('Link') or { '' } == '', 'the interim headers leaked into the final response'
}

fn test_continue_is_skipped_for_the_final_response() {
	resp := get_from(['HTTP/1.1 100 Continue\r\n\r\n' + final_response])!
	assert resp.status_code == 200
	assert resp.body == 'hello'
}

fn test_several_interim_responses_are_skipped() {
	resp := get_from(['HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 102 Processing\r\n\r\n',
		'HTTP/1.1 103 Early Hints\r\nLink: </a.js>; rel=preload\r\n\r\n' + final_response])!
	assert resp.status_code == 200
	assert resp.body == 'hello'
}

fn test_an_interim_response_split_across_reads_is_skipped() {
	// The interim head ends mid-read, and the final status line is split.
	resp := get_from(['HTTP/1.1 103 Early', ' Hints\r\n\r\nHTT', final_response[3..]])!
	assert resp.status_code == 200
	assert resp.body == 'hello'
}

fn test_switching_protocols_is_the_final_response() {
	resp := get_from(['HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\r\n'])!
	assert resp.status_code == 101
	assert resp.body == ''
}
