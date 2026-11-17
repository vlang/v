module httptest

import net
import net.http
import time

struct HelloHandler {}

fn (_ HelloHandler) handle(_ http.Request) http.Response {
	return http.Response{
		status_code:  200
		status_msg:   'OK'
		body:         'hello test'
		http_version: '1.1'
	}
}

fn test_server_roundtrip() {
	mut ts := new_server(HelloHandler{})!
	defer {
		ts.close()
	}
	assert ts.url.starts_with('http://127.0.0.1:')
	resp := http.get('${ts.url}/hi')!
	assert resp.status_code == 200
	assert resp.body == 'hello test'
}

fn test_server_with_config_roundtrip() {
	mut ts := new_server_with_config(handler: HelloHandler{}, worker_num: 4)!
	defer {
		ts.close()
	}
	assert ts.url.starts_with('http://127.0.0.1:')
	resp := http.get('${ts.url}/hi')!
	assert resp.status_code == 200
	assert resp.body == 'hello test'
}

// test_server_handles_concurrent_connections is a regression test for a
// single-worker starvation: an idle keep-alive connection occupies its
// worker's read loop, so a second client needs another worker to be
// served promptly.
fn test_server_handles_concurrent_connections() {
	mut ts := new_server(HelloHandler{})!
	defer {
		ts.close()
	}
	addr := ts.server.addr
	mut idle := net.dial_tcp(addr)!
	defer {
		idle.close() or {}
	}
	idle.set_read_timeout(5 * time.second)
	idle.set_write_timeout(5 * time.second)
	idle.write_string('GET /hi HTTP/1.1\r\nHost: ${addr}\r\nConnection: keep-alive\r\n\r\n')!
	resp1 := read_raw_response(mut idle)!
	assert resp1.contains('200 OK')
	assert resp1.contains('hello test')
	// The idle connection stays open here on purpose. The second client
	// uses a fresh connection and must still be served promptly.
	resp := http.fetch(url: '${ts.url}/hi', read_timeout: 5 * time.second)!
	assert resp.status_code == 200
	assert resp.body == 'hello test'
}

fn read_raw_response(mut conn net.TcpConn) !string {
	mut buf := []u8{len: 4096}
	mut out := []u8{}
	for {
		n := conn.read(mut buf)!
		if n <= 0 {
			break
		}
		out << buf[..n]
		s := out.bytestr()
		if idx := s.index('\r\n\r\n') {
			mut content_length := -1
			for line in s[..idx].split('\r\n') {
				if line.to_lower().starts_with('content-length:') {
					content_length = line.all_after(':').trim_space().int()
					break
				}
			}
			if content_length >= 0 && out.len >= idx + 4 + content_length {
				break
			}
			if content_length == -1 {
				break
			}
		}
		if out.len > 64 * 1024 {
			break
		}
	}
	return out.bytestr()
}
