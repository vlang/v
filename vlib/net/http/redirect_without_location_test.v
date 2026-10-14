import net
import net.http
import time

fn serve_redirect_response(mut conn net.TcpConn, status http.Status) {
	defer { conn.close() or {} }
	conn.set_read_timeout(5 * time.second)
	mut request := ''
	mut buf := []u8{len: 4096}
	for !request.contains('\r\n\r\n') {
		n := conn.read(mut buf) or { return }
		if n <= 0 { return }
		request += buf[..n].bytestr()
	}
	path := request.split(' ')[1]
	mut body := 'redirect body'
	mut code := int(status)
	mut location := ''
	if path == '/terminal' {
		code = 200
		body = 'terminal body'
	} else if path == '/empty' {
		location = 'Location: \r\n'
	} else if path == '/redirect' {
		location = 'Location: /terminal\r\n'
	}
	response := 'HTTP/1.1 ${code} Status\r\nContent-Length: ${body.len}\r\nConnection: close\r\n${location}\r\n${body}'
	conn.write(response.bytes()) or { panic(err) }
}

fn serve_redirect_fixture(mut listener net.TcpListener, status http.Status) {
	for _ in 0 .. 5 {
		mut conn := listener.accept() or { return }
		serve_redirect_response(mut conn, status)
	}
}

fn reject_unexpected_redirect(_req &http.Request, _n int, _url string) ! {
	return error('redirect callback called')
}

fn check_redirect_without_location_status(status http.Status) ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	listener.set_accept_timeout(5 * time.second)
	port := listener.addr()!.port()!
	t := spawn serve_redirect_fixture(mut listener, status)
	defer {
		listener.close() or {}
		t.wait()
	}
	for path in ['/missing', '/empty'] {
		req := http.Request{
			url:         'http://127.0.0.1:${port}${path}'
			on_redirect: reject_unexpected_redirect
		}
		res := req.do()!
		assert res.status() == status
		assert res.body == 'redirect body'
	}
	req := http.Request{
		url:         'http://127.0.0.1:${port}/redirect'
		on_redirect: reject_unexpected_redirect
	}
	if _ := req.do() {
		assert false, 'a real redirect must invoke the callback'
	} else {
		assert err.msg() == 'redirect callback called'
	}
	res := http.get('http://127.0.0.1:${port}/redirect')!
	assert res.status() == .ok
	assert res.body == 'terminal body'
}

fn test_redirect_without_location() ! {
	for status in [http.Status.moved_permanently, .found, .see_other, .temporary_redirect,
		.permanent_redirect] {
		check_redirect_without_location_status(status)!
	}
}
