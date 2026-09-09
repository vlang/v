import net
import net.http
import time

fn test_https_get() {
	$if !network ? {
		return
	}
	assert http.get_text('https://vlang.io/version') == '0.1.5'
	println('https ok')
}

fn test_http_get_from_vlang_utc_now() {
	$if !network ? {
		return
	}
	url := 'http://vlang.io/utc_now'
	println('Test getting current time from HTTP ${url} by http.get')
	res := http.get(url) or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.int() > 1566403696
	println('Current time is: ${res.body.int()}')
}

fn test_https_get_from_vlang_utc_now() {
	$if !network ? {
		return
	}
	url := 'https://vlang.io/utc_now'
	println('Test getting current time from HTTPS ${url} by http.get')
	res := http.get(url) or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.int() > 1566403696
	println('Current time is: ${res.body.int()}')
}

fn test_http_public_servers() {
	$if !network ? {
		return
	}
	urls := [
		'http://github.com/robots.txt',
		'http://google.com/robots.txt',
		// 'http://yahoo.com/robots.txt',
	]
	for url in urls {
		println('Testing http.get on public HTTP url: ${url} ')
		res := http.get(url) or { panic(err) }
		assert res.status() == .ok
		assert res.body != ''
	}
}

fn test_https_public_servers() {
	$if !network ? {
		return
	}
	urls := [
		'https://github.com/robots.txt',
		'https://google.com/robots.txt',
		// 'https://yahoo.com/robots.txt',
	]
	for url in urls {
		println('Testing http.get on public HTTPS url: ${url} ')
		res := http.get(url) or { panic(err) }
		assert res.status() == .ok
		assert res.body != ''
	}
}

struct RelativeRedirectHandler {}

fn (mut handler RelativeRedirectHandler) handle(req http.Request) http.Response {
	mut res := http.Response{}
	match req.url {
		'/relative-redirect/3?abc=xyz' {
			res.header = http.new_header(key: .location, value: '/relative-redirect/2?abc=xyz')
			res.set_status(.found)
		}
		'/relative-redirect/2?abc=xyz' {
			res.header = http.new_header(key: .location, value: '/relative-redirect/1?abc=xyz')
			res.set_status(.found)
		}
		'/relative-redirect/1?abc=xyz' {
			res.header = http.new_header(key: .location, value: '/get?abc=xyz')
			res.set_status(.found)
		}
		'/get?abc=xyz' {
			res.body = '{"args": {"abc": "xyz"}}'
			res.set_status(.ok)
		}
		else {
			res.body = req.url
			res.set_status(.not_found)
		}
	}

	res.set_version(req.version)
	return res
}

fn test_relative_redirects() {
	$if !network ? {
		return
	}
	mut server := &http.Server{
		accept_timeout:       100 * time.millisecond
		handler:              RelativeRedirectHandler{}
		addr:                 '127.0.0.1:0'
		show_startup_message: false
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		server.stop()
		t.wait()
		panic(err)
	}
	defer {
		server.stop()
		t.wait()
	}
	res := http.get('http://${server.addr}/relative-redirect/3?abc=xyz') or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.contains('"abc": "xyz"')
}

fn user_agent_response_body(request string) string {
	for line in request.split('\r\n') {
		if line.to_lower().starts_with('user-agent:') {
			user_agent := line.all_after(':').trim_space()
			return '{"user-agent": "${user_agent}"}'
		}
	}
	return '{"user-agent": ""}'
}

fn serve_user_agent_once(mut listener net.TcpListener) {
	mut conn := listener.accept() or {
		listener.close() or {}
		return
	}
	defer {
		conn.close() or {}
		listener.close() or {}
	}
	mut request := ''
	mut buf := []u8{len: 4096}
	for {
		n := conn.read(mut buf) or { return }
		if n <= 0 {
			return
		}
		request += buf[..n].bytestr()
		if request.contains('\r\n\r\n') {
			break
		}
	}
	body := user_agent_response_body(request)
	response := 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: ${body.len}\r\nConnection: close\r\n\r\n${body}'
	conn.write(response.bytes()) or {}
}

fn start_user_agent_server() !(int, thread) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	return port, spawn serve_user_agent_once(mut listener)
}

fn test_default_user_agent() {
	$if !network ? {
		return
	}
	port, server := start_user_agent_server()!
	res := http.get('http://127.0.0.1:${port}/user-agent') or {
		server.wait()
		panic(err)
	}
	server.wait()
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.contains('"user-agent": "v.http"')
}

fn test_custom_user_agent() {
	$if !network ? {
		return
	}
	ua := 'V http test for UA'
	port, server := start_user_agent_server()!
	mut req := http.new_request(.get, 'http://127.0.0.1:${port}/user-agent', '')
	req.user_agent = ua
	res := req.do() or {
		server.wait()
		panic(err)
	}
	server.wait()
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.contains('"user-agent": "${ua}"')
}
