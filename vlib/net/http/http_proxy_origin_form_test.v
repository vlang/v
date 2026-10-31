// vtest vflags: -d use_openssl
// vtest build: !windows
module http

import net
import time

fn origin_form_proxy_once(mut listener net.TcpListener) []string {
	defer {
		listener.close() or {}
	}
	mut client := listener.accept() or { panic(err) }
	client.set_read_timeout(5 * time.second)
	client.set_write_timeout(5 * time.second)
	defer {
		client.close() or {}
	}
	connect := read_proxy_connect_response(mut client) or { panic(err) }
	client.write_string('HTTP/1.1 200 Connection Established\r\n\r\n') or { panic(err) }
	request := read_proxy_connect_response(mut client) or { panic(err) }
	client.write_string('HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok') or {
		panic(err)
	}
	return [connect, request]
}

fn test_plain_http_proxy_tunnel_uses_origin_form() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	listener.set_accept_timeout(5 * time.second)
	proxy_port := listener.addr()!.port()!
	server := spawn origin_form_proxy_once(mut listener)
	proxy := new_http_proxy('http://127.0.0.1:${proxy_port}')!
	response := fetch(
		url:           'http://origin.invalid:8080/a%20b?query=x%2Fy'
		proxy:         proxy
		read_timeout:  5 * time.second
		write_timeout: 5 * time.second
	)!
	requests := server.wait()
	assert response.status_code == 200
	assert response.body == 'ok'
	assert requests[0].starts_with('CONNECT origin.invalid:8080 HTTP/1.1\r\n'), requests[0]
	assert requests[1].starts_with('GET /a%20b?query=x%2Fy HTTP/1.1\r\n'), requests[1]
	assert requests[1].contains('\r\nHost: origin.invalid:8080\r\n'), requests[1]
}
