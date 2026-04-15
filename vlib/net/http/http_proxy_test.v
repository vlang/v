module http

import encoding.base64
import net
import net.mbedtls
import net.urllib
import os
import time

const sample_proxy_url = 'https://localhost'
const sample_auth_proxy_url = 'http://user:pass@localhost:8888'

const sample_host = '127.0.0.1:1337'
const sample_request = &Request{
	url: 'http://${sample_host}'
}
const sample_path = '/'
const proxy_https_request_count = 12
const proxy_https_test_cert_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt'
const proxy_https_test_key_path = @VEXEROOT +
	'/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.key'

fn test_proxy_fields() ? {
	sample_proxy := new_http_proxy(sample_proxy_url)!
	sample_auth_proxy := new_http_proxy(sample_auth_proxy_url)!

	assert sample_proxy.scheme == 'https'
	assert sample_proxy.host == 'localhost:443'
	assert sample_proxy.hostname == 'localhost'
	assert sample_proxy.port == 443
	assert sample_proxy.url == sample_proxy_url
	assert sample_auth_proxy.scheme == 'http'
	assert sample_auth_proxy.username == 'user'
	assert sample_auth_proxy.password == 'pass'
	assert sample_auth_proxy.host == 'localhost:8888'
	assert sample_auth_proxy.hostname == 'localhost'
	assert sample_auth_proxy.port == 8888
	assert sample_auth_proxy.url == sample_auth_proxy_url
}

fn test_proxy_headers() ? {
	sample_proxy := new_http_proxy(sample_proxy_url)!
	headers := sample_proxy.build_proxy_headers(sample_host)

	assert headers == 'CONNECT 127.0.0.1:1337 HTTP/1.1\r\n' + 'Host: 127.0.0.1\r\n' +
		'Proxy-Connection: Keep-Alive\r\n\r\n'
}

fn test_proxy_headers_authenticated() ? {
	sample_proxy := new_http_proxy(sample_auth_proxy_url)!
	headers := sample_proxy.build_proxy_headers(sample_host)

	auth_token := base64.encode(('${sample_proxy.username}:' + '${sample_proxy.password}').bytes())

	assert headers == 'CONNECT 127.0.0.1:1337 HTTP/1.1\r\n' + 'Host: 127.0.0.1\r\n' +
		'Proxy-Connection: Keep-Alive\r\nProxy-Authorization: Basic ${auth_token}\r\n\r\n'
}

enum ProxyTunnelCopyResult {
	data
	timeout
	closed
}

fn count_open_file_descriptors() int {
	$if windows {
		return 0
	} $else {
		fds := os.ls('/dev/fd') or { return 0 }
		return fds.len
	}
}

fn start_https_proxy_test_target_server() !(int, chan bool) {
	ready := chan int{cap: 1}
	done := chan bool{cap: 1}
	spawn fn [ready, done] () {
		mut port_listener := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
		port := int((port_listener.addr() or { panic(err) }).port() or { panic(err) })
		port_listener.close() or {}
		mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
			cert:                   proxy_https_test_cert_path
			cert_key:               proxy_https_test_key_path
			validate:               false
			in_memory_verification: false
		}) or { panic(err) }
		ready <- port
		defer {
			listener.shutdown() or {}
			done <- true
		}
		for _ in 0 .. proxy_https_request_count {
			mut conn := listener.accept() or { panic(err) }
			handle_https_proxy_test_target_connection(mut conn)
		}
	}()
	return <-ready, done
}

fn handle_https_proxy_test_target_connection(mut conn mbedtls.SSLConn) {
	defer {
		conn.shutdown() or {}
	}
	mut request_buf := []u8{len: 2048}
	_ = conn.read(mut request_buf) or { return }
	conn.write_string('HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok') or {
		return
	}
}

fn start_https_proxy_test_server(target_port int) !(int, chan bool) {
	ready := chan int{cap: 1}
	done := chan bool{cap: 1}
	spawn fn [ready, done, target_port] () {
		mut listener := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
		port := int((listener.addr() or { panic(err) }).port() or { panic(err) })
		ready <- port
		mut workers := []thread{cap: proxy_https_request_count}
		for _ in 0 .. proxy_https_request_count {
			mut client := listener.accept() or { panic(err) }
			workers << spawn handle_https_proxy_test_tunnel(mut client, target_port)
		}
		listener.close() or {}
		workers.wait()
		done <- true
	}()
	return <-ready, done
}

fn handle_https_proxy_test_tunnel(mut client net.TcpConn, target_port int) {
	defer {
		client.close() or {}
	}
	request := read_proxy_request(mut client) or { return }
	if !request.starts_with('CONNECT 127.0.0.1:${target_port} HTTP/1.1\r\n') {
		client.write_string('HTTP/1.1 400 Bad Request\r\n\r\n') or {}
		return
	}
	mut upstream := net.dial_tcp('127.0.0.1:${target_port}') or {
		client.write_string('HTTP/1.1 502 Bad Gateway\r\n\r\n') or {}
		return
	}
	defer {
		upstream.close() or {}
	}
	client.write_string('HTTP/1.1 200 Connection Established\r\n\r\n') or { return }
	client.set_read_timeout(50 * time.millisecond)
	upstream.set_read_timeout(50 * time.millisecond)
	deadline := time.now().add(2 * time.second)
	for time.now() < deadline {
		client_state := copy_https_proxy_test_tunnel_data(mut client, mut upstream)
		upstream_state := copy_https_proxy_test_tunnel_data(mut upstream, mut client)
		if client_state == .closed || upstream_state == .closed {
			return
		}
	}
}

fn read_proxy_request(mut conn net.TcpConn) !string {
	mut total_bytes_read := 0
	mut msg := [4096]u8{}
	mut buffer := [1]u8{}
	for total_bytes_read < msg.len {
		bytes_read := conn.read_ptr(&buffer[0], 1)!
		if bytes_read == 0 {
			return error('unexpected EOF while reading proxy request')
		}
		msg[total_bytes_read] = buffer[0]
		total_bytes_read++
		if total_bytes_read > 3 && msg[total_bytes_read - 1] == `\n`
			&& msg[total_bytes_read - 2] == `\r` && msg[total_bytes_read - 3] == `\n`
			&& msg[total_bytes_read - 4] == `\r` {
			return msg[..total_bytes_read].bytestr()
		}
	}
	return error('proxy request headers exceeded 4096 bytes')
}

fn copy_https_proxy_test_tunnel_data(mut src net.TcpConn, mut dst net.TcpConn) ProxyTunnelCopyResult {
	mut buf := []u8{len: 1024}
	bytes_read := src.read(mut buf) or {
		if err.code() == net.err_timed_out_code {
			return .timeout
		}
		return .closed
	}
	if bytes_read <= 0 {
		return .closed
	}
	dst.write(buf[..bytes_read]) or { return .closed }
	return .data
}

fn test_https_proxy_requests_do_not_leak_sockets() ! {
	$if windows {
		return
	}
	target_port, target_done := start_https_proxy_test_target_server()!
	proxy_port, proxy_done := start_https_proxy_test_server(target_port)!
	baseline_fds := count_open_file_descriptors()
	proxy := new_http_proxy('http://127.0.0.1:${proxy_port}')!
	for _ in 0 .. proxy_https_request_count {
		resp := fetch(
			method:   .get
			url:      'https://127.0.0.1:${target_port}/'
			proxy:    proxy
			validate: false
		)!
		assert resp.status_code == 200
		assert resp.body == 'ok'
	}
	_ = <-target_done
	_ = <-proxy_done
	time.sleep(100 * time.millisecond)
	final_fds := count_open_file_descriptors()
	assert final_fds <= baseline_fds + 3
}

fn test_http_proxy_do() {
	env := os.environ()
	mut env_proxy := ''

	for envvar in ['http_proxy', 'HTTP_PROXY', 'https_proxy', 'HTTPS_PROXY'] {
		prox_val := env[envvar] or { continue }
		if prox_val != '' {
			env_proxy = env[envvar]
		}
	}
	if env_proxy != '' {
		println('Has usable proxy env vars')
		proxy := new_http_proxy(env_proxy)!
		mut header := new_header(key: .user_agent, value: 'vlib')
		header.add_custom('X-Vlang-Test', 'proxied')!
		res := proxy.http_do(urllib.parse('http://httpbin.org/headers')!, Method.get, '/headers', &Request{
			proxy:  proxy
			header: header
		})!
		println(res.status_code)
		println('he4aders ${res.header}')
		assert res.status_code == 200
		// assert res.header.data['X-Vlang-Test'] == 'proxied'
	} else {
		println('Proxy env vars (HTTP_PROXY or HTTPS_PROXY) not set. Skipping test.')
	}
}
