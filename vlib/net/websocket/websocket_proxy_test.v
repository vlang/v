// vtest build: !windows
import encoding.base64
import os
import net
import net.websocket
import time

const proxy_github_job = os.getenv('GITHUB_JOB')

const proxy_should_skip = proxy_github_job != '' && proxy_github_job != 'websocket_tests'

const proxy_ws_target_port = 30105
const proxy_ws_port = 30106
const proxy_ping_payload = 'through the proxy'

@[heap]
struct ProxyWebsocketTestResults {
	echoes       chan bool
	initial_pong chan bool
	ping_reply   chan bool
pub mut:
	error_message string
}

fn start_proxy_ws_server(listen_port int) ! {
	mut s := websocket.new_server(.ip, listen_port, '')
	s.on_connect(fn (mut s websocket.ServerClient) !bool {
		return s.resource_name == '/'
	})!
	s.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ! {
		if msg.opcode == .pong {
			ws.write_string('pong:${msg.payload.bytestr()}')!
		} else if msg.opcode == .text_frame && msg.payload.bytestr() == 'request-ping' {
			ws.write(proxy_ping_payload.bytes(), .ping)!
		} else {
			ws.write(msg.payload, msg.opcode)!
		}
	})
	start_proxy_ws_server_in_thread(mut s)
}

fn start_proxy_ws_server_in_thread(mut ws websocket.Server) {
	spawn fn [mut ws] () {
		ws.listen() or { panic('websocket server could not listen, err: ${err}') }
	}()
	for ws.get_state() != .open {
		time.sleep(10 * time.millisecond)
	}
}

fn start_proxy_server(listen_port int, target string, requests chan string) thread {
	ready := chan bool{cap: 1}
	proxy := spawn fn [listen_port, target, requests, ready] () {
		mut listener := net.listen_tcp(.ip, ':${listen_port}') or { panic(err) }
		ready <- true
		mut client := listener.accept() or {
			listener.close() or {}
			panic(err)
		}
		listener.close() or {}
		request := read_proxy_request(mut client) or {
			requests <- 'ERROR: ${err}'
			client.close() or {}
			return
		}
		requests <- request
		mut upstream := net.dial_tcp(target) or {
			client.write_string('HTTP/1.1 502 Bad Gateway\r\n\r\n') or {}
			client.close() or {}
			return
		}
		client.write_string('HTTP/1.1 200 Connection Established\r\n\r\n') or {
			client.close() or {}
			upstream.close() or {}
			return
		}
		tunnel_proxy_connection(mut client, mut upstream)
	}()
	_ := <-ready
	return proxy
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

fn tunnel_proxy_connection(mut client net.TcpConn, mut upstream net.TcpConn) {
	defer {
		client.close() or {}
		upstream.close() or {}
	}
	client.set_read_timeout(50 * time.millisecond)
	upstream.set_read_timeout(50 * time.millisecond)
	deadline := time.now().add(4 * time.second)
	mut buf := []u8{len: 1024}
	for time.now() < deadline {
		if !relay_proxy_connection(mut client, mut upstream, mut buf)
			|| !relay_proxy_connection(mut upstream, mut client, mut buf) {
			break
		}
	}
}

fn relay_proxy_connection(mut src net.TcpConn, mut dst net.TcpConn, mut buf []u8) bool {
	bytes_read := src.read(mut buf) or {
		return err.code() == net.err_timed_out_code
	}
	if bytes_read == 0 {
		return false
	}
	dst.write(buf[..bytes_read]) or { return false }
	return true
}

fn proxy_open_cb(mut client websocket.Client) ! {
	client.pong()!
}

fn proxy_error_cb(mut client websocket.Client, err string, mut res ProxyWebsocketTestResults) ! {
	res.error_message = err
}

fn proxy_message_cb(mut client websocket.Client, msg &websocket.Message, mut res ProxyWebsocketTestResults) ! {
	if msg.opcode != .text_frame {
		return
	}
	match msg.payload.bytestr() {
		'pong:' {
			res.initial_pong.try_push(true)
		}
		'pong:${proxy_ping_payload}' {
			res.ping_reply.try_push(true)
		}
		'a' {
			status := res.echoes.try_push(true)
			assert status == .success, 'received extra echoes through the proxy'
		}
		else {
			res.error_message = 'unexpected payload: ${msg.payload.bytestr()}'
		}
	}
}

fn wait_for_proxy_event(events chan bool, description string) ! {
	select {
		_ := <-events {
		}
		5 * time.second {
			return error('timed out waiting for ${description}')
		}
	}
}

fn test_ws_connection_through_proxy() ! {
	if proxy_should_skip {
		return
	}
	start_proxy_ws_server(proxy_ws_target_port)!
	requests := chan string{cap: 1}
	proxy := start_proxy_server(proxy_ws_port, '127.0.0.1:${proxy_ws_target_port}', requests)
	mut results := ProxyWebsocketTestResults{
		echoes:       chan bool{cap: 2}
		initial_pong: chan bool{cap: 1}
		ping_reply:   chan bool{cap: 1}
	}
	mut client := websocket.new_client('ws://127.0.0.1:${proxy_ws_target_port}', websocket.ClientOpt{
		proxy_url: 'http://user:pass@127.0.0.1:${proxy_ws_port}'
	})!
	client.on_open(proxy_open_cb)
	client.on_error_ref(proxy_error_cb, &results)
	client.on_message_ref(proxy_message_cb, &results)
	client.connect()!
	defer {
		if client.get_state() == .open {
			client.close(1000, 'done') or {}
		}
	}
	done := chan bool{cap: 1}
	spawn fn [mut client, done] () {
		client.listen() or {}
		done <- true
	}()
	for _ in 0 .. 2 {
		client.write_string('a')!
		wait_for_proxy_event(results.echoes, 'a message echo through the proxy')!
	}
	wait_for_proxy_event(results.initial_pong, 'the initial pong through the proxy')!
	client.write_string('request-ping')!
	wait_for_proxy_event(results.ping_reply, 'the ping payload echoed through the proxy')!
	request := <-requests
	auth_token := base64.encode('user:pass'.bytes())
	assert request.starts_with('CONNECT 127.0.0.1:${proxy_ws_target_port} HTTP/1.1\r\n')
	assert request.contains('Proxy-Connection: Keep-Alive\r\n')
	assert request.contains('Proxy-Authorization: Basic ${auth_token}\r\n')
	client.close(1000, 'done')!
	wait_for_proxy_event(done, 'the client listener to exit')!
	proxy.wait()
	assert results.error_message == ''
	assert results.echoes.len == 0, 'received extra echoes through the proxy'
}
