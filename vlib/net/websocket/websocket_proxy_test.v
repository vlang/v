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

@[heap]
struct ProxyWebsocketTestResults {
pub mut:
	nr_messages      int
	nr_pong_received int
	error_message    string
}

fn start_proxy_ws_server(listen_port int) ! {
	mut s := websocket.new_server(.ip, listen_port, '')
	s.set_ping_interval(1)
	s.on_connect(fn (mut s websocket.ServerClient) !bool {
		return s.resource_name == '/'
	})!
	s.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ! {
		match msg.opcode {
			.pong { ws.write_string('pong')! }
			else { ws.write(msg.payload, msg.opcode)! }
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

fn start_proxy_server(listen_port int, target string, requests chan string) {
	ready := chan bool{cap: 1}
	spawn fn [listen_port, target, requests, ready] () {
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
	for time.now() < deadline {
		relay_proxy_connection(mut client, mut upstream)
		relay_proxy_connection(mut upstream, mut client)
	}
}

fn relay_proxy_connection(mut src net.TcpConn, mut dst net.TcpConn) {
	mut buf := []u8{len: 1024}
	bytes_read := src.read(mut buf) or {
		if err.code() == net.err_timed_out_code {
			return
		}
		return
	}
	if bytes_read > 0 {
		dst.write(buf[..bytes_read]) or {}
	}
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
		'pong' {
			res.nr_pong_received++
		}
		'a' {
			res.nr_messages++
		}
		else {
			res.error_message = 'unexpected payload: ${msg.payload.bytestr()}'
		}
	}
}

fn test_ws_connection_through_proxy() ! {
	if proxy_should_skip {
		return
	}
	start_proxy_ws_server(proxy_ws_target_port)!
	requests := chan string{cap: 1}
	start_proxy_server(proxy_ws_port, '127.0.0.1:${proxy_ws_target_port}', requests)
	mut results := ProxyWebsocketTestResults{}
	mut client := websocket.new_client('ws://127.0.0.1:${proxy_ws_target_port}', websocket.ClientOpt{
		proxy_url: 'http://user:pass@127.0.0.1:${proxy_ws_port}'
	})!
	client.on_open(proxy_open_cb)
	client.on_error_ref(proxy_error_cb, results)
	client.on_message_ref(proxy_message_cb, results)
	client.connect()!
	spawn client.listen()
	for msg in ['a', 'a'] {
		client.write(msg.bytes(), .text_frame)!
		time.sleep(100 * time.millisecond)
	}
	time.sleep(1500 * time.millisecond)
	request := <-requests
	auth_token := base64.encode('user:pass'.bytes())
	assert request.starts_with('CONNECT 127.0.0.1:${proxy_ws_target_port} HTTP/1.1\r\n')
	assert request.contains('Proxy-Connection: Keep-Alive\r\n')
	assert request.contains('Proxy-Authorization: Basic ${auth_token}\r\n')
	client.close(1000, 'done') or {}
	time.sleep(100 * time.millisecond)
	assert results.error_message == ''
	assert results.nr_pong_received >= 2
	assert results.nr_messages == 2
}
