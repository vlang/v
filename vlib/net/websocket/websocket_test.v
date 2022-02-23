import os
import net
import net.websocket
import time
import rand

struct WebsocketTestResults {
pub mut:
	nr_messages      int
	nr_pong_received int
}

// Do not run these tests everytime, since they are flaky.
// They have their own specialized CI runner.
const github_job = os.getenv('GITHUB_JOB')

const should_skip = github_job != '' && github_job != 'websocket_tests'

// tests with internal ws servers
fn test_ws_ipv6() {
	if should_skip {
		return
	}
	port := 30000 + rand.intn(1024) or { 0 }
	go start_server(.ip6, port)
	time.sleep(500 * time.millisecond)
	ws_test(.ip6, 'ws://localhost:$port') or { assert false }
}

// tests with internal ws servers
fn test_ws_ipv4() {
	if should_skip {
		return
	}
	port := 30000 + rand.intn(1024) or { 0 }
	go start_server(.ip, port)
	time.sleep(500 * time.millisecond)
	ws_test(.ip, 'ws://localhost:$port') or { assert false }
}

fn start_server(family net.AddrFamily, listen_port int) ? {
	mut s := websocket.new_server(family, listen_port, '')
	// make that in execution test time give time to execute at least one time
	s.ping_interval = 1

	s.on_connect(fn (mut s websocket.ServerClient) ?bool {
		// here you can look att the client info and accept or not accept
		// just returning a true/false
		if s.resource_name != '/' {
			panic('unexpected resource name in test')
			return false
		}
		return true
	}) ?
	s.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ? {
		match msg.opcode {
			.pong { ws.write_string('pong') or { panic(err) } }
			else { ws.write(msg.payload, msg.opcode) or { panic(err) } }
		}
	})

	s.on_close(fn (mut ws websocket.Client, code int, reason string) ? {
		// not used
	})
	s.listen() or { panic('websocket server could not listen') }
}

// ws_test tests connect to the websocket server from websocket client
fn ws_test(family net.AddrFamily, uri string) ? {
	eprintln('connecting to $uri ...')

	mut test_results := WebsocketTestResults{}
	mut ws := websocket.new_client(uri) ?
	ws.on_open(fn (mut ws websocket.Client) ? {
		ws.pong() ?
		assert true
	})
	ws.on_error(fn (mut ws websocket.Client, err string) ? {
		println('error: $err')
		// this can be thrown by internet connection problems
		assert false
	})

	ws.on_message_ref(fn (mut ws websocket.Client, msg &websocket.Message, mut res WebsocketTestResults) ? {
		println('client got type: $msg.opcode payload:\n$msg.payload')
		if msg.opcode == .text_frame {
			smessage := msg.payload.bytestr()
			match smessage {
				'pong' {
					res.nr_pong_received++
				}
				'a' {
					res.nr_messages++
				}
				else {
					assert false
				}
			}
		} else {
			println('Binary message: $msg')
		}
	}, test_results)
	ws.connect() or { panic('fail to connect') }
	go ws.listen()
	text := ['a'].repeat(2)
	for msg in text {
		ws.write(msg.bytes(), .text_frame) or { panic('fail to write to websocket') }
		// sleep to give time to recieve response before send a new one
		time.sleep(100 * time.millisecond)
	}
	// sleep to give time to recieve response before asserts
	time.sleep(1500 * time.millisecond)
	// We expect at least 2 pongs, one sent directly and one indirectly
	assert test_results.nr_pong_received >= 2
	assert test_results.nr_messages == 2
}
