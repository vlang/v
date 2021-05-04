import os
import x.websocket
import time
import rand

struct WebsocketTestResults {
pub mut:
	nr_messages      int
	nr_pong_received int
}

const github_job = os.getenv('GITHUB_JOB')

// tests with internal ws servers
fn test_ws() {
	if github_job != '' && github_job != 'websocket_tests' {
		// Do not run these tests everytime, since they are flaky.
		// They have their own specialized CI runner.
		return
	}
	port := 30000 + rand.intn(1024)
	go start_server(port)
	time.sleep(500 * time.millisecond)
	ws_test('ws://localhost:$port') or { assert false }
}

fn start_server(listen_port int) ? {
	mut s := websocket.new_server(listen_port, '')
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
	s.listen() or {}
}

// ws_test tests connect to the websocket server from websocket client
fn ws_test(uri string) ? {
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
