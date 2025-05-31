// vtest build: !windows
import os
import net
import net.websocket
import time

@[heap]
struct WebsocketTestResults {
pub mut:
	nr_messages      int
	nr_pong_received int
	nr_closes        int
}

// Do not run these tests every time, since they are flaky.
// They have their own specialized CI runner.
const github_job = os.getenv('GITHUB_JOB')

const should_skip = get_should_skip()

fn get_should_skip() bool {
	return github_job != '' && github_job != 'websocket_tests'
}

// tests with internal ws servers
fn test_ws_ipv6() {
	if should_skip {
		return
	}
	start_server(.ip6, 30001)!

	ws_test(.ip6, 'ws://localhost:30001') or {
		eprintln('> error while connecting .ip6, err: ${err}')
		assert false
	}
}

// tests with internal ws servers
fn test_ws_ipv4() {
	if should_skip {
		return
	}
	start_server(.ip, 30002)!

	ws_test(.ip, 'ws://localhost:30002') or {
		eprintln('> error while connecting .ip, err: ${err}')
		assert false
	}
}

fn start_server(family net.AddrFamily, listen_port int) ! {
	eprintln('> start_server family:${family} | listen_port: ${listen_port}')
	mut s := websocket.new_server(family, listen_port, '')
	// make that in execution test time give time to execute at least one time
	s.set_ping_interval(1)

	s.on_connect(fn (mut s websocket.ServerClient) !bool {
		// here you can look att the client info and accept or not accept
		// just returning a true/false
		if s.resource_name != '/' {
			panic('unexpected resource name in test')
			return false
		}
		return true
	})!
	s.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ! {
		match msg.opcode {
			.pong { ws.write_string('pong')! }
			else { ws.write(msg.payload, msg.opcode)! }
		}
	})

	s.on_close(fn (mut ws websocket.Client, code int, reason string) ! {
		// not used
	})
	start_server_in_thread_and_wait_till_it_is_ready_to_accept_connections(mut s)
	eprintln('> start_server finished')
}

fn start_server_in_thread_and_wait_till_it_is_ready_to_accept_connections(mut ws websocket.Server) {
	eprintln('-----------------------------------------------------------------------------')
	spawn fn [mut ws] () {
		ws.listen() or { panic('websocket server could not listen, err: ${err}') }
	}()
	for ws.get_state() != .open {
		time.sleep(10 * time.millisecond)
	}
	eprintln('-----------------------------------------------------------------------------')
}

fn open_cb(mut client websocket.Client) ! {
	client.pong()!
	assert true
}

fn close_cb(mut client websocket.Client, err string) ! {
	println('error: ${err}')
	// this can be thrown by internet connection problems
	assert false
}

fn message_cb(mut client websocket.Client, msg &websocket.Message, mut res WebsocketTestResults) ! {
	println('client got type: ${msg.opcode} payload:\n${msg.payload}')
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
		println('Binary message: ${msg}')
	}
}

// ws_test tests connect to the websocket server from websocket client
fn ws_test(family net.AddrFamily, uri string) ! {
	eprintln('connecting to ${uri} ...')

	mut test_results := WebsocketTestResults{}
	mut client := websocket.new_client(uri)!
	client.on_open(open_cb)
	client.on_error(close_cb)
	client.on_message_ref(message_cb, test_results)
	client.connect()!
	spawn client.listen()

	text := ['a'].repeat(2)
	for msg in text {
		client.write(msg.bytes(), .text_frame) or {
			panic('fail to write to websocket, err: ${err}')
		}
		// sleep to give time to receive response before send a new one
		time.sleep(100 * time.millisecond)
	}
	// sleep to give time to receive response before asserts
	time.sleep(1500 * time.millisecond)
	// We expect at least 2 pongs, one sent directly and one indirectly
	assert test_results.nr_pong_received >= 2
	assert test_results.nr_messages == 2
}

fn on_message_cb_2(mut cli websocket.Client, msg &websocket.Message) ! {
	if msg.opcode == .text_frame {
		cli.close(1000, 'closing connection')!
	}
}

fn on_close_cb_2(mut cli websocket.Client, code int, reason string, mut res WebsocketTestResults) ! {
	res.nr_closes++
}

fn test_on_close_when_server_closing_connection() ! {
	mut ws := websocket.new_server(.ip, 30003, '')
	ws.on_message(on_message_cb_2)
	mut test_results := WebsocketTestResults{}
	ws.on_close_ref(on_close_cb_2, test_results)
	start_server_in_thread_and_wait_till_it_is_ready_to_accept_connections(mut ws)

	mut client := websocket.new_client('ws://localhost:30003')!
	client.connect()!
	spawn client.listen()
	time.sleep(1000 * time.millisecond)
	client.write_string('a message')!
	time.sleep(1000 * time.millisecond)
	assert test_results.nr_closes == 1
}

fn on_close_cb_3(mut cli websocket.Client, code int, reason string, mut res WebsocketTestResults) ! {
	res.nr_closes++
}

fn test_on_close_when_client_closing_connection() ! {
	mut ws := websocket.new_server(.ip, 30004, '')
	start_server_in_thread_and_wait_till_it_is_ready_to_accept_connections(mut ws)

	mut client := websocket.new_client('ws://localhost:30004')!
	mut test_results := WebsocketTestResults{}
	client.on_close_ref(on_close_cb_3, test_results)
	client.connect()!
	spawn client.listen()
	time.sleep(1000 * time.millisecond)
	client.close(1000, 'closing connection')!
	time.sleep(1000 * time.millisecond)
	assert test_results.nr_closes == 1
}
