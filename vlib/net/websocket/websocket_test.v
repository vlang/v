// vtest build: !windows
import os
import net
import net.websocket
import time

@[heap]
struct WebsocketCloseCount {
pub mut:
	nr_closes int
}

@[heap]
struct WebsocketEchoEvents {
	echoes chan bool
	pongs  chan bool
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

fn message_cb(mut client websocket.Client, msg &websocket.Message, events &WebsocketEchoEvents) ! {
	println('client got type: ${msg.opcode} payload:\n${msg.payload}')
	if msg.opcode == .text_frame {
		smessage := msg.payload.bytestr()
		match smessage {
			'pong' {
				events.pongs.try_push(true)
			}
			'a' {
				status := events.echoes.try_push(true)
				assert status == .success, 'received extra echoes'
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

	events := &WebsocketEchoEvents{
		echoes: chan bool{cap: 2}
		pongs:  chan bool{cap: 2}
	}
	mut client := websocket.new_client(uri)!
	client.on_open(open_cb)
	client.on_error(close_cb)
	client.on_message_ref(message_cb, events)
	client.connect()!
	defer {
		if client.get_state() == .open {
			client.close(1000, 'done') or {}
		}
	}
	done := start_ws_listener(mut client)
	for _ in 0 .. 2 {
		client.write_string('a')!
		wait_for_ws_event(events.echoes, 'a message echo')!
	}
	// Expect a response to the client's initial pong and to the pong sent
	// automatically in response to the server's periodic ping.
	for _ in 0 .. 2 {
		wait_for_ws_event(events.pongs, 'a pong response')!
	}
	client.close(1000, 'done')!
	wait_for_ws_event(done, 'the client listener to exit')!
	assert events.echoes.len == 0, 'received extra echoes'
}

fn count_close(mut cli websocket.Client, code int, reason string, mut res WebsocketCloseCount) ! {
	res.nr_closes++
}

fn wait_for_ws_event(events chan bool, description string) ! {
	select {
		_ := <-events {
		}
		5 * time.second {
			return error('timed out waiting for ${description}')
		}
	}
}

fn start_ws_listener(mut client websocket.Client) chan bool {
	done := chan bool{cap: 1}
	spawn fn [mut client, done] () {
		client.listen() or {}
		done <- true
	}()
	return done
}

fn test_on_close_when_server_closing_connection() ! {
	mut ws := websocket.new_server(.ip, 30003, '')
	closed := chan bool{cap: 1}
	ws.on_message(fn [closed] (mut cli websocket.Client, msg &websocket.Message) ! {
		if msg.opcode == .text_frame {
			cli.close(1000, 'closing connection')!
			closed <- true
		}
	})
	mut test_results := WebsocketCloseCount{}
	ws.on_close_ref(count_close, &test_results)
	start_server_in_thread_and_wait_till_it_is_ready_to_accept_connections(mut ws)

	mut client := websocket.new_client('ws://localhost:30003')!
	client.connect()!
	done := start_ws_listener(mut client)
	client.write_string('a message')!
	wait_for_ws_event(closed, 'the server close')!
	wait_for_ws_event(done, 'the client listener to exit')!
	assert test_results.nr_closes == 1
}

fn test_close_unconnected_client_returns_error() ! {
	mut client := websocket.new_client('ws://localhost:30004')!
	client.close(1000, 'closing connection') or {
		assert err.msg() == 'Socket already closed: 1000'
		return
	}
	assert false, 'closing an unconnected client should return an error'
}

fn test_on_close_when_client_closing_connection() ! {
	mut ws := websocket.new_server(.ip, 30004, '')
	ws.on_attached(fn (mut sc websocket.ServerClient) ! {
		sc.client.write_string('ready')!
	})
	start_server_in_thread_and_wait_till_it_is_ready_to_accept_connections(mut ws)

	mut client := websocket.new_client('ws://localhost:30004')!
	mut test_results := WebsocketCloseCount{}
	client.on_close_ref(count_close, &test_results)
	ready := chan bool{cap: 1}
	client.on_message(fn [ready] (mut cli websocket.Client, msg &websocket.Message) ! {
		if msg.opcode == .text_frame && msg.payload.bytestr() == 'ready' {
			ready <- true
		}
	})
	client.connect()!
	done := start_ws_listener(mut client)
	wait_for_ws_event(ready, 'the client listener to receive the first message')!
	client.close(1000, 'closing connection')!
	wait_for_ws_event(done, 'the client listener to exit')!
	assert test_results.nr_closes == 1
}

fn attached_fail_cb(mut sc websocket.ServerClient) ! {
	return error('attached hook failure')
}

fn client_close_ignore_cb(mut cli websocket.Client, code int, reason string) ! {}

fn test_on_attached_error_does_not_leak_client() ! {
	mut ws := websocket.new_server(.ip, 30005, '')
	ws.on_attached(attached_fail_cb)
	start_server_in_thread_and_wait_till_it_is_ready_to_accept_connections(mut ws)

	mut client := websocket.new_client('ws://localhost:30005')!
	client.on_close(client_close_ignore_cb)
	client.connect()!
	done := start_ws_listener(mut client)
	// The server removes the client before closing its socket.
	wait_for_ws_event(done, 'the rejected client listener to exit')!

	client_count := rlock ws.server_state {
		ws.server_state.clients.len
	}
	assert client_count == 0
}
