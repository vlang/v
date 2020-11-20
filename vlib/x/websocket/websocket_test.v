import x.websocket
import time

// Tests with external ws & wss servers
fn test_ws() {
	go start_server()
	time.sleep_ms(100)
	ws_test('ws://localhost:30000') or {
		assert false
	}
}

fn start_server() ? {
	mut s := websocket.new_server(30000, '')
	// Make that in execution test time give time to execute at least one time
	s.ping_interval = 100
	s.on_connect(fn (mut s websocket.ServerClient) ?bool {
		// Here you can look att the client info and accept or not accept
		// just returning a true/false
		if s.resource_name != '/' {
			panic('unexpected resource name in test')
			return false
		}
		return true
	}) ?
	s.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ? {
		// payload := if msg.payload.len == 0 { '' } else { string(msg.payload, msg.payload.len) }
		// println('server client ($ws.id) got message: opcode: $msg.opcode, payload: $payload')
		ws.write(msg.payload, msg.opcode) or {
			panic(err)
		}
	})
	s.on_close(fn (mut ws websocket.Client, code int, reason string) ? {
		// println('client ($ws.id) closed connection')
	})
	s.listen() or {
		// println('error on server listen: $err')
	}
}

fn ws_test(uri string) ? {
	eprintln('connecting to $uri ...')
	mut ws := websocket.new_client(uri) ?
	ws.on_open(fn (mut ws websocket.Client) ? {
		println('open!')
		ws.pong()
		assert true
	})
	ws.on_error(fn (mut ws websocket.Client, err string) ? {
		println('error: $err')
		// this can be thrown by internet connection problems
		assert false
	})
	ws.on_close(fn (mut ws websocket.Client, code int, reason string) ? {
		println('closed')
	})
	ws.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ? {
		println('client got type: $msg.opcode payload:\n$msg.payload')
		if msg.opcode == .text_frame {
			smessage := msg.payload.bytestr()
			println('Message: $smessage')
			assert smessage == 'a'
		} else {
			println('Binary message: $msg')
		}
	})
	ws.connect() or {
		panic('fail to connect')
	}
	go ws.listen()
	text := ['a'].repeat(2)
	for msg in text {
		ws.write(msg.bytes(), .text_frame) or {
			panic('fail to write to websocket')
		}
		// sleep to give time to recieve response before send a new one
		time.sleep_ms(100)
	}
	// sleep to give time to recieve response before asserts
	time.sleep_ms(500)
}
