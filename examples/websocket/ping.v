module main

import time
import net.websocket

fn main() {
	go start_server()
	time.sleep(100 * time.millisecond)
	start_client()?
}

// start_server starts the websocket server, it receives messages
// and send it back to the client that sent it
fn start_server() ? {
	mut s := websocket.new_server(.ip6, 30000, '')
	// Make that in execution test time give time to execute at least one time
	s.ping_interval = 100
	s.on_connect(fn (mut s websocket.ServerClient) ?bool {
		// Here you can look att the client info and accept or not accept
		// just returning a true/false
		if s.resource_name != '/' {
			return false
		}
		return true
	})?
	s.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ? {
		ws.write(msg.payload, msg.opcode) or { panic(err) }
	})
	s.on_close(fn (mut ws websocket.Client, code int, reason string) ? {
		// println('client ($ws.id) closed connection')
	})
	s.listen() or { println('error on server listen: $err') }
	unsafe {
		s.free()
	}
}

// start_client starts the websocket client, it writes a message to
// the server and prints all the messages received
fn start_client() ? {
	mut ws := websocket.new_client('ws://localhost:30000')?
	// mut ws := websocket.new_client('wss://echo.websocket.org:443')?
	// use on_open_ref if you want to send any reference object
	ws.on_open(fn (mut ws websocket.Client) ? {
		println('open!')
	})
	// use on_error_ref if you want to send any reference object
	ws.on_error(fn (mut ws websocket.Client, err string) ? {
		println('error: $err')
	})
	// use on_close_ref if you want to send any reference object
	ws.on_close(fn (mut ws websocket.Client, code int, reason string) ? {
		println('closed')
	})
	// use on_message_ref if you want to send any reference object
	ws.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ? {
		if msg.payload.len > 0 {
			message := msg.payload.bytestr()
			println('client got type: $msg.opcode payload:\n$message')
		}
	})
	// you can add any pointer reference to use in callback
	// t := TestRef{count: 10}
	// ws.on_message_ref(fn (mut ws websocket.Client, msg &websocket.Message, r &SomeRef) ? {
	// // println('type: $msg.opcode payload:\n$msg.payload ref: $r')
	// }, &r)
	ws.connect() or { println('error on connect: $err') }
	go write_echo(mut ws) // or { println('error on write_echo $err') }
	ws.listen() or { println('error on listen $err') }
	unsafe {
		ws.free()
	}
}

fn write_echo(mut ws websocket.Client) ? {
	message := 'echo this'
	for i := 0; i <= 10; i++ {
		// Server will send pings every 30 seconds
		ws.write_string(message) or { println('panicing writing $err') }
		time.sleep(100 * time.millisecond)
	}
	ws.close(1000, 'normal') or { println('panicing $err') }
}
