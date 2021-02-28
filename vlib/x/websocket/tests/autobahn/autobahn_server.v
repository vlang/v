// use this to test websocket server to the autobahn test
module main

import x.websocket

fn main() {
	mut s := websocket.new_server(9002, '/')
	s.on_message(on_message)
	s.listen() or { panic(err) }
}

fn handle_case(case_nr int) ? {
	uri := 'ws://localhost:9002/runCase?case=$case_nr&agent=v-client'
	mut ws := websocket.new_client(uri) ?
	ws.on_message(on_message)
	ws.connect() ?
	ws.listen() ?
}

fn on_message(mut ws websocket.Client, msg &websocket.Message) ? {
	// autobahn tests expects to send same message back
	if msg.opcode == .pong {
		// We just wanna pass text and binary message back to autobahn
		return
	}
	ws.write(msg.payload, msg.opcode) or { panic(err) }
}
