module main

import os
import x.websocket
import term

// this server accepts client connections and broadcast all messages to other connected clients
fn main() {
	go start_server()
	println('press enter to quit...')
	os.get_line()
}

fn start_server() ? {
	mut s := websocket.new_server(30000, '')
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
	
	// on_message_ref, broadcast all incoming messages to all clients except the one sent it
	s.on_message_ref(fn (mut ws websocket.Client, msg &websocket.Message, mut m websocket.Server) ? {
		for _, cli in m.clients {
			mut c := cli
			if c.client.state == .open && c.client.id !=  ws.id {
				c.client.write(msg.payload, websocket.OPCode.text_frame) or {
					panic(err)
				}
			}
		}
	}, s)

	s.on_close(fn (mut ws websocket.Client, code int, reason string) ? {
		println(term.green('client ($ws.id) closed connection'))
	})
	s.listen() or {
		println(term.red('error on server listen: $err'))
	}
	unsafe {
		s.free()
	}
}
