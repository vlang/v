module main

import os
import x.websocket
import term

// This client should be compiled an run in different konsoles 
// it connects to the server who will broadcast your messages
// to all other connected clients
fn main() {
	mut ws := start_client() ?
	println(term.green('client $ws.id ready'))
	println('Write message and enter to send...')
	for {
		line := os.get_line() 
		if line == '' {
			break
		}
		ws.write_str(line)
	}
	ws.close(1000, 'normal') or {
		println(term.red('panicing $err'))
	}
	unsafe {
		ws.free()
	}
}

fn start_client() ?&websocket.Client {
	mut ws := websocket.new_client('ws://localhost:30000')?
	// mut ws := websocket.new_client('wss://echo.websocket.org:443')?
	// use on_open_ref if you want to send any reference object
	ws.on_open(fn (mut ws websocket.Client) ? {
		println(term.green('websocket connected to the server and ready to send messages...'))
	})
	// use on_error_ref if you want to send any reference object
	ws.on_error(fn (mut ws websocket.Client, err string) ? {
		println(term.red('error: $err'))
	})
	// use on_close_ref if you want to send any reference object
	ws.on_close(fn (mut ws websocket.Client, code int, reason string) ? {
		println(term.green('the connection to the server successfully closed'))
	})
	// on new messages from other clients, display them in blue text
	ws.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ? {
		if msg.payload.len > 0 {
			message := msg.payload.bytestr()
			println(term.blue('$message'))
		}
	})

	ws.connect() or {
		println(term.red('error on connect: $err'))
	}
	
	go ws.listen() or {
		println(term.red('error on listen $err'))
	}
	return ws
}