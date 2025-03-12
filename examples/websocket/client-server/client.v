// vtest build: present_openssl?
module main

import os
import net.websocket
import term

// This client should be compiled an run in different consoles
// it connects to the server who will broadcast your messages
// to all other connected clients
fn main() {
	mut ws := start_client()!
	defer {
		unsafe {
			ws.free()
		}
	}
	println(term.green('client ${ws.id} ready'))
	println('Write message and enter to send...')
	for {
		line := os.get_line()
		if line == '' {
			break
		}
		ws.write_string(line)!
	}
	ws.close(1000, 'normal') or {
		eprintln(term.red('ws.close err: ${err}'))
		exit(1)
	}
}

fn start_client() !&websocket.Client {
	mut ws := websocket.new_client('ws://localhost:30000')!
	// mut ws := websocket.new_client('wss://echo.websocket.org:443')?
	// use on_open_ref if you want to send any reference object
	ws.on_open(fn (mut ws websocket.Client) ! {
		println(term.green('ws.on_open websocket connected to the server and ready to send messages...'))
	})
	// use on_error_ref if you want to send any reference object
	ws.on_error(fn (mut ws websocket.Client, err string) ! {
		println(term.red('ws.on_error error: ${err}'))
	})
	// use on_close_ref if you want to send any reference object
	ws.on_close(fn (mut ws websocket.Client, code int, reason string) ! {
		println(term.green('ws.on_close the connection to the server successfully closed'))
	})
	// on new messages from other clients, display them in blue text
	ws.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ! {
		if msg.payload.len > 0 {
			message := msg.payload.bytestr()
			println(term.blue('ws.on_message `${message}`'))
		}
	})

	ws.connect() or {
		eprintln(term.red('ws.connect error: ${err}'))
		return err
	}

	spawn ws.listen() // or { println(term.red('error on listen $err')) }
	return ws
}
