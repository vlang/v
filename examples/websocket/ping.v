// vtest build: present_openssl?
module main

import time
import term
import net.websocket

fn main() {
	spawn start_server()
	time.sleep(100 * time.millisecond)
	start_client()!
}

fn slog(message string) {
	eprintln(term.colorize(term.bright_yellow, message))
}

fn clog(message string) {
	eprintln(term.colorize(term.cyan, message))
}

fn wlog(message string) {
	eprintln(term.colorize(term.bright_blue, message))
}

// start_server starts the websocket server, it receives messages
// and send it back to the client that sent it
fn start_server() ! {
	mut s := websocket.new_server(.ip6, 30000, '')
	defer {
		unsafe {
			s.free()
		}
	}
	// Make that in execution test time give time to execute at least one time
	s.set_ping_interval(100)
	s.on_connect(fn (mut s websocket.ServerClient) !bool {
		slog('ws.on_connect, s.client_key: ${s.client_key}')
		// Here you can look att the client info and accept or not accept
		// just returning a true/false
		if s.resource_name != '/' {
			return false
		}
		return true
	})!
	s.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ! {
		slog('s.on_message msg.opcode: ${msg.opcode} | msg.payload: ${msg.payload}')
		ws.write(msg.payload, msg.opcode) or {
			eprintln('ws.write err: ${err}')
			return err
		}
	})
	s.on_close(fn (mut ws websocket.Client, code int, reason string) ! {
		slog('s.on_close code: ${code}, reason: ${reason}')
		// println('client ($ws.id) closed connection')
	})
	s.listen() or {
		slog('s.listen err: ${err}')
		return err
	}
	slog('s.listen finished')
}

// start_client starts the websocket client, it writes a message to
// the server and prints all the messages received
fn start_client() ! {
	mut ws := websocket.new_client('ws://localhost:30000')!
	defer {
		unsafe {
			ws.free()
		}
	}
	// mut ws := websocket.new_client('wss://echo.websocket.org:443')?
	// use on_open_ref if you want to send any reference object
	ws.on_open(fn (mut ws websocket.Client) ! {
		clog('ws.on_open')
	})
	// use on_error_ref if you want to send any reference object
	ws.on_error(fn (mut ws websocket.Client, err string) ! {
		clog('ws.on_error error: ${err}')
	})
	// use on_close_ref if you want to send any reference object
	ws.on_close(fn (mut ws websocket.Client, code int, reason string) ! {
		clog('ws.on_close')
	})
	// use on_message_ref if you want to send any reference object
	ws.on_message(fn (mut ws websocket.Client, msg &websocket.Message) ! {
		if msg.payload.len > 0 {
			message := msg.payload.bytestr()
			clog('ws.on_message client got type: ${msg.opcode} payload: `${message}`')
		}
	})
	// you can add any pointer reference to use in callback
	// t := TestRef{count: 10}
	// ws.on_message_ref(fn (mut ws websocket.Client, msg &websocket.Message, r &SomeRef) ? {
	// // eprintln('type: $msg.opcode payload:\n$msg.payload ref: $r')
	// }, &r)
	ws.connect() or {
		clog('ws.connect err: ${err}')
		return err
	}
	clog('ws.connect succeeded')
	spawn write_echo(mut ws) // or { println('error on write_echo $err') }
	ws.listen() or {
		clog('ws.listen err: ${err}')
		return err
	}
	clog('ws.listen finished')
}

fn write_echo(mut ws websocket.Client) ! {
	wlog('write_echo, start')
	message := 'echo this'
	for i := 0; i <= 5; i++ {
		// Server will send pings every 30 seconds
		wlog('write_echo, writing message: `${message}` ...')
		ws.write_string(message) or {
			wlog('write_echo, ws.write_string err: ${err}')
			return err
		}
		time.sleep(100 * time.millisecond)
	}
	ws.close(1000, 'normal') or {
		wlog('write_echo, close err: ${err}')
		return err
	}
	wlog('write_echo, done')
}
