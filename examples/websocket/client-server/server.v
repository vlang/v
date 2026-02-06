// vtest build: present_openssl?
module main

import net.websocket
import term

fn slog(message string) {
	eprintln(term.colorize(term.bright_yellow, message))
}

// this server accepts client connections and broadcast all messages to other connected clients
fn main() {
	eprintln('press ctrl-c to quit...')
	start_server()!
}

fn start_server() ! {
	slog('start_server')
	mut s := websocket.new_server(.ip6, 30000, '')
	defer {
		unsafe {
			s.free()
		}
	}
	// Make that in execution test time give time to execute at least one time
	s.set_ping_interval(100)
	s.on_connect(fn (mut s websocket.ServerClient) !bool {
		slog('s.on_connect')
		// Here you can look att the client info and accept or not accept
		// just returning a true/false
		if s.resource_name != '/' {
			return false
		}
		return true
	})!

	// on_message_ref, broadcast all incoming messages to all clients except the one sent it
	s.on_message_ref(fn (mut ws websocket.Client, msg &websocket.Message, mut m websocket.Server) ! {
		slog('s.on_message_ref')
		// for _, cli in m.clients {
		for i, _ in rlock m.server_state {
			m.server_state.clients
		} {
			mut c := rlock m.server_state {
				m.server_state.clients[i] or { continue }
			}
			if c.client.get_state() == .open && c.client.id != ws.id {
				c.client.write(msg.payload, websocket.OPCode.text_frame) or { panic(err) }
			}
		}
	}, s)

	s.on_close(fn (mut ws websocket.Client, code int, reason string) ! {
		slog('s.on_close')
		println(term.green('client (${ws.id}) closed connection'))
	})

	s.listen() or { println(term.red('error on server listen: ${err}')) }
	slog('s.listen done')
}
