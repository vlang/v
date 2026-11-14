module tls

import net
import time

fn test_loopback_handshake() {
	mut ln := net.listen_tcp(.ip, '127.0.0.1:18448', net.ListenOptions{}) or { panic(err) }

	spawn fn (mut ln net.TcpListener) {
		mut conn := ln.accept() or {
			eprintln('accept err')
			return
		}
		mut sc := accept(conn, Config{ verify: false }) or {
			eprintln('accept2 err: ${err}')
			return
		}
		sc.handshake() or {
			eprintln('server hs err: ${err}')
			return
		}
		eprintln('server hs OK')
		sc.close() or {}
	}(mut ln)

	time.sleep(500 * time.millisecond)

	mut c := dial('127.0.0.1', 18448, Config{ server_name: 'localhost', verify: false }) or { panic(err) }
	c.handshake() or { panic('client hs err: ${err}') }
	eprintln('client hs OK')
	c.close() or {}
	ln.close() or {}
}
