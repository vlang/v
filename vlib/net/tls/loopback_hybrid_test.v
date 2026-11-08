module tls

import net
import time

fn test_loopback_hybrid() {
	mut ln := net.listen_tcp(.ip, '127.0.0.1:18449', net.ListenOptions{}) or { panic(err) }

	spawn fn (mut ln net.TcpListener) {
		mut conn := ln.accept() or { return }
		mut sc := accept(conn, Config{ verify: false, pq_mode: .hybrid_prefer }) or { return }
		sc.handshake() or {
			eprintln('server hs err: ${err}')
			return
		}
		mut buf := []u8{len: 64}
		n := sc.read(mut buf) or { return }
		sc.write(buf[..n]) or {}
		sc.close() or {}
	}(mut ln)

	time.sleep(500 * time.millisecond)

	mut c := dial('127.0.0.1', 18449, Config{ server_name: 'localhost', verify: false, pq_mode: .hybrid_prefer }) or { panic(err) }
	c.handshake() or { panic('client hs err: ${err}') }
	msg := [u8(9), 8, 7, 6]
	c.write(msg) or { panic(err) }
	mut rbuf := []u8{len: 64}
	rn := c.read(mut rbuf) or { panic(err) }
	assert rn == msg.len
	assert rbuf[..rn] == msg
	c.close() or {}
	ln.close() or {}
}
