module tls

import net
import os
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

// test_loopback_close_notify proves send_close_notify reaches the peer as
// an alert: after the echo the server notifies, and the client's next
// read fails with a peer-alert error instead of a silent EOF.
fn test_loopback_close_notify() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	mut l := listen(Config{
		cert: os.join_path(td, 'rsa_leaf_cert.pem')
		key:  os.join_path(td, 'rsa_leaf_pkcs8_key.pem')
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	done := chan string{cap: 1}
	spawn fn (mut l Listener, done chan string) {
		mut conn := l.accept() or {
			done <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		mut buf := []u8{len: 4}
		n := conn.read(mut buf) or {
			done <- 'read err: ${err}'
			return
		}
		conn.write(buf[..n]) or {
			done <- 'write err: ${err}'
			return
		}
		conn.send_close_notify() or {
			done <- 'notify err: ${err}'
			return
		}
		done <- 'ok'
	}(mut l, done)
	mut c := dial('127.0.0.1', port, Config{ server_name: 'localhost', verify: false }) or {
		panic(err)
	}
	c.handshake() or { panic('client hs err: ${err}') }
	c.write('ping'.bytes()) or { panic('write: ${err}') }
	mut buf := []u8{len: 4}
	n := c.read(mut buf) or { panic('echo read: ${err}') }
	assert buf[..n] == 'ping'.bytes()
	c.read(mut buf) or {
		assert err.msg().contains('peer alert')
		assert <-done == 'ok'
		l.close() or {}
		return
	}
	c.close() or {}
	l.close() or {}
	assert false, 'read after close_notify must fail'
}
