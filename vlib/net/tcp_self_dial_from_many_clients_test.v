module main

import io
import net
import time

const self_dial_clients = 20
const self_dial_timeout = 5 * time.second

fn start_self_dial_client(address string, id u8, dial_errors chan string, close_clients chan bool) ! {
	mut conn := net.dial_tcp(address) or {
		dial_errors <- err.msg()
		return
	}
	defer { conn.close() or {} }
	dial_errors <- ''
	conn.set_write_timeout(self_dial_timeout)
	conn.write([id])!
	_ = <-close_clients or { false }
}

fn test_tcp_self_dialing() {
	// Keep a slot in the backlog for each client while the dials are reported.
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0', backlog: self_dial_clients)!
	defer { listener.close() or {} }
	listener.set_accept_timeout(self_dial_timeout)
	address := listener.addr()!.str()
	dial_errors := chan string{cap: self_dial_clients}
	close_clients := chan bool{}
	defer {
		if !close_clients.closed {
			close_clients.close()
		}
	}

	mut clients := []thread !{cap: self_dial_clients}
	for id in 0 .. self_dial_clients {
		clients << spawn start_self_dial_client(address, u8(id), dial_errors, close_clients)
	}

	mut failures := []string{}
	for _ in 0 .. self_dial_clients {
		select {
			message := <-dial_errors {
				if message != '' {
					failures << message
				}
			}
			self_dial_timeout {
				assert false, 'a TCP client did not report its dial result'
				return
			}
		}
	}
	assert failures.len < 2, 'too many failed TCP dials: ${failures}'

	mut peers := []&net.TcpConn{cap: self_dial_clients}
	defer {
		for mut peer in peers {
			peer.close() or {}
		}
	}
	for _ in 0 .. self_dial_clients - failures.len {
		peers << listener.accept()!
	}
	// Release the clients only after all successful connections are accepted.
	close_clients.close()
	for client in clients {
		client.wait()!
	}

	mut seen := []u8{cap: peers.len}
	mut data := []u8{len: 1}
	for mut peer in peers {
		peer.set_read_timeout(self_dial_timeout)
		io.read_exact(mut peer, mut data)!
		assert data[0] < self_dial_clients
		assert data[0] !in seen
		seen << data[0]
		peer.read(mut data) or {
			assert err is io.Eof
			continue
		}
		assert false, 'expected the client to close after sending its byte'
	}
}
