module websocket

import net
import time

// See https://github.com/vlang/v/issues/32371

// start_listening starts `s` on its own thread and returns the port that it listens on.
fn start_listening(mut s Server) !int {
	spawn fn [mut s] () {
		s.listen() or { panic('websocket server could not listen, err: ${err}') }
	}()
	for s.get_state() != .open {
		time.sleep(10 * time.millisecond)
	}
	addr := s.ls.addr()!
	return int(addr.port()!)
}

fn answers(address string) bool {
	mut conn := net.dial_tcp(address) or { return false }
	conn.close() or {}
	return true
}

// has_address reports whether `ip` is an address of this machine, by binding it:
// the bind fails at once where it is not.
fn has_address(ip string) bool {
	mut l := net.listen_tcp(.ip, '${ip}:0') or { return false }
	l.close() or {}
	return true
}

fn test_listen_address() {
	assert new_server(.ip, 30000, '').listen_address() == ':30000'
	assert new_server(.ip, 30000, '', host: '127.0.0.1').listen_address() == '127.0.0.1:30000'
	assert new_server(.ip, 30000, '', host: 'localhost').listen_address() == 'localhost:30000'
	assert new_server(.ip6, 30000, '', host: '::1').listen_address() == '[::1]:30000'
	assert new_server(.ip6, 30000, '', host: '[::1]').listen_address() == '[::1]:30000'
}

// test_host_limits_the_server_to_that_address starts one server without `host` and one with
// `host: '127.0.0.1'`. Both must answer on 127.0.0.1. Where 127.0.0.2 is also an address of
// this machine (the whole 127.0.0.0/8 block is the loopback on Linux and Windows), the server
// without `host` must answer there too, and the one limited to 127.0.0.1 must not.
// Where 127.0.0.2 is not an address of this machine (macOS and the BSDs configure 127.0.0.1
// alone), there is no second address to ask on, and that half is not judged.
fn test_host_limits_the_server_to_that_address() ! {
	mut everywhere := new_server(.ip, 0, '')
	everywhere_port := start_listening(mut everywhere)!
	mut local_only := new_server(.ip, 0, '', host: '127.0.0.1')
	local_port := start_listening(mut local_only)!
	assert answers('127.0.0.1:${everywhere_port}')
	assert answers('127.0.0.1:${local_port}')
	if !has_address('127.0.0.2') {
		eprintln('> 127.0.0.2 is not an address of this machine: the other-address half is not judged')
		return
	}
	assert answers('127.0.0.2:${everywhere_port}'), 'a server without host did not answer on 127.0.0.2'
	assert !answers('127.0.0.2:${local_port}'), 'a server with host: 127.0.0.1 answered on 127.0.0.2'
}
