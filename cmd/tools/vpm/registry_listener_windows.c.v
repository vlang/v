module main

import net
import time

#include "@VMODROOT/cmd/tools/vpm/registry_listener.h"

fn C.vpm_registry_prepare_listener(handle i32) i32
fn C.vpm_registry_listen(handle i32) i32

// registry_listen_tcp_exclusive keeps the normal TCP defaults, but changes
// address ownership before bind. Any failure leaves no socket owned by the caller.
fn registry_listen_tcp_exclusive(port int) !&net.TcpListener {
	socket := net.new_tcp_socket(.ip)!
	mut keep_socket := false
	defer {
		if !keep_socket {
			net.close(socket.handle) or {}
		}
	}
	net.socket_error_message(C.vpm_registry_prepare_listener(socket.handle),
		'reserving the registry port for exclusive use failed')!
	mut bound := socket
	bound.bind(':${port}')!
	net.socket_error_message(C.vpm_registry_listen(socket.handle), 'listening for registry connections failed')!
	mut listener := &net.TcpListener{
		sock:            socket
		accept_deadline: time.unix(0)
		accept_timeout:  net.infinite_timeout
	}
	$if net_nonblocking_sockets ? {
		listener.is_blocking = false
	}
	keep_socket = true
	return listener
}
