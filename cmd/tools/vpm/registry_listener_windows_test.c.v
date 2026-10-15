module main

import net

fn registry_listener_option(handle int, option int) !int {
	mut value := i32(0)
	mut length := u32(sizeof(value))
	net.socket_error(C.getsockopt(handle, C.SOL_SOCKET, option, &value, &length))!
	assert length == sizeof(value)
	return int(value)
}

fn test_windows_registry_listener_is_exclusive_without_address_reuse() {
	mut listener := registry_listen_tcp(0)!
	defer { listener.close() or {} }
	assert registry_listener_option(listener.sock.handle, C.SO_EXCLUSIVEADDRUSE)! == 1
	assert registry_listener_option(listener.sock.handle, C.SO_REUSEADDR)! == 0
	assert listener.addr()!.port()! > 0
	assert listener.accept_deadline.unix() == 0
	assert listener.accept_timeout() == net.infinite_timeout
	$if net_nonblocking_sockets ? {
		assert !listener.is_blocking
	} $else {
		assert listener.is_blocking
	}
}
