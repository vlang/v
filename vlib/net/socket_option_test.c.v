module net

fn read_socket_option(handle int, option SocketOption) !int {
	mut value := i32(0)
	mut size := u32(sizeof(value))
	socket_error(C.getsockopt(handle, C.SOL_SOCKET, int(option), voidptr(&value), &size))!
	// Windows can return a single byte for boolean options.
	assert size > 0 && size <= sizeof(value)
	return int(value)
}

fn test_tcp_socket_option_values() ! {
	mut sock := new_tcp_socket(.ip)!
	defer {
		sock.close() or {}
	}
	sock.bind('127.0.0.1:0')!
	for enabled in [false, true, false] {
		sock.set_option_bool(.keep_alive, enabled)!
		actual := read_socket_option(sock.handle, .keep_alive)!
		assert (actual != 0) == enabled
	}
	sock.set_option_int(.send_buf_size, 32768)!
	// Some platforms round or double the requested buffer size.
	assert read_socket_option(sock.handle, .send_buf_size)! >= 32768
}

fn test_raw_socket_option_values() ! {
	// SOL_SOCKET options work on a datagram socket without raw-socket privileges.
	mut sock := RawSocket{
		handle: socket_error(C.socket(i32(AddrFamily.ip), i32(SocketType.udp), 0))!
	}
	defer {
		sock.close() or {}
	}
	for enabled in [false, true, false] {
		sock.set_option_bool(.broadcast, enabled)!
		actual := read_socket_option(sock.handle, .broadcast)!
		assert (actual != 0) == enabled
	}
	sock.set_option_int(.send_buf_size, 32768)!
	assert read_socket_option(sock.handle, .send_buf_size)! >= 32768
}

fn test_raw_ip_header_option_rejects_invalid_socket() {
	mut sock := RawSocket{
		handle: -1
	}
	for enabled in [false, true] {
		sock.set_ip_header_included(enabled) or {
			assert err.code() != 0
			continue
		}
		assert false, 'setting an option on an invalid socket must fail'
	}
}
