module net

import time

fn tcp_socket_receive_timeout(handle int) !time.Duration {
	mut value := C.timeval{}
	mut size := u32(sizeof(value))
	socket_error(C.getsockopt(handle, C.SOL_SOCKET, C.SO_RCVTIMEO, voidptr(&value), &size))!
	return time.Duration(value.tv_sec) * time.second + time.Duration(value.tv_usec) * time.microsecond
}

fn test_tcp_blocking_read_uses_socket_timeout() ! {
	$if windows || is_coroutine ? {
		return
	}
	mut listener := listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut client := dial_tcp(listener.addr()!.str())!
	defer { client.close() or {} }
	mut server := listener.accept()!
	defer { server.close() or {} }
	assert client.read_timeout_in_socket
	assert server.read_timeout_in_socket
	assert tcp_socket_receive_timeout(client.sock.handle)! == tcp_default_read_timeout
	assert tcp_socket_receive_timeout(server.sock.handle)! == tcp_default_read_timeout

	server.set_read_timeout(200 * time.millisecond)
	assert tcp_socket_receive_timeout(server.sock.handle)! >= 200 * time.millisecond
	mut buffer := []u8{len: 8}
	started := time.new_stopwatch()
	server.read(mut buffer) or {
		assert err.code() == err_timed_out.code()
		assert started.elapsed() >= 100 * time.millisecond
		assert started.elapsed() < 2 * time.second
		client.write_string('ok')!
		assert server.read(mut buffer)! == 2
		assert buffer[..2].bytestr() == 'ok'
		for timeout in [no_timeout, infinite_timeout] {
			server.set_read_timeout(timeout)
			assert server.read_timeout_in_socket
			assert tcp_socket_receive_timeout(server.sock.handle)! == 0
		}
		server.set_read_timeout(no_timeout)
		server.set_read_deadline(time.now().add(50 * time.millisecond))
		server.read(mut buffer) or {
			assert err.code() == err_timed_out.code()
			return
		}
		assert false, 'a deadline-only read must time out'
		return
	}
	assert false, 'a silent peer must time out'
}
