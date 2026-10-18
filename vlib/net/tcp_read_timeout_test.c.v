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
			client.write_string('ok')!
			assert server.read(mut buffer)! == 2
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

fn test_tcp_read_deadline_wins_over_the_socket_timeout() ! {
	$if windows || is_coroutine ? {
		return
	}
	mut listener := listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	mut client := dial_tcp(listener.addr()!.str())!
	defer { client.close() or {} }
	mut server := listener.accept()!
	defer { server.close() or {} }
	// the default read timeout (30 s) is in the socket; a deadline set is what the read waits for
	server.set_read_deadline(time.now().add(200 * time.millisecond))
	mut buffer := []u8{len: 8}
	started := time.new_stopwatch()
	server.read(mut buffer) or {
		assert err.code() == err_timed_out.code()
		assert started.elapsed() < 2 * time.second, 'returned after ${started.elapsed()}'
		return
	}
	assert false, 'a silent peer must time out'
}

fn test_tcp_accept_deadline_wins_over_the_infinite_accept_timeout() ! {
	$if windows || is_coroutine ? {
		return
	}
	mut listener := listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	// the default accept timeout is infinite; a deadline set ends the wait
	listener.set_accept_deadline(time.now().add(200 * time.millisecond))
	started := time.new_stopwatch()
	listener.wait_for_accept() or {
		assert err.code() == err_timed_out.code()
		assert started.elapsed() < 2 * time.second, 'returned after ${started.elapsed()}'
		return
	}
	assert false, 'nobody connected'
}

fn test_a_deadline_before_1970_is_set_and_due() ! {
	assert !deadline_set(time.Time{})
	assert !deadline_set(time.unix(0)) // select_deadline reads it as infinite
	assert deadline_set(time.unix(-1))
	assert deadline_set(time.unix_nanosecond(0, 1)) // the same second as the sentinel
	$if windows || is_coroutine ? {
		return
	}
	mut listener := listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	listener.set_accept_deadline(time.unix(-1))
	started := time.new_stopwatch()
	listener.wait_for_accept() or {
		assert err.code() == err_timed_out.code()
		assert started.elapsed() < time.second, 'returned after ${started.elapsed()}'
		return
	}
	assert false, 'nobody connected'
}

fn tcp_connect_after_deadline_delay(address string) {
	time.sleep(100 * time.millisecond)
	mut client := dial_tcp(address) or { panic(err) }
	client.close() or { panic(err) }
}

fn test_epoch_subsecond_accept_deadlines_are_due() ! {
	for nanosecond in [1, 999999999] {
		mut listener := listen_tcp(.ip, '127.0.0.1:0')!
		defer { listener.close() or {} }
		listener.set_accept_deadline(time.unix_nanosecond(0, nanosecond))
		// Wake an incorrectly infinite wait so a regression fails without hanging.
		connector := spawn tcp_connect_after_deadline_delay(listener.addr()!.str())
		mut timed_out := false
		listener.wait_for_accept() or {
			assert err.code() == err_timed_out.code()
			timed_out = true
		}
		connector.wait()
		assert timed_out, 'expired deadline waited for a connection'
	}
}

fn test_unset_accept_deadlines_allow_waiting_without_a_timeout() ! {
	for deadline in [time.Time{}, time.unix(0)] {
		mut listener := listen_tcp(.ip, '127.0.0.1:0')!
		defer { listener.close() or {} }
		listener.set_accept_timeout(no_timeout)
		listener.set_accept_deadline(deadline)
		connector := spawn tcp_connect_after_deadline_delay(listener.addr()!.str())
		listener.wait_for_accept()!
		connector.wait()
	}
}
