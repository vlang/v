module net

import time

fn check_connection_deadline_getters[T](mut conn T) ! {
	for i, deadline in [time.Time{}, time.unix(0), time.unix(-1), time.unix_nanosecond(0, 1),
		time.now().add(time.second)] {
		conn.set_read_deadline(deadline)
		conn.set_write_deadline(deadline)
		if i < 2 {
			if actual := conn.read_deadline() {
				assert false, 'unset read deadline returned ${actual}'
			}
			if actual := conn.write_deadline() {
				assert false, 'unset write deadline returned ${actual}'
			}
		} else {
			assert conn.read_deadline()! == deadline
			assert conn.write_deadline()! == deadline
		}
	}
}

fn test_connection_deadline_getters_agree_with_wait_state() ! {
	mut tcp := TcpConn{}
	mut udp := UdpConn{}
	mut raw := RawConn{}
	check_connection_deadline_getters(mut tcp)!
	check_connection_deadline_getters(mut udp)!
	check_connection_deadline_getters(mut raw)!
}

fn test_accept_deadline_getter_agrees_with_wait_state() ! {
	mut listener := TcpListener{}
	for i, deadline in [time.Time{}, time.unix(0), time.unix(-1), time.unix_nanosecond(0, 1),
		time.now().add(time.second)] {
		listener.set_accept_deadline(deadline)
		if i < 2 {
			if actual := listener.accept_deadline() {
				assert false, 'unset accept deadline returned ${actual}'
			}
		} else {
			assert listener.accept_deadline()! == deadline
		}
	}
}

fn send_after_deadline_chunk_delay(addr string) {
	time.sleep(50 * time.millisecond)
	mut sender := dial_udp(addr) or { panic(err) }
	defer {
		sender.close() or {}
	}
	sender.write_string('chunked') or { panic(err) }
}

fn test_select_deadline_retries_chunks_and_preserves_unset_sentinels() ! {
	future := time.parse_iso8601('9999-12-31T23:59:59Z')!
	for deadline in [time.Time{}, time.unix(0), future] {
		mut listener := listen_udp('127.0.0.1:0')!
		defer {
			listener.close() or {}
		}
		writer := spawn send_after_deadline_chunk_delay(listener.sock.address()!.str())
		ready := select_deadline_chunked(listener.sock.handle, .read, deadline,
			5 * time.millisecond)!
		writer.wait()
		assert ready
		mut buf := []u8{len: 16}
		read, _ := listener.read(mut buf)!
		assert buf[..read].bytestr() == 'chunked'
	}
}

fn test_select_deadline_chunks_still_expire_at_the_real_deadline() ! {
	mut listener := listen_udp('127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	writer := spawn send_after_deadline_chunk_delay(listener.sock.address()!.str())
	ready := select_deadline_chunked(listener.sock.handle, .read,
		time.now().add(20 * time.millisecond), 5 * time.millisecond) or {
		assert err.code() == err_timed_out.code()
		false
	}
	writer.wait()
	assert !ready, 'wait continued past its finite deadline until data arrived'
}
