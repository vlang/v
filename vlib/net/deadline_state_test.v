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
