import os
import time
import net
import net.unix

// A read deadline that was set wins over the connection's read timeout, as in net. In its own
// file: unix_socket_test.v is on the test runner's flaky list, which ignores its failures.
fn test_a_read_deadline_wins_over_the_timeout() ! {
	path := os.join_path(os.vtmp_dir(), 'v_unix_deadline_${os.getpid()}.sock')
	os.rm(path) or {}
	mut l := unix.listen_stream(path)!
	defer {
		l.close() or {}
		os.rm(path) or {}
	}
	mut c := unix.connect_stream(path)!
	defer {
		c.close() or {}
	}
	mut server := l.accept()!
	defer {
		server.close() or {}
	}
	server.set_read_timeout(100 * time.millisecond)
	server.set_read_deadline(time.now().add(400 * time.millisecond))
	sw := time.new_stopwatch()
	server.wait_for_read() or {}
	assert sw.elapsed() >= 350 * time.millisecond, 'returned after ${sw.elapsed()}'
}

fn unix_connect_after_deadline_delay(path string) {
	time.sleep(100 * time.millisecond)
	mut client := unix.connect_stream(path) or { panic(err) }
	client.close() or { panic(err) }
}

fn test_epoch_subsecond_accept_deadlines_are_due() ! {
	for nanosecond in [1, 999999999] {
		path := os.join_path(os.vtmp_dir(), 'v_unix_epoch_${os.getpid()}_${nanosecond}.sock')
		os.rm(path) or {}
		mut listener := unix.listen_stream(path)!
		defer {
			listener.close() or {}
			os.rm(path) or {}
		}
		listener.set_accept_deadline(time.unix_nanosecond(0, nanosecond))
		// Wake an incorrectly infinite wait so a regression fails without hanging.
		connector := spawn unix_connect_after_deadline_delay(path)
		mut timed_out := false
		listener.wait_for_accept() or {
			assert err.code() == net.err_timed_out.code()
			timed_out = true
		}
		connector.wait()
		assert timed_out, 'expired deadline waited for a connection'
	}
}

fn test_unset_accept_deadlines_allow_waiting_without_a_timeout() ! {
	for i, deadline in [time.Time{}, time.unix(0)] {
		path := os.join_path(os.vtmp_dir(), 'v_unix_unset_${os.getpid()}_${i}.sock')
		os.rm(path) or {}
		mut listener := unix.listen_stream(path)!
		defer {
			listener.close() or {}
			os.rm(path) or {}
		}
		listener.set_accept_timeout(net.no_timeout)
		listener.set_accept_deadline(deadline)
		connector := spawn unix_connect_after_deadline_delay(path)
		listener.wait_for_accept()!
		connector.wait()
	}
}

fn unix_write_after_deadline_delay(path string) {
	time.sleep(100 * time.millisecond)
	mut client := unix.connect_stream(path) or { panic(err) }
	defer {
		client.close() or {}
	}
	time.sleep(100 * time.millisecond)
	client.write_string('future') or { panic(err) }
}

fn test_far_future_unix_deadlines_allow_delayed_readiness() ! {
	deadline := time.parse_iso8601('9999-12-31T23:59:59Z')!
	path := os.join_path(os.vtmp_dir(), 'v_unix_future_${os.getpid()}.sock')
	os.rm(path) or {}
	mut listener := unix.listen_stream(path)!
	defer {
		listener.close() or {}
		os.rm(path) or {}
	}
	listener.set_accept_timeout(20 * time.millisecond)
	listener.set_accept_deadline(deadline)
	writer := spawn unix_write_after_deadline_delay(path)
	listener.wait_for_accept()!
	mut server := listener.accept()!
	defer {
		server.close() or {}
	}
	server.set_read_timeout(20 * time.millisecond)
	server.set_read_deadline(deadline)
	server.wait_for_read()!
	mut buf := []u8{len: 16}
	read := server.read(mut buf)!
	writer.wait()
	assert buf[..read].bytestr() == 'future'
	assert listener.accept_deadline()! == deadline
	assert server.read_deadline()! == deadline
}
