import os
import time
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
