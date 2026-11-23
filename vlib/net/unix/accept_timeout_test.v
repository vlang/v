import os
import time
import net
import net.unix

fn testsuite_begin() {
	// Relative socket names stay within sun_path even when the CI scratch path is long.
	os.chdir(os.vtmp_dir()) or { panic(err) }
}

fn unblock_accept_if_stuck(path string, done chan bool) {
	// Release accept if it ignored the configured timeout or deadline.
	select {
		_ := <-done {
			return
		}
		3 * time.second {
		}
	}
	mut client := unix.connect_stream(path) or { return }
	client.close() or {}
}

fn test_accept_honours_the_accept_timeout() ! {
	check_accept_timeout(false)!
}

fn test_accept_honours_the_accept_deadline() ! {
	check_accept_timeout(true)!
}

fn check_accept_timeout(use_deadline bool) ! {
	path := 'v_unix_accept_${use_deadline}_${os.getpid()}.sock'
	os.rm(path) or {}
	mut l := unix.listen_stream(path)!
	defer {
		l.close() or {}
		os.rm(path) or {}
	}
	done := chan bool{cap: 1}
	watchdog := spawn unblock_accept_if_stuck(path, done)
	timeout := 25 * time.millisecond
	sw := time.new_stopwatch()
	if use_deadline {
		l.set_accept_deadline(time.now().add(timeout))
	} else {
		l.set_accept_timeout(timeout)
	}
	timed_out := if mut conn := l.accept() {
		conn.close() or {}
		false
	} else {
		err.code() == net.err_timed_out_code
	}
	elapsed := sw.elapsed()
	done <- true
	watchdog.wait()
	assert timed_out, 'expected accept() to time out'
	// Windows timers tick about every 15.6 ms, so a wait may end up to one tick early
	// (a 25 ms timeout returned after 18 ms on CI).
	min_elapsed := $if windows { timeout - 16 * time.millisecond } $else { 20 * time.millisecond }
	assert elapsed >= min_elapsed && elapsed < 2 * time.second, 'accept() returned after ${elapsed}, expected about ${timeout}'
}
