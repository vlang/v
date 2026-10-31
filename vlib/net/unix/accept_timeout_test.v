import os
import time
import net.unix

// unblock_accept_later connects a client after a delay, so that a broken accept() that ignores
// its timeout returns instead of hanging the whole test run.
fn unblock_accept_later(path string) {
	time.sleep(3 * time.second)
	mut client := unix.connect_stream(path) or { return }
	client.close() or {}
}

fn test_accept_honours_the_accept_timeout() ! {
	path := os.join_path(os.vtmp_dir(), 'v_unix_accept_timeout_${os.getpid()}.sock')
	os.rm(path) or {}
	mut l := unix.listen_stream(path)!
	defer {
		l.close() or {}
		os.rm(path) or {}
	}
	l.set_accept_timeout(200 * time.millisecond)
	spawn unblock_accept_later(path)
	sw := time.new_stopwatch()
	mut accepted := true
	l.accept() or { accepted = false }
	elapsed := sw.elapsed()
	assert !accepted, 'accept() returned a connection, but no client was connecting'
	assert elapsed < 2 * time.second, 'accept() returned after ${elapsed}, expected about 200ms'
}

fn test_accept_honours_the_accept_deadline() ! {
	path := os.join_path(os.vtmp_dir(), 'v_unix_accept_deadline_${os.getpid()}.sock')
	os.rm(path) or {}
	mut l := unix.listen_stream(path)!
	defer {
		l.close() or {}
		os.rm(path) or {}
	}
	l.set_accept_deadline(time.now().add(200 * time.millisecond))
	spawn unblock_accept_later(path)
	sw := time.new_stopwatch()
	mut accepted := true
	l.accept() or { accepted = false }
	elapsed := sw.elapsed()
	assert !accepted, 'accept() returned a connection, but no client was connecting'
	assert elapsed < 2 * time.second, 'accept() returned after ${elapsed}, expected about 200ms'
}
