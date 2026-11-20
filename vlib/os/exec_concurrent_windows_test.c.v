module os

import time

// run_at starts a child at a shared deadline and reports how long the call took.
fn run_at(args []string, go_at time.Time) i64 {
	for time.now() < go_at {
	}
	sw := time.new_stopwatch()
	exec(args)
	return sw.elapsed().milliseconds()
}

fn test_exec_does_not_wait_for_unrelated_child() {
	// CreateProcess with bInheritHandles = TRUE gives a child every inheritable
	// handle open in the parent at that moment. Without serialising the
	// CreatePipe/CreateProcess window, a concurrent spawn copies another
	// thread's pipe write end, and its caller cannot see EOF until the
	// unrelated child exits. A fast call must stay fast regardless.
	// The slow child runs about ten seconds; without the lock a fast call is
	// held for its whole lifetime. The generous threshold keeps the test stable
	// on a loaded CI runner while still catching the multi-second stall.
	go_at := time.now().add(200 * time.millisecond)
	slow := spawn run_at(['ping', '-n', '11', '127.0.0.1'], go_at)
	mut fast := []thread i64{}
	for _ in 0 .. 4 {
		fast << spawn run_at(['cmd', '/d', '/c', 'echo', 'hi'], go_at)
	}
	ms := fast.wait()
	slow.wait()
	for m in ms {
		assert m < 5000, 'os.exec waited ${m} ms for an unrelated slow child: ${ms}'
	}
}
