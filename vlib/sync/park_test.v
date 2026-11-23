module sync

import sync.stdatomic { new_atomic }
import time

fn wait_for_registered_park(mut p Parker, tid u64, done chan bool) ! {
	sw := time.new_stopwatch()
	for sw.elapsed() < 2 * time.second {
		p.mutex.lock()
		registered := tid in p.waiters
		p.mutex.unlock()
		select {
			_ := <-done {
				return error('park returned before unpark')
			}
			else {
			}
		}
		if registered {
			return
		}
		time.sleep(time.millisecond)
	}
	return error('park did not register a waiter')
}

fn wait_for_park_return(done chan bool) ! {
	select {
		_ := <-done {
			return
		}
		2 * time.second {
		}
	}
	return error('park did not return after unpark')
}

// A parked worker must stay blocked until unparked, then proceed.
fn test_parker_unpark_wakes_worker() ! {
	mut p := new_parker()
	tid_ch := chan u64{cap: 1}
	done_ch := chan bool{cap: 1}
	worker := spawn fn [mut p, tid_ch, done_ch] () {
		tid_ch <- thread_id()
		p.park()
		done_ch <- true
	}()

	worker_tid := <-tid_ch
	wait_for_registered_park(mut p, worker_tid, done_ch)!
	p.unpark(worker_tid)
	wait_for_park_return(done_ch)!
	worker.wait()
}

// Unpark-before-park must return immediately (stored permit, no lost wakeup).
fn test_parker_unpark_before_park_returns_immediately() {
	mut p := new_parker()
	p.unpark(thread_id())
	// If the permit was lost, this park would hang the test suite.
	p.park()
	assert true, 'park after unpark must return immediately'
}

// Multiple parked workers are each woken by their own unpark.
fn test_parker_unpark_multiple_workers() {
	mut p := new_parker()
	n := 4
	tid_ch := chan u64{cap: n}
	done_ch := chan bool{cap: n}
	mut finished := new_atomic(0)
	defer {
		tid_ch.close()
		done_ch.close()
	}
	for _ in 0 .. n {
		spawn fn [mut p, mut finished, tid_ch, done_ch] () {
			tid_ch <- thread_id()
			p.park()
			finished.add(1)
			done_ch <- true
		}()
	}
	for _ in 0 .. n {
		tid := <-tid_ch
		p.unpark(tid)
	}
	for _ in 0 .. n {
		_ := <-done_ch
	}
	assert finished.load() == n, 'all workers must be woken'
}

fn test_parker_repeated_early_unparks_retain_one_permit() ! {
	mut p := new_parker()
	tid_ch := chan u64{cap: 1}
	start := chan bool{cap: 1}
	done := chan bool{cap: 2}
	worker := spawn fn [mut p, tid_ch, start, done] () {
		tid_ch <- thread_id()
		_ := <-start
		for _ in 0 .. 2 {
			p.park()
			done <- true
		}
	}()
	tid := <-tid_ch
	p.unpark(tid)
	p.unpark(tid)
	start <- true
	wait_for_park_return(done)!
	// The first park consumed the single permit. The second must register.
	wait_for_registered_park(mut p, tid, done)!
	p.unpark(tid)
	wait_for_park_return(done)!
	worker.wait()
}

fn test_parker_reuses_a_worker_id_for_multiple_cycles() {
	mut p := new_parker()
	tid_ch := chan u64{cap: 1}
	done_ch := chan bool{}
	defer { tid_ch.close() }
	defer { done_ch.close() }
	mut worker := spawn fn [mut p, tid_ch, done_ch] () {
		tid_ch <- thread_id()
		for _ in 0 .. 20 {
			p.park()
			done_ch <- true
		}
	}()
	tid := <-tid_ch
	for _ in 0 .. 20 {
		p.unpark(tid)
		// the receive must not be inside the `assert`: `-prod` drops asserts, and the worker
		// would then block for ever in `done_ch <- true`
		got := <-done_ch
		assert got
	}
	worker.wait()
}
