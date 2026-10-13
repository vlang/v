import sync
import sync.stdatomic { new_atomic }
import time

// A parked worker must stay blocked until unparked, then proceed.
fn test_park_unpark_wakes_worker() {
	tid_ch := chan u64{cap: 1}
	done_ch := chan bool{cap: 1}
	mut proceeded := new_atomic(false)
	defer {
		tid_ch.close()
		done_ch.close()
	}
	spawn fn [mut proceeded, tid_ch, done_ch] () {
		tid_ch <- sync.thread_id()
		sync.park()
		proceeded.store(true)
		done_ch <- true
	}()

	worker_tid := <-tid_ch
	time.sleep(50 * time.millisecond)
	assert proceeded.load() == false, 'worker must stay parked until unparked'
	sync.unpark(worker_tid)
	_ := <-done_ch
	assert proceeded.load() == true, 'worker must proceed after unpark'
}

// Unpark-before-park must return immediately (stored permit, no lost wakeup).
fn test_unpark_before_park_returns_immediately() {
	sync.unpark(sync.thread_id())
	// If the permit was lost, this park would hang the test suite.
	sync.park()
	assert true, 'park after unpark must return immediately'
}

// Multiple parked workers are each woken by their own unpark.
fn test_park_unpark_multiple_workers() {
	n := 4
	tid_ch := chan u64{cap: n}
	done_ch := chan bool{cap: n}
	mut finished := new_atomic(0)
	defer {
		tid_ch.close()
		done_ch.close()
	}
	for _ in 0 .. n {
		spawn fn [mut finished, tid_ch, done_ch] () {
			tid_ch <- sync.thread_id()
			sync.park()
			finished.add(1)
			done_ch <- true
		}()
	}
	for _ in 0 .. n {
		tid := <-tid_ch
		sync.unpark(tid)
	}
	for _ in 0 .. n {
		_ := <-done_ch
	}
	assert finished.load() == n, 'all workers must be woken'
}
