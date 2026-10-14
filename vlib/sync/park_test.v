import sync
import sync.stdatomic { new_atomic }
import time

// A parked worker must stay blocked until unparked, then proceed.
fn test_parker_unpark_wakes_worker() {
	mut p := sync.new_parker()
	tid_ch := chan u64{cap: 1}
	done_ch := chan bool{cap: 1}
	mut proceeded := new_atomic(false)
	defer {
		tid_ch.close()
		done_ch.close()
	}
	spawn fn [mut p, mut proceeded, tid_ch, done_ch] () {
		tid_ch <- sync.thread_id()
		p.park()
		proceeded.store(true)
		done_ch <- true
	}()

	worker_tid := <-tid_ch
	time.sleep(50 * time.millisecond)
	assert proceeded.load() == false, 'worker must stay parked until unparked'
	p.unpark(worker_tid)
	_ := <-done_ch
	assert proceeded.load() == true, 'worker must proceed after unpark'
}

// Unpark-before-park must return immediately (stored permit, no lost wakeup).
fn test_parker_unpark_before_park_returns_immediately() {
	mut p := sync.new_parker()
	p.unpark(sync.thread_id())
	// If the permit was lost, this park would hang the test suite.
	p.park()
	assert true, 'park after unpark must return immediately'
}

// Multiple parked workers are each woken by their own unpark.
fn test_parker_unpark_multiple_workers() {
	mut p := sync.new_parker()
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
			tid_ch <- sync.thread_id()
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

fn test_parker_repeated_early_unparks_retain_one_permit() {
	mut p := sync.new_parker()
	tid := sync.thread_id()
	p.unpark(tid)
	p.unpark(tid)
	p.park()
	// The second park must wait for a new unpark.
	mut second_returned := new_atomic(false)
	mut waker := spawn fn [mut p, tid, mut second_returned] () {
		time.sleep(20 * time.millisecond)
		assert !second_returned.load(), 'early unparks must coalesce'
		p.unpark(tid)
	}()
	p.park()
	second_returned.store(true)
	waker.wait()
}

fn test_parker_reuses_a_worker_id_for_multiple_cycles() {
	mut p := sync.new_parker()
	tid_ch := chan u64{cap: 1}
	done_ch := chan bool{}
	defer { tid_ch.close() }
	defer { done_ch.close() }
	mut worker := spawn fn [mut p, tid_ch, done_ch] () {
		tid_ch <- sync.thread_id()
		for _ in 0 .. 20 {
			p.park()
			done_ch <- true
		}
	}()
	tid := <-tid_ch
	for _ in 0 .. 20 {
		p.unpark(tid)
		assert <-done_ch
	}
	worker.wait()
}
