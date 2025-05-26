import sync
import sync.stdatomic { new_atomic }
import time

// Test single thread wake-up
fn test_single_thread_wakeup() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	mut done := new_atomic(false)
	mut wake_count := new_atomic(0)

	spawn fn [mut done, mut wake_count, mut cond, mut mutex] () {
		mutex.lock()
		defer {
			mutex.unlock()
		}

		for !done.load() {
			cond.wait() // Wait for signal
			wake_count.add(1)
		}
	}()

	// Ensure waiter has entered waiting state
	time.sleep(100 * time.millisecond)

	// Send signal
	mutex.lock()
	cond.signal()
	done.store(true)
	cond.signal() // signal again to ensure thread exit
	mutex.unlock()

	// Wait for result
	time.sleep(100 * time.millisecond)
	assert wake_count.load() == 1, 'Should wake 1 thread'
}

// Test broadcast wake-up of multiple waiters
fn test_broadcast_wakeup() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	num_threads := 5
	mut wake_counter := new_atomic(0)

	// Start multiple waiters
	for _ in 0 .. num_threads {
		spawn fn [mut wake_counter, mut cond, mut mutex] () {
			mutex.lock()
			defer {
				mutex.unlock()
			}
			cond.wait()
			wake_counter.add(1)
		}()
	}

	// Wait for all threads to enter waiting state
	time.sleep(300 * time.millisecond)

	// Broadcast wake-up
	mutex.lock()
	cond.broadcast()
	mutex.unlock()

	// Verify results
	time.sleep(500 * time.millisecond)
	assert wake_counter.load() == num_threads, 'Should wake all threads'
}

// Test multiple consecutive signals
fn test_multiple_signals() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	mut counter := new_atomic(0)
	num_signals := 3

	spawn fn [num_signals, mut counter, mut cond, mut mutex] () {
		mutex.lock()
		defer {
			mutex.unlock()
		}

		for _ in 0 .. num_signals {
			cond.wait()
			counter.add(1)
		}
	}()

	time.sleep(100 * time.millisecond)

	// Send multiple signals
	for _ in 0 .. num_signals {
		mutex.lock()
		cond.signal()
		mutex.unlock()
		time.sleep(50 * time.millisecond)
	}

	time.sleep(300 * time.millisecond)
	assert counter.load() == num_signals, 'Should match signal count'
}

// Test lock reacquisition correctness
fn test_lock_reaquire() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	mut lock_held := new_atomic(false)

	spawn fn [mut lock_held, mut cond, mut mutex] () {
		mutex.lock()
		cond.wait()
		lock_held.store(!mutex.try_lock()) // Test lock state
		mutex.unlock()
	}()

	time.sleep(100 * time.millisecond)

	mutex.lock()
	cond.signal()
	mutex.unlock()

	time.sleep(100 * time.millisecond)
	assert lock_held.load(), 'Lock should be properly reacquired'
}

// Test signaling without waiters
fn test_signal_without_waiters() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)

	// Empty signals should not crash
	mutex.lock()
	cond.signal()
	cond.broadcast()
	mutex.unlock()

	assert true, 'Should handle empty signal gracefully'
}

fn test_cond_wait_for() {
	mut m := sync.new_mutex()
	mut cond := sync.new_cond(m)

	// case1: timeout test
	mut start := time.now()
	m.lock()
	mut ret := cond.wait_for(5 * time.millisecond)
	m.unlock()
	mut duration := time.now() - start

	assert !ret
	assert duration >= 4 * time.millisecond
	assert duration < 50 * time.millisecond

	// case2: signal before timeout
	mut ready := sync.new_semaphore()
	spawn fn [mut cond, mut ready] () {
		ready.wait()
		time.sleep(5 * time.millisecond)
		cond.signal()
	}()
	start = time.now()
	m.lock()
	ready.post()
	ret = cond.wait_for(20 * time.millisecond)
	m.unlock()
	duration = time.now() - start

	assert ret
	assert duration >= 4 * time.millisecond
	assert duration < 50 * time.millisecond

	// case3: parallel threads timeout
	mut completed := new_atomic(0)
	mut started := new_atomic(0)
	total := 5
	for i in 0 .. total {
		spawn fn [mut m, mut cond, mut completed, mut started] (i int) {
			m.lock()
			defer { m.unlock() }
			started.add(1)
			if i % 2 == 0 {
				// should got signal before timeout
				cond.wait_for(100 * time.millisecond)
				completed.add(1)
			} else {
				// should timeout
				ret := cond.wait_for(10 * time.millisecond)
				if !ret {
					completed.add(1)
				}
			}
		}(i)
	}
	for started.load() < total {
		time.sleep(1 * time.millisecond)
	}
	time.sleep(20 * time.millisecond)
	cond.broadcast() // wakeup all threads
	time.sleep(50 * time.millisecond)

	assert completed.load() == total
}
