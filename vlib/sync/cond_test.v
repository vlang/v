import sync
import sync.stdatomic { new_atomic }

// Test single thread wake-up scenario for condition variable
fn test_single_thread_wakeup() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	mut done := new_atomic(false)
	mut wake_count := new_atomic(0)
	ready_ch := chan bool{cap: 1}
	done_ch := chan bool{cap: 1}
	defer {
		ready_ch.close()
		done_ch.close()
	}

	// Spawn waiting thread
	spawn fn [mut done, mut wake_count, mut cond, mut mutex, ready_ch, done_ch] () {
		mutex.lock()
		defer {
			mutex.unlock()
		}
		ready_ch <- true // Notify main thread of readiness

		// Wait loop for conditional signals
		for !done.load() {
			cond.wait()
			wake_count.add(1)
		}
		done_ch <- true // Notify completion
	}()

	// Wait for the worker to enter waiting state
	_ := <-ready_ch

	// Trigger signaling sequence
	mutex.lock()
	cond.signal() // Wake the waiting thread
	done.store(true) // Terminate worker loop
	cond.signal() // Extra signal for loop exit
	mutex.unlock()

	// Verify result
	_ := <-done_ch
	assert wake_count.load() == 1, 'Should wake exactly 1 thread'
}

// Test broadcast wake-up of multiple waiting threads
fn test_broadcast_wakeup() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	num_threads := 5
	mut wake_counter := new_atomic(0)
	ready_ch := chan bool{cap: num_threads}
	done_ch := chan bool{cap: num_threads}
	defer {
		ready_ch.close()
		done_ch.close()
	}

	// Spawn multiple waiting threads
	for _ in 0 .. num_threads {
		spawn fn [mut wake_counter, mut cond, mut mutex, ready_ch, done_ch] () {
			mutex.lock()
			defer {
				mutex.unlock()
			}
			ready_ch <- true // Notify readiness
			cond.wait() // Wait for broadcast
			wake_counter.add(1)
			done_ch <- true // Notify completion
		}()
	}

	// Wait for all threads to enter waiting state
	for _ in 0 .. num_threads {
		_ := <-ready_ch
	}

	// Trigger broadcast wake-up
	mutex.lock()
	cond.broadcast()
	mutex.unlock()

	// Verify all threads completed
	for _ in 0 .. num_threads {
		_ := <-done_ch
	}
	assert wake_counter.load() == num_threads, 'Should wake all threads'
}

// Test consecutive signal delivery sequencing
fn test_multiple_signals() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	mut counter := new_atomic(0)
	num_signals := 3
	ready_ch := chan bool{cap: 1}
	wait_sync_ch := chan bool{cap: 1} // Synchronization for wait-sequence tracking
	done_ch := chan bool{cap: 1}
	defer {
		ready_ch.close()
		wait_sync_ch.close()
		done_ch.close()
	}

	spawn fn [num_signals, mut counter, mut cond, mut mutex, ready_ch, wait_sync_ch, done_ch] () {
		mutex.lock()
		defer {
			mutex.unlock()
		}

		ready_ch <- true // Initial readiness notification

		// Process multiple signals sequentially
		for _ in 0 .. num_signals {
			cond.wait()
			counter.add(1)
			wait_sync_ch <- true // Signal processing complete
		}
		done_ch <- true
	}()

	// Wait for initial setup
	_ := <-ready_ch

	// Send first signal
	mutex.lock()
	cond.signal()
	mutex.unlock()

	// Send subsequent signals with synchronization
	for _ in 1 .. num_signals {
		_ := <-wait_sync_ch // Wait for previous signal processing
		mutex.lock()
		cond.signal()
		mutex.unlock()
	}

	_ := <-done_ch
	assert counter.load() == num_signals, 'Signal count should match counter value'
}

// Test lock reacquisition mechanics after wait()
fn test_lock_reacquire() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)
	mut lock_held := new_atomic(false)
	ready_ch := chan bool{cap: 1}
	done_ch := chan bool{cap: 1}
	defer {
		ready_ch.close()
		done_ch.close()
	}

	spawn fn [mut lock_held, mut cond, mut mutex, ready_ch, done_ch] () {
		mutex.lock()
		defer {
			mutex.unlock()
		}
		ready_ch <- true

		cond.wait()
		// Test lock state after wakeup
		lock_held.store(!mutex.try_lock()) // Should fail -> store true
		done_ch <- true
	}()

	_ := <-ready_ch

	mutex.lock()
	cond.signal()
	mutex.unlock()

	_ := <-done_ch
	assert lock_held.load(), 'Mutex should be reacquired automatically after wait()'
}

// Test empty signal/broadcast scenario
fn test_signal_without_waiters() {
	mut mutex := sync.new_mutex()
	mut cond := sync.new_cond(mutex)

	// Verify no panic occurs
	mutex.lock()
	cond.signal() // No-op with no waiters
	cond.broadcast() // No-op with no waiters
	mutex.unlock()

	assert true, 'Should handle empty signal operations safely'
}
