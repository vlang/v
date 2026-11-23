import sync

fn test_spinlock() {
	mut counter := 0
	mut s := sync.new_spin_lock()
	defer {
		s.destroy()
	}
	num_threads := 10
	iterations := 10
	mut wg := sync.new_waitgroup()
	wg.add(num_threads)
	attempts := chan bool{cap: num_threads}

	s.lock()
	for _ in 0 .. num_threads {
		spawn fn (mut wg sync.WaitGroup, s &sync.SpinLock, counter_ref &int, iterations int, attempts chan bool) {
			acquired := s.try_lock()
			if acquired {
				s.unlock()
			}
			attempts <- acquired
			for _ in 0 .. iterations {
				s.lock()
				unsafe {
					// The lock protects the shared counter.
					(*counter_ref) = *counter_ref + 1
				}
				s.unlock()
			}
			wg.done()
		}(mut wg, s, &counter, iterations, attempts)
	}
	mut all_blocked := true
	for _ in 0 .. num_threads {
		acquired := <-attempts
		all_blocked = all_blocked && !acquired
	}
	s.unlock()
	wg.wait()
	assert all_blocked
	assert counter == num_threads * iterations

	s.lock()
	acquired_while_locked := s.try_lock()
	s.unlock()
	assert !acquired_while_locked
	acquired := s.try_lock()
	assert acquired
	acquired_again := s.try_lock()
	s.unlock()
	assert !acquired_again
}
