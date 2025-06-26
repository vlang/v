import sync

fn test_spinlock() {
	mut counter := 0
	mut s := sync.new_spin_lock()
	num_threads := 10
	mut wg := sync.new_waitgroup()
	wg.add(num_threads)

	for _ in 0 .. num_threads {
		spawn fn (mut wg sync.WaitGroup, s &sync.SpinLock, counter_ref &int) {
			defer {
				s.unlock()
				wg.done()
			}
			s.lock()
			(*counter_ref)++
		}(mut wg, s, &counter)
	}
	wg.wait()
	assert counter == num_threads
}
