import sync
import rand
import time

fn test_spinlock() {
	mut counter := 0
	mut s := sync.new_spin_lock()
	num_threads := 10
	iterations := 10
	mut wg := sync.new_waitgroup()
	wg.add(num_threads)

	for _ in 0 .. num_threads {
		spawn fn (mut wg sync.WaitGroup, s &sync.SpinLock, counter_ref &int, iterations int) {
			for _ in 0 .. iterations {
				s.lock()

				unsafe {
					tmp := *counter_ref
					randval := rand.intn(100) or { 1 }
					time.sleep(randval * time.nanosecond)

					(*counter_ref) = tmp + 1
				}
				s.unlock()
			}
			wg.done()
		}(mut wg, s, &counter, iterations)
	}
	wg.wait()
	assert counter == num_threads * iterations
}
