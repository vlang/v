import sync
import datatypes.lockfree

fn test_counter() {
	number_threads := 10
	mut counter := lockfree.new_counter(u64(0))
	mut wg := sync.new_waitgroup()
	for i in 0 .. number_threads {
		wg.add(1)
		spawn fn (mut c lockfree.Counter[u64], id int, mut wg sync.WaitGroup) {
			for j in 0 .. 1000 {
				c.increment()
			}
			wg.done()
		}(mut counter, i, mut wg)
	}

	for i in 0 .. number_threads {
		wg.add(1)
		spawn fn (mut c lockfree.Counter[u64], id int, mut wg sync.WaitGroup) {
			for j in 0 .. 1000 {
				c.decrement()
			}
			wg.done()
		}(mut counter, i, mut wg)
	}

	wg.wait()
	assert counter.get() == u64(0)

	counter.increment_by(100)
	assert counter.get() == u64(100)
	counter.decrement_by(100)
	assert counter.get() == u64(0)

	counter.increment_by(1024)
	counter.clear()
	assert counter.get() == u64(0)

	mut counter_init := lockfree.new_counter(u64(100))
	assert counter_init.get() == u64(100)
}
