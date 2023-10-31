import time

const (
	iterations_per_thread = 100000
)

fn add_elements(shared foo []int, n int) {
	for _ in 0 .. iterations_per_thread {
		foo << n
	}
	foo[0]++
}

fn test_autolocked_array() {
	shared abc := &[0]
	spawn add_elements(shared abc, 1)
	spawn add_elements(shared abc, 3)
	for _ in 0 .. iterations_per_thread {
		abc << 0
	}
	// wait for coroutines to finish - that should really be
	// done by channels, yield, semaphore...
	for {
		mut finished_threads := 0
		rlock abc {
			finished_threads = unsafe {
				abc[0]
			}
		}
		if finished_threads == 2 {
			break
		}
		time.sleep(100 * time.millisecond)
	}
	// create histogram of results
	mut result := [0, 0, 0, 0]
	rlock abc {
		// automatic rlock for iteration is also not implemented, yet
		for v in abc {
			if v > 3 {
				panic('unexpected element on array')
			}
			result[v]++
		}
	}
	assert result[0] == iterations_per_thread
	assert result[1] == iterations_per_thread
	assert result[2] == 1 // number of non-main threads
	assert result[3] == iterations_per_thread
}
