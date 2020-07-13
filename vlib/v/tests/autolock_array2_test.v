import sync
import time

const (
	iterations_per_thread2 = 100000
)

fn inc_elements(shared foo []int, n int) {
	for _ in 0 .. iterations_per_thread2 {
		foo[n]++
	}
	foo[0]++ // indicat that thread is finished
}

fn test_autolocked_array_2() {
	shared abc := &[0, 0, 0]
	go inc_elements(shared abc, 1)
	go inc_elements(shared abc, 2)
	for _ in 0 .. iterations_per_thread2 {
		abc[2]++
	}
	// wait for coroutines to finish - that should really be
	// done by channels, yield, semaphore...
	for {
		mut finished_threads := 0
		rlock abc {
			finished_threads = abc[0]
		}
		if finished_threads == 2 {
			break
		}
		time.sleep_ms(100)
	}
	rlock abc {
		assert abc[1] == iterations_per_thread2
		assert abc[2] == 2 * iterations_per_thread2
	}
}
