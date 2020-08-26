import sync
import time

fn run_forever(shared foo []int, sem sync.Semaphore) {
	for {
		foo[0]++
	}
	sem.post() // indicate that thread is finished - never happens
}

fn test_semaphore() {
	shared abc := &[0]
	sem := sync.new_semaphore()
	go run_forever(shared abc, sem)
	for _ in 0 .. 1000 {
		unsafe { abc[0]-- }
	}
	// wait for the 2 coroutines to finish using the semaphore
	stopwatch := time.new_stopwatch({})
	mut elapsed := stopwatch.elapsed()
	if !sem.timed_wait(50 * time.millisecond) {
		// we should come here due to timeout
		elapsed = stopwatch.elapsed()
	}
	println('elapsed: ${f64(elapsed)/time.second}s')
	assert elapsed >= 48* time.millisecond
}
