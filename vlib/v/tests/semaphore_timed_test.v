// vtest flaky: true
// vtest retry: 3
import sync
import time

fn run_forever(shared foo []int, mut sem sync.Semaphore) {
	for {
		foo[0]++
	}
	sem.post() // indicate that thread is finished - never happens
}

fn test_semaphore() {
	shared abc := &[0]
	mut sem := sync.new_semaphore()
	spawn run_forever(shared abc, mut sem)
	for _ in 0 .. 1000 {
		unsafe { abc[0]-- }
	}
	// wait for the 2 coroutines to finish using the semaphore
	stopwatch := time.new_stopwatch()
	mut elapsed := stopwatch.elapsed()
	if !sem.timed_wait(200 * time.millisecond) {
		// we should come here due to timeout
		elapsed = stopwatch.elapsed()
	}
	elapsed_ms := f64(elapsed) / time.millisecond
	println('elapsed: ${elapsed_ms:.1f}ms')
	assert elapsed_ms >= 190.0
}
