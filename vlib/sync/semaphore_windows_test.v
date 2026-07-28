module sync

fn test_semaphore_timed_wait_reports_acquisition_after_fast_path() {
	mut sem := new_semaphore_init(1)
	did_acquire := sem.wait_for_available_count(1_000_000_000)
	was_consumed := !sem.try_wait()
	sem.destroy()

	assert did_acquire
	assert was_consumed
}

fn wake_then_post_semaphore(mut sem Semaphore) {
	for _ in 0 .. 20 {
		sync_sleep_nanoseconds(5_000_000)
		C.WakeConditionVariable(&sem.cond)
	}
	sem.post()
}

fn test_semaphore_timed_wait_handles_spurious_wake_with_infinite_timeout() {
	mut sem := new_semaphore()
	worker := spawn wake_then_post_semaphore(mut sem)
	acquired := sem.timed_wait(infinite_timeout)
	worker.wait()
	sem.destroy()

	assert acquired
}
