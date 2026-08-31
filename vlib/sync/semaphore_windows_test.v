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

fn test_semaphore_long_finite_timeout_uses_multiple_chunks() {
	timeout := i64(60 * 24 * 60 * 60) * 1_000_000_000
	first_t_ms := sync_milliseconds(timeout)
	assert first_t_ms == u32(C.INFINITE - 1)

	elapsed_after_first_chunk := i64(first_t_ms) * 1_000_000
	expired, next_t_ms := sync_timeout_chunk(timeout, elapsed_after_first_chunk)
	assert !expired
	assert next_t_ms > 0
	assert next_t_ms < first_t_ms

	expired_at_deadline, _ := sync_timeout_chunk(timeout, timeout)
	assert expired_at_deadline
}

fn test_semaphore_near_infinite_timeout_saturates_deadline() {
	time_now := i64(10_000_000_000)
	assert sync_timeout_deadline(time_now, infinite_timeout - 1) == infinite_timeout
	assert sync_timeout_deadline(time_now, 1_000_000_000) == time_now + 1_000_000_000
}
