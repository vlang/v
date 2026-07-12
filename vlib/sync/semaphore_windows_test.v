module sync

import time

fn test_semaphore_timed_wait_reports_acquisition_after_fast_path() {
	mut sem := new_semaphore_init(1)
	did_acquire := sem.wait_for_available_count(time.second)
	was_consumed := !sem.try_wait()
	sem.destroy()

	assert did_acquire
	assert was_consumed
}
