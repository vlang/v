module executor

import time

fn test_first_job_error_is_returned_after_accepted_jobs_drain() {
	mut ex := new(queue_size: 4)!
	ran := chan int{cap: 2}
	result := chan string{cap: 1}

	ex.try_post(fn [ran] () ! {
		ran <- 1
		return error('first owner failure')
	})!
	ex.try_post(fn [ran] () ! {
		ran <- 2
	})!

	runner := spawn fn [mut ex, result] () {
		ex.run() or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	assert wait_for_int(ran, 'first job did not run') == 1
	assert wait_for_int(ran, 'accepted job after error did not drain') == 2
	assert wait_for_string(result, 'run did not return first job error') == 'first owner failure'
	ex.wait() or {
		assert err.msg() == 'first owner failure'
		runner.wait()
		return
	}
	assert false, 'wait did not return first job error'
}

fn test_admission_closes_after_first_job_error() {
	mut ex := new(queue_size: 1)!
	ex.try_post(fn () ! {
		return error('owner failure')
	})!
	mut saw_job_error := false
	ex.run_one() or {
		assert err.msg() == 'owner failure'
		saw_job_error = true
	}
	assert saw_job_error

	ex.try_post(fn () ! {}) or {
		assert err.msg() == 'executor: executor is closed'
		ex.wait() or { assert err.msg() == 'owner failure' }
		return
	}
	assert false, 'try_post accepted after first job error'
}

fn test_run_one_after_job_error_requires_continued_pumping_to_drain() {
	mut ex := new(queue_size: 3)!
	ran := chan int{cap: 2}

	ex.try_post(fn [ran] () ! {
		ran <- 1
		return error('owner failure')
	})!
	ex.try_post(fn [ran] () ! {
		ran <- 2
	})!

	ex.run_one() or { assert err.msg() == 'owner failure' }
	assert wait_for_int(ran, 'erroring job did not run') == 1

	ex.run_one() or { assert err.msg() == 'owner failure' }
	assert wait_for_int(ran, 'accepted job after error was not drained') == 2

	ex.wait() or {
		assert err.msg() == 'owner failure'
		return
	}
	assert false, 'wait did not return the stored owner failure'
}

fn test_drain_pending_after_job_error_requires_continued_pumping_when_limit_is_reached() {
	mut ex := new(queue_size: 4)!
	ran := chan int{cap: 3}

	ex.try_post(fn [ran] () ! {
		ran <- 1
		return error('owner failure')
	})!
	ex.try_post(fn [ran] () ! {
		ran <- 2
	})!
	ex.try_post(fn [ran] () ! {
		ran <- 3
	})!

	ex.drain_pending(1) or { assert err.msg() == 'owner failure' }
	assert wait_for_int(ran, 'erroring drain job did not run') == 1

	ex.drain_pending(8) or { assert err.msg() == 'owner failure' }
	assert wait_for_int(ran, 'second accepted job was not drained') == 2
	assert wait_for_int(ran, 'third accepted job was not drained') == 3

	ex.wait() or {
		assert err.msg() == 'owner failure'
		return
	}
	assert false, 'wait did not return the stored owner failure'
}

fn wait_for_int(signal chan int, message string) int {
	select {
		value := <-signal {
			return value
		}
		1 * time.second {
			assert false, message
		}
	}
	return 0
}

fn wait_for_string(signal chan string, message string) string {
	select {
		value := <-signal {
			return value
		}
		1 * time.second {
			assert false, message
		}
	}
	return ''
}
