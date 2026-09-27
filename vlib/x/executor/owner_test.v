module executor

import time

fn test_owner_checks_are_true_only_while_pumping() {
	mut ex := new(queue_size: 2)!
	observed := chan bool{cap: 1}

	assert !ex.is_owner_thread()
	mut rejected_before_pump := false
	ex.assert_owner_thread() or {
		assert err.msg() == 'executor: current thread is not the executor owner'
		rejected_before_pump = true
	}
	assert rejected_before_pump

	ex.try_post(fn [mut ex, observed] () ! {
		observed <- ex.is_owner_thread()
		ex.assert_owner_thread()!
	})!

	assert ex.run_one()!
	assert wait_for_bool(observed, 'owner check was false inside owner callback')
	assert !ex.is_owner_thread()
	ex.assert_owner_thread() or {
		assert err.msg() == 'executor: current thread is not the executor owner'
		ex.stop()
		assert !ex.run_one()!
		ex.wait()!
		return
	}
	assert false, 'assert_owner_thread succeeded after owner pump returned'
}

fn test_owner_identity_is_cleared_after_job_error() {
	mut ex := new(queue_size: 1)!
	observed := chan bool{cap: 1}

	ex.try_post(fn [mut ex, observed] () ! {
		observed <- ex.is_owner_thread()
		return error('owner error')
	})!

	mut saw_job_error := false
	ex.run_one() or {
		assert err.msg() == 'owner error'
		saw_job_error = true
	}
	assert saw_job_error
	assert wait_for_bool(observed, 'owner identity was not set during erroring job')
	assert !ex.is_owner_thread()
}

fn wait_for_bool(signal chan bool, message string) bool {
	select {
		value := <-signal {
			return value
		}
		1 * time.second {
			assert false, message
		}
	}
	return false
}
