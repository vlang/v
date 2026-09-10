module executor

import time

fn test_run_sync_from_foreign_thread_waits_until_owner_executes_job() {
	mut ex := new(queue_size: 4)!
	sync_started := chan bool{cap: 1}
	sync_ran := chan bool{cap: 1}
	sync_done := chan bool{cap: 1}
	barrier_started := chan bool{cap: 1}
	release_barrier := chan bool{cap: 1}
	run_result := chan string{cap: 1}

	runner := spawn fn [mut ex, run_result] () {
		ex.run() or {
			run_result <- err.msg()
			return
		}
		run_result <- 'ok'
	}()

	caller := spawn fn [mut ex, sync_started, sync_ran, sync_done] () {
		sync_started <- true
		ex.run_sync(fn [sync_ran] () ! {
			sync_ran <- true
		}) or {
			sync_done <- false
			return
		}
		sync_done <- true
	}()

	assert wait_for_bool(sync_started, 'run_sync caller did not start')
	assert wait_for_bool(sync_ran, 'run_sync job did not run on owner pump')
	assert wait_for_bool(sync_done, 'run_sync caller did not finish')
	ex.try_post(fn [barrier_started, release_barrier] () ! {
		barrier_started <- true
		_ := <-release_barrier
	})!
	assert wait_for_bool(barrier_started, 'owner barrier job did not start')
	ex.stop()
	release_barrier <- true
	assert wait_for_string(run_result, 'run did not stop') == 'ok'
	ex.wait()!
	caller.wait()
	runner.wait()
}

fn test_run_sync_returns_job_error_to_foreign_caller() {
	mut ex := new(queue_size: 2)!
	run_result := chan string{cap: 1}
	sync_result := chan string{cap: 1}

	runner := spawn fn [mut ex, run_result] () {
		ex.run() or {
			run_result <- err.msg()
			return
		}
		run_result <- 'ok'
	}()

	caller := spawn fn [mut ex, sync_result] () {
		ex.run_sync(fn () ! {
			return error('sync job failed')
		}) or {
			sync_result <- err.msg()
			return
		}
		sync_result <- 'ok'
	}()

	assert wait_for_string(sync_result, 'run_sync error was not returned') == 'sync job failed'
	assert wait_for_string(run_result, 'run did not return after sync error') == 'sync job failed'
	ex.wait() or { assert err.msg() == 'sync job failed' }
	caller.wait()
	runner.wait()
}

fn test_run_sync_from_owner_runs_inline_without_deadlock() {
	mut ex := new(queue_size: 4)!
	events := chan int{cap: 4}

	ex.try_post(fn [mut ex, events] () ! {
		events <- 1
		ex.run_sync(fn [events] () ! {
			events <- 100
		})!
		events <- 2
	})!
	ex.try_post(fn [events] () ! {
		events <- 3
	})!

	assert ex.drain_pending(2)! == 2
	assert wait_for_int(events, 'outer job did not start') == 1
	assert wait_for_int(events, 'inline run_sync job did not run') == 100
	assert wait_for_int(events, 'outer job did not finish') == 2
	assert wait_for_int(events, 'second queued job did not run') == 3

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}

fn test_run_sync_owner_inline_error_caught_closes_admission_and_wait_returns_error() {
	mut ex := new(queue_size: 4)!
	events := chan int{cap: 3}
	post_result := chan string{cap: 1}
	run_result := chan string{cap: 1}

	ex.try_post(fn [mut ex, events, post_result] () ! {
		events <- 1
		ex.run_sync(fn () ! {
			return error('inline sync failed')
		}) or { events <- 2 }
		ex.try_post(fn () ! {}) or {
			post_result <- err.msg()
			return
		}
		post_result <- 'accepted'
		ex.stop()
	})!
	ex.try_post(fn [events] () ! {
		events <- 3
	})!

	runner := spawn fn [mut ex, run_result] () {
		ex.run() or {
			run_result <- err.msg()
			return
		}
		run_result <- 'ok'
	}()

	assert wait_for_int(events, 'outer job did not start') == 1
	assert wait_for_int(events, 'inline run_sync error was not caught') == 2
	assert wait_for_string(post_result, 'try_post after inline error did not return') == 'executor: executor is closed'
	assert wait_for_int(events, 'accepted job after inline error was not drained') == 3
	assert wait_for_string(run_result, 'run did not return inline first error') == 'inline sync failed'
	ex.wait() or {
		assert err.msg() == 'inline sync failed'
		runner.wait()
		return
	}
	assert false, 'wait returned successfully after inline run_sync error'
}

fn test_run_one_returns_owner_inline_run_sync_error_caught_by_outer_job() {
	mut ex := new(queue_size: 4)!
	events := chan int{cap: 2}

	ex.try_post(fn [mut ex, events] () ! {
		ex.run_sync(fn () ! {
			return error('inline sync failed')
		}) or { events <- 1 }
	})!
	ex.try_post(fn [events] () ! {
		events <- 2
	})!

	mut saw_inline_error := false
	ex.run_one() or {
		assert err.msg() == 'inline sync failed'
		saw_inline_error = true
	}
	assert saw_inline_error
	assert wait_for_int(events, 'outer job did not catch inline run_sync error') == 1

	mut saw_closed := false
	ex.try_post(fn () ! {}) or {
		assert err.msg() == 'executor: executor is closed'
		saw_closed = true
	}
	assert saw_closed

	ex.run_one() or { assert err.msg() == 'inline sync failed' }
	assert wait_for_int(events, 'accepted job after inline error was not drained') == 2
	ex.wait() or {
		assert err.msg() == 'inline sync failed'
		return
	}
	assert false, 'wait returned successfully after inline run_sync error'
}

fn test_drain_pending_limit_returns_owner_inline_run_sync_error_caught_by_outer_job() {
	mut ex := new(queue_size: 4)!
	events := chan int{cap: 2}

	ex.try_post(fn [mut ex, events] () ! {
		ex.run_sync(fn () ! {
			return error('inline sync failed')
		}) or { events <- 1 }
	})!
	ex.try_post(fn [events] () ! {
		events <- 2
	})!

	ex.drain_pending(1) or { assert err.msg() == 'inline sync failed' }
	assert wait_for_int(events, 'outer job did not catch inline run_sync error') == 1

	ex.drain_pending(8) or { assert err.msg() == 'inline sync failed' }
	assert wait_for_int(events, 'accepted job after inline error was not drained') == 2
	ex.wait() or {
		assert err.msg() == 'inline sync failed'
		return
	}
	assert false, 'wait returned successfully after inline run_sync error'
}

fn test_run_sync_from_owner_after_stop_returns_closed_without_running_nested_job() {
	mut ex := new(queue_size: 2)!
	result := chan string{cap: 1}
	nested_ran := chan bool{cap: 1}

	ex.try_post(fn [mut ex, result, nested_ran] () ! {
		ex.stop()
		ex.run_sync(fn [nested_ran] () ! {
			nested_ran <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	})!

	assert ex.run_one()!
	assert wait_for_string(result, 'owner run_sync after stop did not return') == 'executor: executor is closed'
	select {
		_ := <-nested_ran {
			assert false, 'nested run_sync job ran after stop'
		}
		else {}
	}
	ex.wait()!
}

fn test_run_sync_from_owner_after_prior_job_error_is_refused_without_nested_execution() {
	mut ex := new(queue_size: 3)!
	result := chan string{cap: 1}
	nested_ran := chan bool{cap: 1}

	ex.try_post(fn () ! {
		return error('first job failed')
	})!
	ex.try_post(fn [mut ex, result, nested_ran] () ! {
		ex.run_sync(fn [nested_ran] () ! {
			nested_ran <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	})!

	mut run_result := 'ok'
	ex.run() or { run_result = err.msg() }
	assert run_result == 'first job failed'
	assert wait_for_string(result, 'owner run_sync after first error did not return') == 'executor: executor is closed'
	select {
		_ := <-nested_ran {
			assert false, 'nested run_sync job ran after first error'
		}
		else {}
	}
	ex.wait() or {
		assert err.msg() == 'first job failed'
		return
	}
	assert false, 'wait returned successfully after first job error'
}

fn test_run_sync_after_stop_returns_error() {
	mut ex := new(queue_size: 1)!
	ex.stop()
	ex.run_sync(fn () ! {}) or {
		assert err.msg() == 'executor: executor is closed'
		return
	}
	assert false, 'run_sync accepted after stop'
}

fn test_run_sync_rejects_nil_job() {
	mut ex := new(queue_size: 1)!
	nil_job := unsafe { JobFn(nil) }
	ex.run_sync(nil_job) or {
		assert err.msg() == 'executor: job function is nil'
		ex.stop()
		return
	}
	assert false, 'run_sync accepted a nil job'
}

fn wait_for_bool(signal chan bool, message string) bool {
	select {
		value := <-signal {
			return value
		}
		executor_sync_call_signal_timeout() {
			assert false, message
		}
	}
	return false
}

fn wait_for_int(signal chan int, message string) int {
	select {
		value := <-signal {
			return value
		}
		executor_sync_call_signal_timeout() {
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
		executor_sync_call_signal_timeout() {
			assert false, message
		}
	}
	return ''
}

fn executor_sync_call_signal_timeout() time.Duration {
	$if windows {
		return 5 * time.second
	}
	return 1 * time.second
}
