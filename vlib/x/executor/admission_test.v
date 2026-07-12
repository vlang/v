module executor

import context
import time

fn test_try_post_returns_backpressure_when_queue_is_full() {
	mut ex := new(queue_size: 1)!
	ex.try_post(fn () ! {})!
	ex.try_post(fn () ! {}) or {
		assert err.msg() == 'executor: queue is full'
		ex.stop()
		return
	}
	assert false, 'try_post accepted a job beyond queue capacity'
}

fn test_post_with_timeout_returns_timeout_when_queue_stays_full() {
	mut ex := new(queue_size: 1)!
	ex.try_post(fn () ! {})!
	ex.post_with_timeout(20 * time.millisecond, fn () ! {}) or {
		assert err.msg() == 'executor: timeout'
		ex.stop()
		return
	}
	assert false, 'post_with_timeout accepted a job while the queue stayed full'
}

fn test_post_with_timeout_zero_timeout_rejects_available_capacity() {
	mut ex := new(queue_size: 1)!
	ran := chan bool{cap: 1}
	ex.post_with_timeout(time.Duration(0), fn [ran] () ! {
		ran <- true
		return
	}) or {
		assert err.msg() == 'executor: timeout'
		assert ex.drain_pending(1)! == 0
		assert_no_bool(ran, 'zero-timeout admission job ran')
		ex.stop()
		ex.wait()!
		return
	}
	assert false, 'post_with_timeout accepted a job with zero timeout'
}

fn test_post_with_timeout_negative_timeout_rejects_available_capacity() {
	mut ex := new(queue_size: 1)!
	ran := chan bool{cap: 1}
	ex.post_with_timeout(-1 * time.nanosecond, fn [ran] () ! {
		ran <- true
		return
	}) or {
		assert err.msg() == 'executor: timeout'
		assert ex.drain_pending(1)! == 0
		assert_no_bool(ran, 'negative-timeout admission job ran')
		ex.stop()
		ex.wait()!
		return
	}
	assert false, 'post_with_timeout accepted a job with negative timeout'
}

fn test_post_with_timeout_long_timeout_accepts_available_capacity() {
	jobs := 4
	mut ex := new(queue_size: jobs)!
	ran := chan int{cap: jobs}

	for i in 0 .. jobs {
		ex.post_with_timeout(1 * time.hour, fn [ran, i] () ! {
			value := i + 1
			ran <- value
			return
		})!
	}

	assert ex.drain_pending(jobs)! == jobs
	mut total := 0
	for _ in 0 .. jobs {
		total += <-ran
	}
	assert total == 10

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}

fn test_post_with_context_returns_parent_cancellation_without_accepting_job() {
	mut ex := new(queue_size: 1)!
	ex.try_post(fn () ! {})!
	mut background := context.background()
	mut parent, cancel := context.with_cancel(mut background)
	cancel()
	ran := chan bool{cap: 1}

	ex.post_with_context(parent, fn [ran] () ! {
		ran <- true
	}) or {
		assert err.msg() == 'context canceled'
		ex.stop()
		assert_no_bool(ran, 'canceled admission job ran')
		return
	}
	assert false, 'post_with_context accepted after parent cancellation'
}

fn test_post_with_context_rejects_nil_job() {
	mut ex := new(queue_size: 1)!
	nil_job := unsafe { JobFn(nil) }
	ex.post_with_context(context.background(), nil_job) or {
		assert err.msg() == 'executor: job function is nil'
		ex.stop()
		return
	}
	assert false, 'post_with_context accepted a nil job'
}

fn test_post_with_timeout_rejects_nil_job() {
	mut ex := new(queue_size: 1)!
	nil_job := unsafe { JobFn(nil) }
	ex.post_with_timeout(1 * time.second, nil_job) or {
		assert err.msg() == 'executor: job function is nil'
		ex.stop()
		return
	}
	assert false, 'post_with_timeout accepted a nil job'
}

fn test_owner_post_with_context_full_queue_returns_owner_capacity_error() {
	mut ex := new(queue_size: 1)!
	owner_started := chan bool{cap: 1}
	filler_accepted := chan bool{cap: 1}
	owner_result := chan string{cap: 1}
	filler_ran := chan bool{cap: 1}
	nested_ran := chan bool{cap: 1}

	ex.try_post(fn [mut ex, owner_started, filler_accepted, owner_result, nested_ran] () ! {
		owner_started <- true
		assert wait_for_bool(filler_accepted, 'filler job was not accepted')
		ex.post_with_context(context.background(), fn [nested_ran] () ! {
			nested_ran <- true
		}) or {
			owner_result <- err.msg()
			return
		}
		owner_result <- 'accepted'
	})!

	filler := spawn fn [mut ex, owner_started, filler_accepted, filler_ran] () {
		assert wait_for_bool(owner_started, 'owner callback did not start')
		ex.try_post(fn [filler_ran] () ! {
			filler_ran <- true
		}) or {
			filler_accepted <- false
			return
		}
		filler_accepted <- true
	}()

	runner := spawn fn [mut ex, owner_result] () {
		ex.run_one() or {
			owner_result <- err.msg()
			return
		}
	}()

	msg := wait_for_string(owner_result, 'owner post_with_context did not return')
	assert msg == 'executor: owner thread cannot wait for queue capacity'

	ex.stop()
	runner.wait()
	filler.wait()
	assert ex.run_one()!
	assert wait_for_bool(filler_ran, 'filler job did not run after owner rejection')
	assert_no_bool(nested_ran, 'owner post_with_context job ran after rejection')
	ex.wait()!
}

fn test_owner_post_with_timeout_full_queue_returns_owner_capacity_error() {
	mut ex := new(queue_size: 1)!
	owner_started := chan bool{cap: 1}
	filler_accepted := chan bool{cap: 1}
	owner_result := chan string{cap: 1}
	filler_ran := chan bool{cap: 1}
	nested_ran := chan bool{cap: 1}

	ex.try_post(fn [mut ex, owner_started, filler_accepted, owner_result, nested_ran] () ! {
		owner_started <- true
		assert wait_for_bool(filler_accepted, 'filler job was not accepted')
		ex.post_with_timeout(1 * time.hour, fn [nested_ran] () ! {
			nested_ran <- true
		}) or {
			owner_result <- err.msg()
			return
		}
		owner_result <- 'accepted'
	})!

	filler := spawn fn [mut ex, owner_started, filler_accepted, filler_ran] () {
		assert wait_for_bool(owner_started, 'owner callback did not start')
		ex.try_post(fn [filler_ran] () ! {
			filler_ran <- true
		}) or {
			filler_accepted <- false
			return
		}
		filler_accepted <- true
	}()

	runner := spawn fn [mut ex, owner_result] () {
		ex.run_one() or {
			owner_result <- err.msg()
			return
		}
	}()

	msg := wait_for_string(owner_result, 'owner post_with_timeout did not return')
	assert msg == 'executor: owner thread cannot wait for queue capacity'

	ex.stop()
	runner.wait()
	filler.wait()
	assert ex.run_one()!
	assert wait_for_bool(filler_ran, 'filler job did not run after owner rejection')
	assert_no_bool(nested_ran, 'owner post_with_timeout job ran after rejection')
	ex.wait()!
}

fn test_owner_try_post_full_queue_returns_queue_full_and_pump_continues() {
	mut ex := new(queue_size: 1)!
	owner_started := chan bool{cap: 1}
	filler_accepted := chan bool{cap: 1}
	owner_result := chan string{cap: 1}
	filler_ran := chan bool{cap: 1}
	nested_ran := chan bool{cap: 1}

	ex.try_post(fn [mut ex, owner_started, filler_accepted, owner_result, nested_ran] () ! {
		owner_started <- true
		assert wait_for_bool(filler_accepted, 'filler job was not accepted')
		ex.try_post(fn [nested_ran] () ! {
			nested_ran <- true
		}) or {
			owner_result <- err.msg()
			return
		}
		owner_result <- 'accepted'
	})!

	filler := spawn fn [mut ex, owner_started, filler_accepted, filler_ran] () {
		assert wait_for_bool(owner_started, 'owner callback did not start')
		ex.try_post(fn [filler_ran] () ! {
			filler_ran <- true
		}) or {
			filler_accepted <- false
			return
		}
		filler_accepted <- true
	}()

	assert ex.run_one()!
	assert wait_for_string(owner_result, 'owner try_post did not return') == 'executor: queue is full'
	filler.wait()
	assert ex.run_one()!
	assert wait_for_bool(filler_ran, 'pump did not continue to filler job')
	assert_no_bool(nested_ran, 'owner try_post job ran after queue-full rejection')

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}

fn test_owner_bounded_posts_accept_when_capacity_is_available() {
	mut ex := new(queue_size: 2)!
	owner_result := chan string{cap: 1}
	context_ran := chan bool{cap: 1}
	timeout_ran := chan bool{cap: 1}

	ex.try_post(fn [mut ex, owner_result, context_ran, timeout_ran] () ! {
		ex.post_with_context(context.background(), fn [context_ran] () ! {
			context_ran <- true
		}) or {
			owner_result <- err.msg()
			return
		}
		ex.post_with_timeout(1 * time.hour, fn [timeout_ran] () ! {
			timeout_ran <- true
		}) or {
			owner_result <- err.msg()
			return
		}
		owner_result <- 'accepted'
	})!

	assert ex.run_one()!
	assert wait_for_string(owner_result, 'owner bounded posts did not return') == 'accepted'
	assert ex.run_one()!
	assert ex.run_one()!
	assert wait_for_bool(context_ran, 'owner post_with_context job did not run')
	assert wait_for_bool(timeout_ran, 'owner post_with_timeout job did not run')

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}

fn test_post_with_context_waits_until_capacity_opens() {
	mut ex := new(queue_size: 1)!
	release := chan bool{cap: 1}
	started := chan bool{cap: 1}
	attempting := chan bool{cap: 1}
	accepted := chan bool{cap: 1}
	ran := chan bool{cap: 1}
	owner_result := chan string{cap: 1}

	ex.try_post(fn [started, release] () ! {
		started <- true
		_ := <-release
	})!

	submitter := spawn fn [mut ex, attempting, accepted, ran] () {
		attempting <- true
		ex.post_with_context(context.background(), fn [ran] () ! {
			ran <- true
		}) or {
			accepted <- false
			return
		}
		accepted <- true
	}()
	assert wait_for_bool(attempting, 'bounded submitter did not start')
	assert_no_bool(accepted, 'post_with_context returned before capacity opened')

	owner := spawn fn [mut ex, owner_result] () {
		ran_one := ex.run_one() or {
			owner_result <- err.msg()
			return
		}
		if ran_one {
			owner_result <- 'ran'
		} else {
			owner_result <- 'empty'
		}
	}()
	assert wait_for_bool(started, 'blocking job did not start')
	assert wait_for_bool(accepted, 'post_with_context did not accept after capacity opened')
	release <- true
	assert wait_for_string(owner_result, 'owner run_one did not return') == 'ran'
	assert ex.run_one()!
	assert wait_for_bool(ran, 'bounded admission job did not run')

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
	owner.wait()
	submitter.wait()
}

fn wait_for_bool(signal chan bool, message string) bool {
	select {
		value := <-signal {
			return value
		}
		executor_admission_signal_timeout() {
			assert false, message
		}
	}
	return false
}

fn wait_for_string(signal chan string, message string) string {
	select {
		value := <-signal {
			return value
		}
		executor_admission_signal_timeout() {
			assert false, message
		}
	}
	return ''
}

fn executor_admission_signal_timeout() time.Duration {
	$if windows {
		return 5 * time.second
	}
	return 1 * time.second
}

fn assert_no_bool(signal chan bool, message string) {
	select {
		_ := <-signal {
			assert false, message
		}
		50 * time.millisecond {}
	}
}
