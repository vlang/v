module executor

import time

fn test_run_drains_accepted_jobs_then_wait_returns() {
	mut ex := new(queue_size: 4)!
	ran := chan int{cap: 3}
	third_started := chan bool{cap: 1}
	release_third := chan bool{cap: 1}
	result := chan string{cap: 1}

	for i in 0 .. 2 {
		ex.try_post(fn [ran, i] () ! {
			ran <- i
		})!
	}
	ex.try_post(fn [ran, third_started, release_third] () ! {
		third_started <- true
		_ := <-release_third
		ran <- 2
	})!

	runner := spawn fn [mut ex, result] () {
		ex.run() or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	assert wait_for_int(ran, 'first run job missing') == 0
	assert wait_for_int(ran, 'second run job missing') == 1
	assert wait_for_bool(third_started, 'third run job did not start')
	ex.stop()
	release_third <- true
	assert wait_for_int(ran, 'third run job missing') == 2
	assert wait_for_string(result, 'run did not return after stop') == 'ok'
	ex.wait()!
	runner.wait()
}

fn test_stop_from_owner_callback_does_not_deadlock() {
	mut ex := new(queue_size: 2)!
	ran := chan bool{cap: 1}
	result := chan string{cap: 1}

	ex.try_post(fn [mut ex, ran] () ! {
		ex.stop()
		ran <- true
	})!

	runner := spawn fn [mut ex, result] () {
		ex.run() or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	assert wait_for_bool(ran, 'owner callback did not run')
	assert wait_for_string(result, 'run did not return after owner stop') == 'ok'
	ex.wait()!
	runner.wait()
}

fn test_wait_from_owner_callback_after_stop_returns_error_without_consuming_wait() {
	mut ex := new(queue_size: 2)!
	wait_result := chan string{cap: 1}
	run_result := chan string{cap: 1}

	ex.try_post(fn [mut ex, wait_result] () ! {
		ex.stop()
		ex.wait() or {
			wait_result <- err.msg()
			return
		}
		wait_result <- 'ok'
	})!

	runner := spawn fn [mut ex, run_result] () {
		ex.run() or {
			run_result <- err.msg()
			return
		}
		run_result <- 'ok'
	}()

	assert wait_for_string(wait_result, 'owner wait did not return') == 'executor: wait cannot be called from executor owner thread'
	assert wait_for_string(run_result, 'run did not return after owner stop') == 'ok'
	ex.wait()!
	runner.wait()
}

fn test_wait_before_terminal_pump_returns_error() {
	mut ex := new(queue_size: 1)!
	ex.wait() or {
		assert err.msg() == 'executor: wait requires a running pump or stopped executor'
		ex.stop()
		return
	}
	assert false, 'wait returned successfully before a terminal owner pump'
}

fn test_wait_before_runner_run_does_not_consume_wait() {
	mut ex := new(queue_size: 2)!
	start_run := chan bool{cap: 1}
	started := chan bool{cap: 1}
	result := chan string{cap: 1}

	ex.try_post(fn [started] () ! {
		started <- true
	})!

	runner := spawn fn [mut ex, start_run, result] () {
		_ := <-start_run
		ex.run() or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	mut saw_precondition := false
	ex.wait() or {
		assert err.msg() == 'executor: wait requires a running pump or stopped executor'
		saw_precondition = true
	}
	assert saw_precondition

	start_run <- true
	assert wait_for_bool(started, 'runner did not start owner pump')
	ex.stop()
	assert wait_for_string(result, 'run did not return after stop') == 'ok'
	ex.wait()!
	mut saw_second_wait := false
	ex.wait() or {
		assert err.msg() == 'executor: wait was already called'
		saw_second_wait = true
	}
	assert saw_second_wait
	runner.wait()
}

fn test_run_after_stop_closes_admission_and_second_run_is_stopped() {
	mut ex := new(queue_size: 1)!
	ex.stop()

	mut rejected_after_stop := false
	ex.try_post(fn () ! {}) or {
		assert err.msg() == 'executor: executor is closed'
		rejected_after_stop = true
	}
	assert rejected_after_stop

	ex.run() or {
		assert err.msg() == 'executor: executor is stopped'
		return
	}
	assert false, 'run after stop returned successfully'
}

fn test_second_wait_returns_error() {
	mut ex := new(queue_size: 1)!
	started := chan bool{cap: 1}
	release := chan bool{cap: 1}
	result := chan string{cap: 1}

	ex.try_post(fn [started, release] () ! {
		started <- true
		_ := <-release
	})!

	runner := spawn fn [mut ex, result] () {
		ex.run() or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	assert wait_for_bool(started, 'run did not start before wait test')
	release <- true
	ex.stop()
	assert wait_for_string(result, 'run did not stop') == 'ok'
	ex.wait()!
	ex.wait() or {
		assert err.msg() == 'executor: wait was already called'
		runner.wait()
		return
	}
	assert false, 'second wait returned successfully'
}

fn test_second_concurrent_run_returns_error() {
	mut ex := new(queue_size: 2)!
	started := chan bool{cap: 1}
	release := chan bool{cap: 1}
	first_result := chan string{cap: 1}
	second_result := chan string{cap: 1}

	ex.try_post(fn [started, release] () ! {
		started <- true
		_ := <-release
	})!

	first := spawn fn [mut ex, first_result] () {
		ex.run() or {
			first_result <- err.msg()
			return
		}
		first_result <- 'ok'
	}()
	assert wait_for_bool(started, 'first run did not start owner job')

	second := spawn fn [mut ex, second_result] () {
		ex.run() or {
			second_result <- err.msg()
			return
		}
		second_result <- 'ok'
	}()

	msg := wait_for_string(second_result, 'second run did not return')
	assert msg == 'executor: owner pump is already running'

	release <- true
	ex.stop()
	assert wait_for_string(first_result, 'first run did not stop') == 'ok'
	ex.wait()!
	first.wait()
	second.wait()
}

fn wait_for_bool(signal chan bool, message string) bool {
	select {
		value := <-signal {
			return value
		}
		5 * time.second {
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
		5 * time.second {
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
		5 * time.second {
			assert false, message
		}
	}
	return ''
}
