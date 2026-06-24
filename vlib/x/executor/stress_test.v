module executor

import context
import time

fn test_many_producers_submit_bounded_work() {
	mut ex := new(queue_size: 16)!
	accepted := chan bool{cap: 16}
	ran := chan int{cap: 16}
	mut producers := []thread{}

	for i in 0 .. 16 {
		producers << spawn fn [mut ex, accepted, ran, i] () {
			ex.post_with_context(context.background(), fn [ran, i] () ! {
				ran <- i
			}) or {
				accepted <- false
				return
			}
			accepted <- true
		}()
	}

	for _ in 0 .. 16 {
		assert wait_for_bool(accepted, 'producer did not finish admission')
	}
	assert ex.drain_pending(16)! == 16

	mut total := 0
	for _ in 0 .. 16 {
		total += wait_for_int(ran, 'producer job did not run')
	}
	assert total == 120

	for producer in producers {
		producer.wait()
	}
	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}

fn test_stop_wakes_waiting_submitter_without_accepting_job() {
	mut ex := new(queue_size: 1)!
	attempting := chan bool{cap: 1}
	result := chan string{cap: 1}
	ran := chan bool{cap: 1}

	ex.try_post(fn () ! {})!
	submitter := spawn fn [mut ex, attempting, result, ran] () {
		attempting <- true
		ex.post_with_context(context.background(), fn [ran] () ! {
			ran <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()

	assert wait_for_bool(attempting, 'waiting submitter did not start')
	ex.stop()
	msg := wait_for_string(result, 'waiting submitter was not woken by stop')
	assert msg == 'executor: executor is closed'
	assert_no_bool(ran, 'job was accepted after stop')
	submitter.wait()
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

fn assert_no_bool(signal chan bool, message string) {
	select {
		_ := <-signal {
			assert false, message
		}
		50 * time.millisecond {}
	}
}
