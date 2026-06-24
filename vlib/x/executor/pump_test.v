module executor

import time

fn test_run_one_executes_one_job_and_reports_true() {
	mut ex := new(queue_size: 4)!
	values := chan int{cap: 2}

	ex.try_post(fn [values] () ! {
		values <- 1
	})!
	ex.try_post(fn [values] () ! {
		values <- 2
	})!

	assert ex.run_one()!
	assert wait_for_int(values, 'first job did not run') == 1
	assert ex.run_one()!
	assert wait_for_int(values, 'second job did not run') == 2
	assert !ex.run_one()!

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}

fn test_drain_pending_runs_at_most_limit() {
	mut ex := new(queue_size: 4)!
	values := chan int{cap: 4}
	for i in 0 .. 4 {
		ex.try_post(fn [values, i] () ! {
			values <- i
		})!
	}

	assert ex.drain_pending(2)! == 2
	assert wait_for_int(values, 'first drained job missing') == 0
	assert wait_for_int(values, 'second drained job missing') == 1

	assert ex.drain_pending(8)! == 2
	assert wait_for_int(values, 'third drained job missing') == 2
	assert wait_for_int(values, 'fourth drained job missing') == 3

	ex.stop()
	assert ex.drain_pending(1)! == 0
	ex.wait()!
}

fn test_drain_pending_rejects_non_positive_limit() {
	mut ex := new(queue_size: 1)!
	ex.drain_pending(0) or {
		assert err.msg() == 'executor: drain limit must be positive'
		ex.stop()
		return
	}
	assert false, 'drain_pending accepted a zero limit'
}

fn test_single_producer_fifo_order_is_preserved() {
	mut ex := new(queue_size: 8)!
	values := chan int{cap: 5}
	for i in 0 .. 5 {
		ex.try_post(fn [values, i] () ! {
			values <- i
		})!
	}

	assert ex.drain_pending(5)! == 5
	for expected in 0 .. 5 {
		assert wait_for_int(values, 'FIFO job missing') == expected
	}

	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
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
