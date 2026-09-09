import context
import time
import x.async as xasync

fn test_every_runs_at_least_one_iteration() {
	parent_ctx, cancel := xasync.with_cancel()
	ran := chan bool{cap: 1}
	result := chan string{cap: 1}
	worker := spawn fn [parent_ctx, ran, result] () {
		xasync.every(parent_ctx, 5 * time.millisecond, fn [ran] (mut ctx context.Context) ! {
			_ = ctx
			ran <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	select {
		did_run := <-ran {
			assert did_run
		}
		1 * time.second {
			assert false, 'periodic job did not run'
		}
	}
	cancel()
	select {
		msg := <-result {
			assert msg == 'context canceled'
		}
		1 * time.second {
			assert false, 'every did not stop after cancellation'
		}
	}
	worker.wait()
}

fn test_every_stops_on_cancellation() {
	parent_ctx, cancel := xasync.with_cancel()
	entered := chan bool{cap: 1}
	result := chan string{cap: 1}
	worker := spawn fn [parent_ctx, entered, result] () {
		entered <- true
		xasync.every(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
			_ = ctx
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	select {
		did_enter := <-entered {
			assert did_enter
		}
		1 * time.second {
			assert false, 'every worker did not start'
		}
	}
	cancel()
	select {
		msg := <-result {
			assert msg == 'context canceled'
		}
		1 * time.second {
			assert false, 'every did not stop on context cancellation'
		}
	}
	worker.wait()
}

fn test_every_returns_immediately_when_parent_is_already_canceled() {
	parent_ctx, cancel := xasync.with_cancel()
	cancel()
	xasync.every(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_every_returns_iteration_error() {
	parent_ctx, cancel := xasync.with_cancel()
	result := chan string{cap: 1}
	worker := spawn fn [parent_ctx, result] () {
		xasync.every(parent_ctx, 5 * time.millisecond, fn (mut ctx context.Context) ! {
			_ = ctx
			return error('periodic failed')
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	select {
		msg := <-result {
			assert msg == 'periodic failed'
		}
		1 * time.second {
			assert false, 'every did not return the periodic job error'
		}
	}
	cancel()
	worker.wait()
}

fn test_every_rejects_zero_interval() {
	xasync.every(context.background(), 0 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'async: interval must be positive'
		return
	}
	assert false
}

fn test_every_rejects_negative_interval() {
	xasync.every(context.background(), -1 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'async: interval must be positive'
		return
	}
	assert false
}

fn test_every_rejects_nil_job() {
	nil_job := unsafe { xasync.JobFn(nil) }
	xasync.every(context.background(), 1 * time.second, nil_job) or {
		assert err.msg() == 'async: job function is nil'
		return
	}
	assert false
}

fn test_every_does_not_overlap_iterations() {
	parent_ctx, cancel := xasync.with_cancel()
	active := chan bool{cap: 1}
	active <- true
	entered := chan bool{cap: 2}
	release := chan bool{cap: 2}
	overlap := chan bool{cap: 1}
	result := chan string{cap: 1}
	worker := spawn fn [parent_ctx, active, entered, release, overlap, result] () {
		xasync.every(parent_ctx, 5 * time.millisecond, fn [active, entered, release, overlap] (mut ctx context.Context) ! {
			select {
				_ := <-active {}
				else {
					overlap <- true
					return error('periodic overlap')
				}
			}
			entered <- true
			done := ctx.done()
			select {
				_ := <-release {}
				_ := <-done {
					active <- true
					return ctx.err()
				}
			}
			active <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	wait_for_periodic_entry(entered)
	select {
		_ := <-entered {
			assert false, 'periodic iterations overlapped while first job was still running'
		}
		50 * time.millisecond {}
	}
	select {
		did_overlap := <-overlap {
			assert !did_overlap
		}
		else {}
	}

	release <- true
	wait_for_periodic_entry(entered)
	cancel()
	release <- true
	select {
		msg := <-result {
			assert msg == 'context canceled'
		}
		1 * time.second {
			assert false, 'every did not stop after non-overlap test cancellation'
		}
	}
	worker.wait()
}

fn test_start_every_rejects_invalid_interval() {
	xasync.start_every(context.background(), 0 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'async: interval must be positive'
		return
	}
	assert false
}

fn test_start_every_rejects_negative_interval() {
	xasync.start_every(context.background(), -1 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'async: interval must be positive'
		return
	}
	assert false
}

fn test_start_every_rejects_nil_job() {
	nil_job := unsafe { xasync.JobFn(nil) }
	xasync.start_every(context.background(), 1 * time.second, nil_job) or {
		assert err.msg() == 'async: job function is nil'
		return
	}
	assert false
}

fn test_start_every_returns_immediately_when_parent_is_already_canceled() {
	parent_ctx, cancel := xasync.with_cancel()
	cancel()
	xasync.start_every(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_periodic_handle_stop_before_first_iteration() {
	ran := chan bool{cap: 1}
	mut handle := xasync.start_every(context.background(), 1 * time.second, fn [ran] (mut ctx context.Context) ! {
		_ = ctx
		ran <- true
	})!
	handle.stop()
	handle.wait()!
	assert_no_periodic_signal(ran, 'periodic handle ran before first interval after stop')
}

fn test_periodic_handle_stop_between_ticks() {
	ticks := chan bool{cap: 2}
	mut handle := xasync.start_every(context.background(), 5 * time.millisecond, fn [ticks] (mut ctx context.Context) ! {
		_ = ctx
		ticks <- true
	})!
	wait_for_periodic_entry(ticks)
	handle.stop()
	handle.wait()!
	assert_no_periodic_signal(ticks, 'periodic handle ticked again after stop and wait')
}

fn test_periodic_handle_stop_during_long_job_waits_for_job_to_return() {
	entered := chan bool{cap: 1}
	release := chan bool{cap: 1}
	waited := chan bool{cap: 1}
	mut handle := xasync.start_every(context.background(), 5 * time.millisecond, fn [entered, release] (mut ctx context.Context) ! {
		_ = ctx
		entered <- true
		_ := <-release
	})!
	wait_for_periodic_entry(entered)
	handle.stop()

	wait_thread := spawn fn [mut handle, waited] () {
		handle.wait() or {
			waited <- false
			return
		}
		waited <- true
	}()
	assert_no_periodic_signal(waited, 'periodic handle wait returned before long job completed')
	release <- true
	wait_for_periodic_entry(waited)
	wait_thread.wait()
}

fn test_periodic_handle_wait_returns_job_error() {
	mut handle := xasync.start_every(context.background(), 5 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
		return error('periodic handle failed')
	})!
	handle.wait() or {
		assert err.msg() == 'periodic handle failed'
		return
	}
	assert false
}

fn test_periodic_handle_wait_returns_job_context_canceled_error_after_stop() {
	entered := chan bool{cap: 1}
	release := chan bool{cap: 1}
	mut handle := xasync.start_every(context.background(), 5 * time.millisecond, fn [entered, release] (mut ctx context.Context) ! {
		_ = ctx
		entered <- true
		_ := <-release
		return error('context canceled')
	})!
	wait_for_periodic_entry(entered)
	handle.stop()
	release <- true
	handle.wait() or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false, 'job-returned context canceled error was treated as normal stop'
}

fn test_periodic_handle_wait_returns_parent_cancellation() {
	parent_ctx, cancel := xasync.with_cancel()
	mut handle := xasync.start_every(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
	})!
	cancel()
	handle.wait() or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_periodic_handle_stop_after_parent_cancel_keeps_parent_error() {
	parent_ctx, cancel := xasync.with_cancel()
	mut handle := xasync.start_every(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
	})!
	cancel()
	handle.stop()
	handle.wait() or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_periodic_handle_does_not_overlap_iterations() {
	active := chan bool{cap: 1}
	active <- true
	entered := chan bool{cap: 2}
	release := chan bool{cap: 2}
	overlap := chan bool{cap: 1}
	mut handle := xasync.start_every(context.background(), 5 * time.millisecond, fn [active, entered, release, overlap] (mut ctx context.Context) ! {
		select {
			_ := <-active {}
			else {
				overlap <- true
				return error('periodic handle overlap')
			}
		}
		entered <- true
		_ = ctx
		_ := <-release
		active <- true
	})!

	wait_for_periodic_entry(entered)
	assert_no_periodic_signal(entered,
		'periodic handle iterations overlapped while first job was still running')
	select {
		did_overlap := <-overlap {
			assert !did_overlap
		}
		else {}
	}

	release <- true
	wait_for_periodic_entry(entered)
	handle.stop()
	release <- true
	handle.wait()!
	assert_no_periodic_signal(entered, 'periodic handle started another iteration after stop')
}

fn test_periodic_handle_double_stop_and_wait() {
	mut handle := xasync.start_every(context.background(), 1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
	})!
	handle.stop()
	handle.stop()
	handle.wait()!
	handle.wait() or {
		assert err.msg() == 'async: periodic wait was already called'
		return
	}
	assert false
}

fn wait_for_periodic_entry(entered chan bool) {
	select {
		did_enter := <-entered {
			assert did_enter
		}
		1 * time.second {
			assert false, 'periodic job did not enter'
		}
	}
}

fn assert_no_periodic_signal(signal chan bool, message string) {
	select {
		_ := <-signal {
			assert false, message
		}
		50 * time.millisecond {}
	}
}
