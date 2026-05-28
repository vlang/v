import context
import time
import x.async as xasync

fn test_task_returns_value() {
	mut task := xasync.run[int](fn (mut ctx context.Context) !int {
		_ = ctx
		return 42
	})!
	assert task.wait()! == 42
}

fn test_task_returns_error() {
	mut task := xasync.run[int](fn (mut ctx context.Context) !int {
		_ = ctx
		return error('task failed')
	})!
	task.wait() or {
		assert err.msg() == 'task failed'
		return
	}
	assert false
}

fn test_task_wait_is_one_shot() {
	mut task := xasync.run[string](fn (mut ctx context.Context) !string {
		_ = ctx
		return 'ok'
	})!
	assert task.wait()! == 'ok'
	task.wait() or {
		assert err.msg() == 'async: task wait was already called'
		return
	}
	assert false
}

fn test_task_concurrent_wait_is_one_shot_without_deadlock() {
	release := chan bool{cap: 1}
	results := chan string{cap: 2}
	mut task := xasync.run[int](fn [release] (mut ctx context.Context) !int {
		_ = ctx
		_ := <-release
		return 7
	})!

	first_waiter := spawn fn [mut task, results] () {
		value := task.wait() or {
			results <- 'error:${err.msg()}'
			return
		}
		results <- 'value:${value}'
	}()
	second_waiter := spawn fn [mut task, results] () {
		value := task.wait() or {
			results <- 'error:${err.msg()}'
			return
		}
		results <- 'value:${value}'
	}()
	release <- true

	mut saw_value := false
	mut saw_second_wait_error := false
	for _ in 0 .. 2 {
		select {
			result := <-results {
				match result {
					'value:7' {
						saw_value = true
					}
					'error:async: task wait was already called' {
						saw_second_wait_error = true
					}
					else {
						assert false, 'unexpected concurrent wait result: ${result}'
					}
				}
			}
			1 * time.second {
				assert false, 'concurrent task waiters did not both finish'
			}
		}
	}
	first_waiter.wait()
	second_waiter.wait()

	assert saw_value
	assert saw_second_wait_error
}

fn test_task_parent_cancellation_is_observed() {
	parent_ctx, cancel := xasync.with_cancel()
	mut task := xasync.run_with_context[int](parent_ctx, fn (mut ctx context.Context) !int {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return error('task did not observe parent cancellation')
			}
		}
		return error('unreachable')
	})!
	cancel()
	task.wait() or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_task_parent_already_canceled_is_observed() {
	parent_ctx, cancel := xasync.with_cancel()
	cancel()
	mut task := xasync.run_with_context[int](parent_ctx, fn (mut ctx context.Context) !int {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return error('task did not observe already canceled parent')
			}
		}
		return error('unreachable')
	})!
	task.wait() or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_task_observes_context_done() {
	parent_ctx, cancel := xasync.with_cancel()
	observed := chan bool{cap: 1}
	mut task := xasync.run_with_context[int](parent_ctx, fn [observed] (mut ctx context.Context) !int {
		done := ctx.done()
		select {
			_ := <-done {
				observed <- true
				return ctx.err()
			}
			1 * time.second {
				observed <- false
				return error('task did not observe ctx.done()')
			}
		}
		return error('unreachable')
	})!
	cancel()
	task.wait() or { assert err.msg() == 'context canceled' }
	select {
		did_observe := <-observed {
			assert did_observe
		}
		1 * time.second {
			assert false, 'task did not report cancellation observation'
		}
	}
}

fn test_task_ignores_cancellation_but_result_publication_is_buffered() {
	parent_ctx, cancel := xasync.with_cancel()
	mut task := xasync.run_with_context[int](parent_ctx, fn (mut ctx context.Context) !int {
		_ = ctx
		time.sleep(20 * time.millisecond)
		return 9
	})!
	cancel()
	time.sleep(60 * time.millisecond)
	assert task.wait()! == 9
}

fn test_task_refuses_nil_function() {
	nil_task_fn := unsafe { xasync.TaskFn[int](nil) }
	xasync.run[int](nil_task_fn) or {
		assert err.msg() == 'async: job function is nil'
		return
	}
	assert false
}
