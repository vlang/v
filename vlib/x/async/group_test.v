import context
import time
import x.async as xasync

fn test_with_cancel_sets_error_and_closes_done() {
	mut ctx, cancel := xasync.with_cancel()
	cancel()
	done := ctx.done()
	select {
		_ := <-done {
			assert ctx.err().msg() == 'context canceled'
		}
		1 * time.second {
			assert false, 'cancel did not close the context done channel'
		}
	}
}

fn test_group_success() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	done := chan int{cap: 2}
	group.go(fn [done] (mut ctx context.Context) ! {
		_ = ctx
		done <- 1
	})!
	group.go(fn [done] (mut ctx context.Context) ! {
		_ = ctx
		done <- 2
	})!
	group.wait()!
	assert (<-done) + (<-done) == 3
}

fn test_group_error_returns_first_error() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('group failed')
	})!
	group.wait() or {
		assert err.msg() == 'group failed'
		return
	}
	assert false
}

fn test_group_default_does_not_collect_errors() {
	mut group := xasync.new_group(context.background())
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('group failed')
	})!
	group.wait() or {
		assert err.msg() == 'group failed'
		assert group.errors().len == 0
		return
	}
	assert false
}

fn test_group_rejects_invalid_error_collection_config() {
	xasync.new_group_with_config(context.background(), collect_errors: true, max_errors: 0) or {
		assert err.msg() == 'async: group max_errors must be positive'
		return
	}
	assert false
}

fn test_group_wait_without_tasks() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	group.wait()!
}

fn test_group_refuses_go_after_wait() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	group.wait()!
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'async: group does not accept new tasks after wait starts'
		return
	}
	assert false
}

fn test_group_refuses_second_wait() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	group.wait()!
	group.wait() or {
		assert err.msg() == 'async: group wait was already called'
		return
	}
	assert false
}

fn test_group_refuses_nil_job() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	nil_job := unsafe { xasync.JobFn(nil) }
	group.go(nil_job) or {
		assert err.msg() == 'async: job function is nil'
		return
	}
	assert false
}

fn test_group_cancels_siblings_cooperatively() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	cancelled := chan bool{cap: 1}
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('stop siblings')
	})!
	group.go(fn [cancelled] (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				cancelled <- true
			}
			1 * time.second {
				cancelled <- false
			}
		}
	})!
	group.wait() or { assert err.msg() == 'stop siblings' }
	assert <-cancelled
}

fn test_group_first_error_remains_stable_with_concurrent_secondary_errors() {
	parent := context.background()
	mut group := xasync.new_group(parent)
	secondary_ready := chan bool{cap: 8}
	for _ in 0 .. 8 {
		group.go(fn [secondary_ready] (mut ctx context.Context) ! {
			secondary_ready <- true
			done := ctx.done()
			select {
				_ := <-done {
					return error('secondary error after cancellation')
				}
				1 * time.second {
					return error('secondary error timeout')
				}
			}
		})!
	}
	for _ in 0 .. 8 {
		assert <-secondary_ready
	}
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('primary error')
	})!
	group.wait() or {
		assert err.msg() == 'primary error'
		return
	}
	assert false
}

fn test_group_collects_errors_without_changing_wait_error() {
	mut group := xasync.new_group_with_config(context.background(),
		collect_errors: true
		max_errors:     4
	)!
	secondary_ready := chan bool{cap: 3}
	for i in 0 .. 3 {
		group.go(fn [secondary_ready, i] (mut ctx context.Context) ! {
			secondary_ready <- true
			done := ctx.done()
			select {
				_ := <-done {
					return error('secondary collected error ${i}')
				}
				1 * time.second {
					return error('secondary timeout ${i}')
				}
			}
		})!
	}
	for _ in 0 .. 3 {
		assert <-secondary_ready
	}
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('primary collected error')
	})!
	group.wait() or {
		assert err.msg() == 'primary collected error'
		errs := group.errors()
		assert errs.len == 4
		assert error_messages_contain(errs, 'primary collected error')
		return
	}
	assert false
}

fn test_group_error_collection_is_bounded_and_includes_first_error() {
	mut group := xasync.new_group_with_config(context.background(),
		collect_errors: true
		max_errors:     2
	)!
	secondary_ready := chan bool{cap: 4}
	for i in 0 .. 4 {
		group.go(fn [secondary_ready, i] (mut ctx context.Context) ! {
			secondary_ready <- true
			done := ctx.done()
			select {
				_ := <-done {
					return error('bounded secondary error ${i}')
				}
				1 * time.second {
					return error('bounded secondary timeout ${i}')
				}
			}
		})!
	}
	for _ in 0 .. 4 {
		assert <-secondary_ready
	}
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('bounded primary error')
	})!
	group.wait() or {
		assert err.msg() == 'bounded primary error'
		errs := group.errors()
		assert errs.len == 2
		assert error_messages_contain(errs, 'bounded primary error')
		return
	}
	assert false
}

fn test_group_errors_returns_snapshot_copy() {
	mut group := xasync.new_group_with_config(context.background(),
		collect_errors: true
		max_errors:     1
	)!
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('snapshot source error')
	})!
	group.wait() or {
		assert err.msg() == 'snapshot source error'
		mut snapshot := group.errors()
		assert snapshot.len == 1
		snapshot[0] = error('mutated snapshot error')
		second_snapshot := group.errors()
		assert second_snapshot.len == 1
		assert second_snapshot[0].msg() == 'snapshot source error'
		return
	}
	assert false
}

fn test_group_many_short_jobs_return_first_error() {
	jobs := 64
	mut group := xasync.new_group(context.background())
	done := chan int{cap: jobs}
	for i in 0 .. jobs {
		group.go(fn [done, i] (mut ctx context.Context) ! {
			_ = ctx
			done <- i
		})!
	}
	group.go(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('group stress failure')
	})!
	group.wait() or {
		assert err.msg() == 'group stress failure'
		mut seen := []bool{len: jobs}
		for _ in 0 .. jobs {
			select {
				i := <-done {
					assert i >= 0
					assert i < jobs
					assert !seen[i]
					seen[i] = true
				}
				1 * time.second {
					assert false, 'group short job did not finish'
				}
			}
		}
		for was_seen in seen {
			assert was_seen
		}
		return
	}
	assert false
}

fn test_group_parent_cancellation_is_observed_by_cooperative_job() {
	parent_ctx, cancel := xasync.with_cancel()
	mut group := xasync.new_group(parent_ctx)
	observed := chan string{cap: 1}
	group.go(fn [observed] (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				err := ctx.err()
				observed <- err.msg()
				return err
			}
			1 * time.second {
				observed <- 'not canceled'
				return error('parent cancellation was not observed')
			}
		}
	})!
	cancel()
	group.wait() or { assert err.msg() == 'context canceled' }
	select {
		msg := <-observed {
			assert msg == 'context canceled'
		}
		2 * time.second {
			assert false, 'cooperative group job did not observe parent cancellation'
		}
	}
}

fn error_messages_contain(errs []IError, msg string) bool {
	for err in errs {
		if err.msg() == msg {
			return true
		}
	}
	return false
}
