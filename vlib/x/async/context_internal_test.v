module async

import context
import time

fn test_async_context_cancel_closes_done_and_sets_error() {
	mut ctx, cancel := new_cancel_context(context.background())
	cancel()
	done := ctx.done()
	select {
		_ := <-done {
			assert ctx.err().msg() == context_canceled
		}
		1 * time.second {
			assert false, 'cancel did not close AsyncContext.done()'
		}
	}
	cancel()
	assert ctx.err().msg() == context_canceled
}

fn test_async_context_timeout_closes_done_and_sets_deadline_error() {
	mut ctx, cancel := new_timeout_context(context.background(), 20 * time.millisecond)
	defer {
		cancel()
	}
	done := ctx.done()
	select {
		_ := <-done {
			assert ctx.err().msg() == context_deadline_exceeded
		}
		1 * time.second {
			assert false, 'timeout did not close AsyncContext.done()'
		}
	}
}

fn test_timeout_result_detects_only_owned_deadline_miss() {
	mut ctx, cancel := new_timeout_context(context.background(), 1 * time.second)
	defer {
		cancel()
	}
	assert !TimeoutResult{
		finished_at: ctx.deadline_at.add(-1 * time.nanosecond)
	}.finished_after_owned_timeout(ctx)
	assert TimeoutResult{
		finished_at: ctx.deadline_at.add(1 * time.nanosecond)
	}.finished_after_owned_timeout(ctx)

	mut background := context.background()
	parent, parent_cancel := context.with_timeout(mut background, 1 * time.second)
	defer {
		parent_cancel()
	}
	mut child, child_cancel := new_timeout_context(parent, 2 * time.second)
	defer {
		child_cancel()
	}
	assert !TimeoutResult{
		finished_at: child.deadline_at.add(1 * time.nanosecond)
	}.finished_after_owned_timeout(child)
}

fn test_async_context_parent_cancel_propagates_to_child() {
	parent, parent_cancel := new_cancel_context(context.background())
	mut child, child_cancel := new_cancel_context(context.Context(parent))
	defer {
		child_cancel()
	}
	parent_cancel()
	done := child.done()
	select {
		_ := <-done {
			assert child.err().msg() == context_canceled
		}
		1 * time.second {
			assert false, 'parent cancellation did not reach child AsyncContext'
		}
	}
}
