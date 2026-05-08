// vtest retry: 3
module context

import time

fn test_with_cancel_cause_no_cause() {
	mut bg := background()
	mut ctx, cancel := with_cancel_cause(mut bg)
	defer {
		cancel(none)
	}

	assert ctx.err() is none
	assert cause(ctx) is none

	cancel(none)
	assert ctx.err().str() == 'context canceled'
	// cause falls back to err() when no cause was set
	assert cause(ctx).str() == 'context canceled'
}

fn test_with_cancel_cause_with_cause() {
	my_err := error('my custom cause')

	mut bg := background()
	mut ctx, cancel := with_cancel_cause(mut bg)
	defer {
		cancel(none)
	}

	assert ctx.err() is none

	cancel(my_err)
	assert ctx.err().str() == 'context canceled'
	assert cause(ctx).str() == 'my custom cause'
}

fn test_with_cancel_cause_first_cause_wins() {
	first := error('first cause')
	second := error('second cause')

	mut bg := background()
	mut ctx, cancel := with_cancel_cause(mut bg)

	cancel(first)
	cancel(second) // should be ignored
	cancel(none) // should be ignored

	assert cause(ctx).str() == 'first cause'
}

fn test_with_cancel_cause_parent_cancel_propagates() {
	mut bg := background()
	mut parent, parent_cancel := with_cancel(mut bg)
	mut ctx, _ := with_cancel_cause(mut parent)

	assert ctx.err() is none

	parent_cancel()
	// give goroutine a moment to propagate
	time.sleep(5 * time.millisecond)

	assert ctx.err().str() == 'context canceled'
}

fn test_with_timeout_cause_fires_cause() {
	my_cause := error('timed out for testing')

	mut bg := background()
	mut ctx, cancel := with_timeout_cause(mut bg, 30 * time.millisecond, my_cause)
	defer {
		cancel()
	}

	// not done yet
	assert ctx.err() is none

	// wait for timeout
	time.sleep(60 * time.millisecond)

	assert ctx.err().str() == 'context deadline exceeded'
	assert cause(ctx).str() == 'timed out for testing'
}

fn test_with_timeout_cause_cancel_before_deadline() {
	my_cause := error('deadline cause')

	mut bg := background()
	mut ctx, cancel := with_timeout_cause(mut bg, 500 * time.millisecond, my_cause)

	// cancel before deadline fires
	cancel()
	time.sleep(5 * time.millisecond)

	assert ctx.err().str() == 'context canceled'
	// no cause was set via CancelCauseFunc; cause falls back to err()
	assert cause(ctx).str() == 'context canceled'
}

fn test_cause_on_plain_cancel_context() {
	// cause() on a non-cause context should just return ctx.err()
	mut bg := background()
	mut ctx, cancel := with_cancel(mut bg)
	defer {
		cancel()
	}

	assert cause(ctx) is none

	cancel()
	assert cause(ctx).str() == 'context canceled'
}
