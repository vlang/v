// vtest retry: 3
module context

import time

fn wait_for_cause_cancellation(mut ctx Context) {
	done := ctx.done()
	select {
		_ := <-done {
		}
		2 * time.second {
			assert false, 'the context did not signal cancellation'
		}
	}
}

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
	wait_for_cause_cancellation(mut ctx)

	assert ctx.err().str() == 'context canceled'
}

fn test_with_timeout_cause_fires_cause() {
	my_cause := error('timed out for testing')

	mut bg := background()
	timeout := 30 * time.millisecond
	earliest_deadline := time.now().add(timeout)
	mut ctx, cancel := with_timeout_cause(mut bg, timeout, my_cause)
	latest_deadline := time.now().add(timeout)
	defer {
		cancel()
	}

	if deadline := ctx.deadline() {
		assert deadline >= earliest_deadline && deadline <= latest_deadline
	} else {
		assert false, 'the context did not record a relative deadline'
	}
	initial_error := ctx.err()
	if time.now() < earliest_deadline {
		assert initial_error is none
	}

	wait_for_cause_cancellation(mut ctx)

	assert ctx.err().str() == 'context deadline exceeded'
	assert cause(ctx).str() == 'timed out for testing'
}

fn test_with_timeout_cause_cancel_before_deadline() {
	my_cause := error('deadline cause')

	mut bg := background()
	mut ctx, cancel := with_timeout_cause(mut bg, 500 * time.millisecond, my_cause)

	// cancel before deadline fires
	cancel()
	wait_for_cause_cancellation(mut ctx)

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

fn assert_deadline_is(ctx Context, expected time.Time) {
	if got := ctx.deadline() {
		assert got.unix() == expected.unix(), 'deadline was ${got.unix()}, want ${expected.unix()}'
		assert got.str() == expected.str(), 'deadline was ${got.str()}, want ${expected.str()}'
	} else {
		assert false, 'expected a deadline of ${expected.str()}'
	}
}

fn test_with_deadline_cause_reports_the_deadline() {
	deadline := time.now().add(1 * time.hour)

	mut bg := background()
	mut ctx, cancel := with_deadline_cause(mut bg, deadline, error('never fires'))
	defer {
		cancel()
	}

	assert_deadline_is(ctx, deadline)
	assert ctx.err() is none
	assert cause(ctx) is none
}

fn test_with_deadline_cause_records_the_cause_when_the_deadline_fires() {
	my_cause := error('deadline cause from with_deadline_cause')

	mut bg := background()
	mut ctx, cancel := with_deadline_cause(mut bg, time.now().add(30 * time.millisecond),
		my_cause)
	defer {
		cancel()
	}

	wait_for_cause_cancellation(mut ctx)

	assert ctx.err().str() == 'context deadline exceeded'
	assert cause(ctx).str() == 'deadline cause from with_deadline_cause'
}

fn test_with_deadline_cause_cancelled_before_the_deadline_keeps_the_generic_cause() {
	my_cause := error('deadline cause from with_deadline_cause')

	mut bg := background()
	mut ctx, cancel := with_deadline_cause(mut bg, time.now().add(1 * time.hour),
		my_cause)

	cancel()
	wait_for_cause_cancellation(mut ctx)

	assert ctx.err().str() == 'context canceled'
	// The cause is only recorded when the deadline itself fires.
	assert cause(ctx).str() == 'context canceled'
}

fn test_with_deadline_cause_reports_an_already_lapsed_deadline() {
	my_cause := error('deadline already in the past')

	mut bg := background()
	mut ctx, cancel := with_deadline_cause(mut bg, time.now().add(-1 * time.second), my_cause)
	defer {
		cancel()
	}

	assert ctx.err().str() == 'context deadline exceeded'
	assert cause(ctx).str() == 'deadline already in the past'
}

// A parent deadline closer than the requested one wins, so the child is a plain
// cancel context and reports no deadline of its own.
fn test_with_deadline_cause_falls_back_to_a_cancel_context() {
	mut bg := background()
	parent_deadline := time.now().add(1 * time.hour)
	mut parent, parent_cancel := with_deadline(mut bg, parent_deadline)
	mut ctx, cancel := with_deadline_cause(mut parent, time.now().add(2 * time.hour),
		error('unused cause'))
	defer {
		cancel()
	}
	parent_cancel()

	if _ := ctx.deadline() {
		assert false, 'expected the fallback context to report no deadline'
	}
}

fn test_with_deadline_cause_is_cancelled_by_its_parent() {
	mut bg := background()
	mut parent, parent_cancel := with_cancel(mut bg)
	mut ctx, _ := with_deadline_cause(mut parent, time.now().add(1 * time.hour),
		error('unused cause'))

	assert ctx.err() is none

	parent_cancel()
	wait_for_cause_cancellation(mut ctx)

	assert ctx.err().str() == 'context canceled'
}
