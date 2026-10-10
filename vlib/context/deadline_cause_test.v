module context

import time

fn wait_until_canceled(mut ctx Context, budget time.Duration) {
	deadline := time.now().add(budget)
	for ctx.err() is none && time.now() < deadline {
		time.sleep(5 * time.millisecond)
	}
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
	deadline := time.now().add(1 * time.second)

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

	assert ctx.err() is none
	assert cause(ctx) is none

	wait_until_canceled(mut ctx, 2 * time.second)

	assert ctx.err().str() == 'context deadline exceeded'
	assert cause(ctx).str() == 'deadline cause from with_deadline_cause'
}

fn test_with_deadline_cause_cancelled_before_the_deadline_keeps_the_generic_cause() {
	my_cause := error('deadline cause from with_deadline_cause')

	mut bg := background()
	mut ctx, cancel := with_deadline_cause(mut bg, time.now().add(500 * time.millisecond),
		my_cause)

	cancel()
	wait_until_canceled(mut ctx, 2 * time.second)

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
	parent_deadline := time.now().add(1 * time.second)
	mut parent, parent_cancel := with_deadline(mut bg, parent_deadline)
	mut ctx, cancel := with_deadline_cause(mut parent, time.now().add(1 * time.hour),
		error('unused cause'))
	defer {
		cancel()
	}
	parent_cancel()

	if _ := ctx.deadline() {
		assert false, 'expected the fallback context to report no deadline'
	}
}

fn test_with_timeout_cause_reports_its_cause_relative_to_the_timeout() {
	my_cause := error('relative timeout cause')

	mut bg := background()
	mut ctx, cancel := with_timeout_cause(mut bg, 30 * time.millisecond, my_cause)
	defer {
		cancel()
	}

	wait_until_canceled(mut ctx, 2 * time.second)

	assert ctx.err().str() == 'context deadline exceeded'
	assert cause(ctx).str() == 'relative timeout cause'
}

fn test_with_deadline_cause_is_cancelled_by_its_parent() {
	mut bg := background()
	mut parent, parent_cancel := with_cancel(mut bg)
	mut ctx, _ := with_deadline_cause(mut parent, time.now().add(1 * time.hour),
		error('unused cause'))

	assert ctx.err() is none

	parent_cancel()
	wait_until_canceled(mut ctx, 2 * time.second)

	assert ctx.err().str() == 'context canceled'
}
