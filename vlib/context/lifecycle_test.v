module context

import time

struct LifecycleParent {
	delegate Context
mut:
	signal chan int
}

fn (p &LifecycleParent) deadline() ?time.Time { return p.delegate.deadline() }

fn (p &LifecycleParent) value(key Key) ?Any { return p.delegate.value(key) }

fn (mut p LifecycleParent) done() chan int { return p.signal }

fn (mut p LifecycleParent) err() IError {
	if p.signal.closed { return error('external parent canceled') }
	return none
}

fn lifecycle_done(mut ctx Context) {
	done := ctx.done()
	mut ready := false
	select {
		_ := <-done {
			ready = true
		}
		5 * time.second {
		}
	}
	assert ready, 'context cancellation did not complete'
}

fn lifecycle_child_count(parent Context) int {
	value := parent.value(cancel_context_key) or { panic('missing cancel context') }
	match value {
		CancelContext {
			value.mutex.lock()
			count := value.children.len
			value.mutex.unlock()
			return count
		}
		else { panic('unexpected cancel context type') }
	}
}

fn lifecycle_parent_worker(parent Context, child Canceler, parent_done chan int, child_done chan int, finished chan bool) {
	wait_for_parent(parent, child, parent_done, child_done)
	finished <- true
}

fn lifecycle_deadline_worker(child Canceler, done chan int, finished chan bool) {
	wait_for_deadline(child, done, time.hour, time.sys_mono_now(), error('unused deadline cause'))
	finished <- true
}

fn test_known_never_cancelable_contexts_unwrap_only_value_contexts() {
	bg := background()
	assert parent_never_cancels(bg)
	assert parent_never_cancels(todo())
	assert parent_never_cancels(Context(&EmptyContext{}))
	wrapped := with_value(with_value(bg, Key('one'), 1), Key('two'), 2)
	assert parent_never_cancels(wrapped)
	mut external := Context(&LifecycleParent{ delegate: wrapped, signal: chan int{} })
	assert !parent_never_cancels(external)
	mut child, cancel := with_cancel(mut external)
	cancel()
	lifecycle_done(mut child)
}

fn test_fallback_parent_worker_finishes_when_child_is_canceled() {
	mut parent := Context(&LifecycleParent{ delegate: background(), signal: chan int{} })
	mut child := new_cancel_context(background())
	parent_done := parent.done()
	child_done := child.done()
	finished := chan bool{cap: 1}
	task := spawn lifecycle_parent_worker(parent, child, parent_done, child_done, finished)
	child.cancel(false, canceled)
	mut completed := false
	select {
		_ := <-finished {
			completed = true
		}
		5 * time.second {
		}
	}
	// Also release a broken parent-only worker, so failure does not leave it parked.
	if !completed { parent_done.close() }
	task.wait()
	assert completed, 'fallback propagation worker did not exit after child cancellation'
}

fn test_fallback_parent_worker_propagates_parent_error() {
	mut parent := Context(&LifecycleParent{ delegate: background(), signal: chan int{} })
	mut child := new_cancel_context(background())
	parent_done := parent.done()
	finished := chan bool{cap: 1}
	task := spawn lifecycle_parent_worker(parent, child, parent_done, child.done(), finished)
	parent_done.close()
	mut completed := false
	select {
		_ := <-finished {
			completed = true
		}
		5 * time.second {
		}
	}
	assert completed
	task.wait()
	assert child.err().msg() == 'external parent canceled'
	assert cause(child).msg() == 'external parent canceled'
}

fn test_fallback_propagation_retains_original_parent_after_caller_rebinds_interface() {
	original := &LifecycleParent{ delegate: background(), signal: chan int{} }
	mut parent := Context(original)
	mut child, cancel := with_cancel(mut parent)
	defer { cancel() }
	parent = background()
	original.signal.close()
	lifecycle_done(mut child)
	assert child.err().msg() == 'external parent canceled'
	assert cause(child).msg() == 'external parent canceled'
}

fn test_deadline_wait_worker_exits_before_its_long_deadline_when_canceled() {
	mut child := new_cancel_context(background())
	finished := chan bool{cap: 1}
	task := spawn lifecycle_deadline_worker(child, child.done(), finished)
	child.cancel(false, canceled)
	mut completed := false
	select {
		_ := <-finished {
			completed = true
		}
		5 * time.second {
		}
	}
	assert completed, 'deadline worker retained its hour-long wait after cancellation'
	task.wait()
	assert child.err().msg() == 'context canceled'
	assert cause(child).msg() == 'context canceled'
}

fn test_nil_first_cause_and_child_first_cancellation_cannot_be_overwritten() {
	mut bg := background()
	mut first, cancel_first := with_cancel_cause(mut bg)
	cancel_first(none)
	cancel_first(error('too late'))
	assert cause(first).msg() == 'context canceled'

	mut parent, cancel_parent := with_cancel_cause(mut bg)
	mut plain, cancel_plain := with_cancel(mut parent)
	mut specific, cancel_specific := with_cancel_cause(mut parent)
	cancel_plain()
	cancel_specific(error('child first'))
	cancel_parent(error('parent later'))
	assert cause(parent).msg() == 'parent later'
	assert cause(plain).msg() == 'context canceled'
	assert cause(specific).msg() == 'child first'
}

fn test_parent_cause_reaches_plain_and_cause_children_including_late_children() {
	mut bg := background()
	mut parent, cancel_parent := with_cancel_cause(mut bg)
	mut plain, cancel_plain := with_cancel(mut parent)
	mut specific, cancel_specific := with_cancel_cause(mut parent)
	defer {
		cancel_plain()
		cancel_specific(none)
	}
	cancel_parent(error('parent first'))
	lifecycle_done(mut plain)
	lifecycle_done(mut specific)
	assert cause(plain).msg() == 'parent first'
	assert cause(specific).msg() == 'parent first'
	mut late, cancel_late := with_cancel(mut parent)
	defer { cancel_late() }
	assert late.err().msg() == 'context canceled'
	assert cause(late).msg() == 'parent first'
}

fn test_cause_cancellation_unregisters_the_wrapper_id_from_parent() {
	mut bg := background()
	mut parent, cancel_parent := with_cancel(mut bg)
	defer { cancel_parent() }
	mut child, cancel_child := with_cancel_cause(mut parent)
	assert lifecycle_child_count(parent) == 1
	cancel_child(error('finished'))
	assert lifecycle_child_count(parent) == 0
	mut deadline_child, cancel_deadline := with_timeout_cause(mut parent, time.hour, error('unused'))
	assert lifecycle_child_count(parent) == 1
	cancel_deadline()
	assert lifecycle_child_count(parent) == 0
	assert child.err().msg() == 'context canceled'
	assert deadline_child.err().msg() == 'context canceled'
}

fn lifecycle_cancel_racer(start chan bool, cancel CancelCauseFunc, label string) {
	_ = <-start
	cancel(error(label))
}

fn test_concurrent_cancellation_keeps_one_winning_cause() {
	for _ in 0 .. 16 {
		mut bg := background()
		mut parent, cancel_parent := with_cancel_cause(mut bg)
		mut child, cancel_child := with_cancel_cause(mut parent)
		start := chan bool{}
		mut tasks := []thread{}
		tasks << spawn lifecycle_cancel_racer(start, cancel_parent, 'parent')
		tasks << spawn lifecycle_cancel_racer(start, cancel_child, 'child one')
		tasks << spawn lifecycle_cancel_racer(start, cancel_child, 'child two')
		start.close()
		tasks.wait()
		winner := cause(child).msg()
		assert winner in ['parent', 'child one', 'child two']
		assert child.err().msg() == 'context canceled'
		cancel_child(error('later'))
		assert cause(child).msg() == winner
		assert lifecycle_child_count(parent) == 0
	}
}

fn lifecycle_register_children(parent_value Context, start chan bool, finished chan bool) {
	_ = <-start
	mut parent := parent_value
	mut valid := true
	for _ in 0 .. 32 {
		mut child, cancel := with_cancel_cause(mut parent)
		cancel(error('child finished'))
		valid = valid && child.err().msg() == 'context canceled'
	}
	finished <- valid
}

fn test_parent_registration_and_removal_are_safe_during_parent_cancellation() {
	mut bg := background()
	mut parent, cancel_parent := with_cancel_cause(mut bg)
	start := chan bool{}
	finished := chan bool{cap: 4}
	mut tasks := []thread{}
	for _ in 0 .. 4 { tasks << spawn lifecycle_register_children(parent, start, finished) }
	start.close()
	cancel_parent(error('parent finished'))
	for _ in 0 .. 4 {
		mut completed := false
		select {
			valid := <-finished {
				completed = valid
			}
			5 * time.second {
			}
		}
		assert completed, 'concurrent registration worker did not complete'
	}
	tasks.wait()
	assert parent.err().msg() == 'context canceled'
	assert lifecycle_child_count(parent) == 0
}

fn lifecycle_publish_cause(start chan bool, cancel CancelCauseFunc) {
	_ = <-start
	cancel(error('published cause'))
}

fn test_concurrent_cause_reads_observe_none_or_the_winning_cause() {
	for _ in 0 .. 32 {
		mut bg := background()
		mut ctx, cancel := with_cancel_cause(mut bg)
		start := chan bool{}
		task := spawn lifecycle_publish_cause(start, cancel)
		start.close()
		for _ in 0 .. 128 {
			observed := cause(ctx)
			assert observed is none || observed.msg() == 'published cause'
		}
		task.wait()
		assert ctx.err().msg() == 'context canceled'
		assert cause(ctx).msg() == 'published cause'
	}
}
