module async

import context
import sync

// Group coordinates a set of related concurrent jobs.
//
// Jobs share a derived context. The first job error is returned by wait() and
// cancels the shared context so sibling jobs can stop cooperatively.
@[heap]
pub struct Group {
mut:
	ctx    context.Context
	cancel context.CancelFn = unsafe { nil }
	wg     &sync.WaitGroup  = sync.new_waitgroup()
	// Protects both WaitGroup lifecycle state and first_err. WaitGroup.add()
	// must not race with WaitGroup.wait(), so go() and wait() share this lock.
	mutex &sync.Mutex = sync.new_mutex()
	// Stored once. Later job errors never replace the first failure observed by
	// the group, even when several jobs fail concurrently after cancellation.
	first_err IError = none
	// Set before wait() calls wg.wait(). Once true, no further jobs are accepted.
	waiting bool
}

// new_group creates a Group with a shared cancellable context derived from parent.
//
// The parent is accepted by value to keep the public call site simple. The
// derived context is owned by the group and canceled on first job error or when
// wait() completes.
pub fn new_group(parent context.Context) &Group {
	ctx, cancel := new_cancel_context(parent)
	return &Group{
		ctx:    context.Context(ctx)
		cancel: cancel
		wg:     sync.new_waitgroup()
		mutex:  sync.new_mutex()
	}
}

// go starts f in a new concurrent task.
//
// Calling go after wait has started returns an error. The task should not
// panic; panics in spawned work are not recovered by x.async.
pub fn (mut g Group) go(f JobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	g.mutex.lock()
	if g.waiting {
		g.mutex.unlock()
		return error(err_group_go_after_wait)
	}
	// add() happens while holding the same mutex that guards wait(), preventing
	// callers from triggering sync.WaitGroup's add-while-waiting misuse panic.
	g.wg.add(1)
	g.mutex.unlock()
	spawn run_group_job(mut g, f)
}

// wait blocks until all accepted group jobs finish.
//
// It returns the first job error, if any. wait may be called once; after it
// starts, the group no longer accepts new jobs.
pub fn (mut g Group) wait() ! {
	g.mutex.lock()
	if g.waiting {
		g.mutex.unlock()
		return error(err_group_wait_called)
	}
	g.waiting = true
	g.mutex.unlock()

	g.wg.wait()
	// Always cancel after all jobs finish to release the derived context and to
	// make the lifecycle symmetric with context.with_cancel().
	g.cancel()
	err := g.get_first_error()
	if err !is none {
		return err
	}
}

fn run_group_job(mut g Group, f JobFn) {
	defer {
		g.wg.done()
	}
	// Each job gets its own local mutable interface value. The underlying
	// context is shared and synchronized by the context module.
	mut job_ctx := g.ctx
	f(mut job_ctx) or { g.set_first_error(err) }
}

fn (mut g Group) set_first_error(err IError) {
	mut should_cancel := false
	g.mutex.lock()
	if g.first_err is none {
		g.first_err = err
		should_cancel = true
	}
	g.mutex.unlock()
	if should_cancel {
		// Cancel outside the group mutex. context cancellation can notify child
		// contexts, so keeping our own lock out of that path avoids lock nesting.
		g.cancel()
	}
}

fn (mut g Group) get_first_error() IError {
	g.mutex.lock()
	err := g.first_err
	g.mutex.unlock()
	return err
}
