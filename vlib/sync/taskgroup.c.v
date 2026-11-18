module sync

// TaskOutcome carries one finished task back to the group owner.
//
// Outcomes travel through a channel, so the waiting side observes every
// result without additional synchronization. wait reorders them by `idx`,
// giving submission order regardless of completion order.
struct TaskOutcome[T] {
	idx   int
	value T
	err   IError = none
}

// TaskState holds the cancellation flag shared by a group and its contexts.
@[heap]
struct TaskState {
mut:
	mu        &Mutex
	cancelled bool
}

// TaskContext is a lightweight handle for cooperative cancellation.
//
// A context is bound to the TaskGroup it comes from. Tasks observe
// cancellation by calling check_cancelled at suitable points and propagating
// the error with `!`. Cancellation is cooperative: work that never checks is
// never interrupted.
pub struct TaskContext {
	state &TaskState
}

// is_cancelled reports whether the group has been cancelled.
pub fn (c &TaskContext) is_cancelled() bool {
	c.state.mu.lock()
	defer {
		c.state.mu.unlock()
	}
	return c.state.cancelled
}

// check_cancelled returns an error when the group has been cancelled.
pub fn (c &TaskContext) check_cancelled() ! {
	if c.is_cancelled() {
		return error('sync: task cancelled')
	}
}

// TaskGroup runs a set of closures concurrently and joins them all.
//
// A group is single-use: spawn children, then wait once for their ordered
// results. The first child error cancels the group; siblings that observe
// cancellation should return early. wait returns the first error after every
// child has finished.
//
// A TaskGroup must be used through the reference returned by new_task_group.
// Spawning accepts a closure, matching `spawn` and `WaitGroup.go` usage.
@[heap]
pub struct TaskGroup[T] {
mut:
	wg        &WaitGroup
	mu        &Mutex
	outcomes  chan TaskOutcome[T]
	state     &TaskState
	count     int
	closed    bool
	first_err IError = none
}

// new_task_group creates an empty TaskGroup for tasks producing T.
pub fn new_task_group[T]() &TaskGroup[T] {
	return &TaskGroup[T]{
		wg:       new_waitgroup()
		mu:       new_mutex()
		outcomes: chan TaskOutcome[T]{cap: 1}
		state:    &TaskState{
			mu: new_mutex()
		}
	}
}

// context returns the cooperative-cancellation context for this group.
pub fn (g &TaskGroup[T]) context() TaskContext {
	return TaskContext{
		state: g.state
	}
}

// spawn runs f concurrently as part of the group.
//
// Each spawn takes the next submission index; wait reports results in that
// order. Spawning after wait has started is an error.
pub fn (mut g TaskGroup[T]) spawn(f fn () !T) ! {
	if f == unsafe { nil } {
		return error('sync: TaskGroup nil job')
	}
	g.mu.lock()
	if g.closed {
		g.mu.unlock()
		return error('sync: TaskGroup spawn after wait')
	}
	idx := g.count
	g.count++
	g.wg.add(1)
	g.mu.unlock()
	spawn fn [T](group &TaskGroup[T], idx int, f fn () !T) {
		mut gg := group
		value := f() or {
			gg.outcomes <- TaskOutcome[T]{
				idx: idx
				err: err
			}
			gg.note_error(err)
			gg.wg.done()
			return
		}
		gg.outcomes <- TaskOutcome[T]{
			idx:   idx
			value: value
		}
		gg.wg.done()
	}(mut g, idx, f)
}

// cancel cooperatively cancels the group.
//
// Tasks observe cancellation through TaskContext.check_cancelled. The first
// task error cancels the group automatically; cancel also allows user-driven
// early cancellation.
pub fn (mut g TaskGroup[T]) cancel() {
	g.state.mu.lock()
	defer {
		g.state.mu.unlock()
	}
	g.state.cancelled = true
}

// wait joins every spawned task and returns their results in submission order.
//
// wait may be called once; a second call is an error. When any task failed,
// wait returns the first observed error after all tasks have finished.
// Waiting on a group with no tasks returns an empty list.
pub fn (mut g TaskGroup[T]) wait() ![]T {
	g.mu.lock()
	if g.closed {
		g.mu.unlock()
		return error('sync: TaskGroup wait called twice')
	}
	g.closed = true
	count := g.count
	g.mu.unlock()
	mut values := []T{len: count}
	mut failed := false
	for _ in 0 .. count {
		outcome := <-g.outcomes
		if outcome.err !is none {
			failed = true
		} else {
			values[outcome.idx] = outcome.value
		}
	}
	g.wg.wait()
	g.cancel()
	if failed {
		g.mu.lock()
		defer {
			g.mu.unlock()
		}
		if g.first_err !is none {
			return g.first_err
		}
		return error('sync: TaskGroup task failed')
	}
	return values
}

// note_error records the first task error and cancels the group.
fn (mut g TaskGroup[T]) note_error(err IError) {
	mut should_cancel := false
	g.mu.lock()
	if g.first_err is none {
		g.first_err = err
		should_cancel = true
	}
	g.mu.unlock()
	if should_cancel {
		g.cancel()
	}
}
