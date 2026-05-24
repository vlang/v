module async

import context
import sync

struct TaskResult[T] {
	value T
	err   IError = none
}

// Task represents one concurrent computation that produces either a value or an error.
//
// A Task is intentionally small: it starts one `spawn`, stores one result in a
// bounded channel, and lets the owner consume that result with wait(). It does
// not recover panics and it does not kill work that ignores cancellation.
@[heap]
pub struct Task[T] {
mut:
	ctx       context.Context
	cancel    context.CancelFn = unsafe { nil }
	result_ch chan TaskResult[T]
	// Guards the one-shot wait contract. The result channel carries the actual
	// value/error, so no additional shared result state is needed.
	mutex  &sync.Mutex = sync.new_mutex()
	waited bool
}

// run starts f with a background context and returns a Task for its result.
//
// The returned Task owns a derived context that is canceled when f finishes.
pub fn run[T](f TaskFn[T]) !&Task[T] {
	return run_with_context[T](context.background(), f)
}

// run_with_context starts f with a context derived from parent.
//
// Cancellation is cooperative. If parent is canceled, f must observe ctx.done()
// and return. The result channel is buffered so f can publish its single result
// even if the owner has not called wait() yet.
pub fn run_with_context[T](parent context.Context, f TaskFn[T]) !&Task[T] {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	ctx, cancel := new_cancel_context(parent)
	mut task := &Task[T]{
		ctx:       context.Context(ctx)
		cancel:    cancel
		result_ch: chan TaskResult[T]{cap: 1}
		mutex:     sync.new_mutex()
	}
	spawn fn [T](task &Task[T], f TaskFn[T]) {
		mut job_ctx := task.ctx
		value := f(mut job_ctx) or {
			task.result_ch <- TaskResult[T]{
				err: err
			}
			task.cancel()
			return
		}
		task.result_ch <- TaskResult[T]{
			value: value
		}
		task.cancel()
	}(task, f)
	return task
}

// wait blocks until the task publishes its result, then returns the value or error.
//
// wait is one-shot. A second call returns a stable error instead of blocking on
// an already-consumed result channel.
pub fn (mut task Task[T]) wait() !T {
	task.mutex.lock()
	if task.waited {
		task.mutex.unlock()
		return error(err_task_wait_called)
	}
	task.waited = true
	task.mutex.unlock()

	result := <-task.result_ch
	if result.err !is none {
		return result.err
	}
	return result.value
}
