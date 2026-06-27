module executor

import context
import sync

// run_sync runs f on the owner thread and waits until it finishes.
//
// Calls from the active owner pump execute inline, outside executor mutexes, to
// avoid deadlock. Inline calls still require open admission; an inline error is
// stored as the executor's first job error even if the outer callback catches it.
// That inline execution is outside FIFO queue ordering.
pub fn (mut e Executor) run_sync(f JobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	thread_id := sync.thread_id()
	e.mutex.lock()
	is_owner := e.owner_active && e.owner_thread_id == thread_id
	accepting := e.accepting
	e.mutex.unlock()
	if is_owner {
		if !accepting {
			return error(err_executor_closed)
		}
		f() or {
			e.set_first_error(err)
			return err
		}
		return
	}

	result_ch := chan ExecutorJobResult{cap: 1}
	job := ExecutorJob{
		f:            f
		result_ch:    result_ch
		has_result:   true
		report_error: true
	}
	e.enqueue_job_with_context(context.background(), job)!
	result := <-result_ch
	if result.err !is none {
		return result.err
	}
}
