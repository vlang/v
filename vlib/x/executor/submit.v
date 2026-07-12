module executor

import context
import sync
import time

// try_post accepts f if the executor is open and queue space is available.
//
// It never waits for queue capacity. A full queue returns
// `executor: queue is full`, making backpressure explicit.
pub fn (mut e Executor) try_post(f JobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	e.enqueue_job_now(ExecutorJob{
		f:            f
		report_error: true
	})!
}

// post_with_context waits until f can be admitted, parent is canceled, or the executor closes.
//
// The parent context bounds admission only. Once accepted, f will run on a
// future owner pump unless the application never pumps the executor.
// Active-owner submissions are accepted while capacity is available. If they
// would need to wait for capacity, they fail with
// `executor: owner thread cannot wait for queue capacity`.
pub fn (mut e Executor) post_with_context(parent context.Context, f JobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	e.enqueue_job_with_context(parent, ExecutorJob{
		f:            f
		report_error: true
	})!
}

// post_with_timeout waits up to timeout for f to be admitted.
//
// The timeout bounds admission only and does not preempt a running callback.
// Active-owner submissions are accepted while capacity is available. If they
// would need to wait for capacity, they fail with
// `executor: owner thread cannot wait for queue capacity`.
pub fn (mut e Executor) post_with_timeout(timeout time.Duration, f JobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	if timeout <= 0 {
		return error(err_timeout)
	}
	e.enqueue_job_with_timeout(time.now().add(timeout), ExecutorJob{
		f:            f
		report_error: true
	})!
}

fn (mut e Executor) enqueue_job_now(job ExecutorJob) ! {
	e.mutex.lock()
	if !e.accepting {
		e.mutex.unlock()
		return error(err_executor_closed)
	}
	if e.queue.len >= e.queue_size {
		e.mutex.unlock()
		return error(err_queue_full)
	}
	e.queue << job
	e.wake_job_ready_locked()
	e.mutex.unlock()
}

fn (mut e Executor) enqueue_job_with_context(parent context.Context, job ExecutorJob) ! {
	thread_id := sync.thread_id()
	mut submit_ctx := parent
	initial_err := submit_ctx.err()
	if initial_err !is none {
		return initial_err
	}
	done := submit_ctx.done()
	mut watch_done := true
	select {
		_ := <-done {
			err := submit_ctx.err()
			if err !is none {
				return err
			}
			watch_done = false
		}
		else {}
	}
	for {
		e.mutex.lock()
		if !e.accepting {
			e.mutex.unlock()
			return error(err_executor_closed)
		}
		if watch_done {
			err := submit_ctx.err()
			if err !is none {
				e.mutex.unlock()
				return err
			}
		}
		if e.queue.len < e.queue_size {
			e.queue << job
			e.wake_job_ready_locked()
			e.mutex.unlock()
			return
		}
		if e.owner_active && e.owner_thread_id == thread_id {
			e.mutex.unlock()
			return error(err_owner_submit_wait)
		}
		submit_ready := e.submit_ready
		e.mutex.unlock()
		if !watch_done {
			_ := <-submit_ready
			continue
		}
		select {
			_ := <-submit_ready {
				continue
			}
			_ := <-done {
				err := submit_ctx.err()
				if err !is none {
					return err
				}
				watch_done = false
				continue
			}
		}
	}
}

fn (mut e Executor) enqueue_job_with_timeout(deadline time.Time, job ExecutorJob) ! {
	thread_id := sync.thread_id()
	for {
		e.mutex.lock()
		if !e.accepting {
			e.mutex.unlock()
			return error(err_executor_closed)
		}
		remaining := deadline - time.now()
		if remaining <= 0 {
			e.mutex.unlock()
			return error(err_timeout)
		}
		if e.queue.len < e.queue_size {
			e.queue << job
			e.wake_job_ready_locked()
			e.mutex.unlock()
			return
		}
		if e.owner_active && e.owner_thread_id == thread_id {
			e.mutex.unlock()
			return error(err_owner_submit_wait)
		}
		submit_ready := e.submit_ready
		e.mutex.unlock()
		select {
			_ := <-submit_ready {
				continue
			}
			remaining {
				return error(err_timeout)
			}
		}
	}
}
