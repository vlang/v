module executor

import sync

// run blocks on the caller thread and executes accepted jobs serially.
//
// It exits after stop() or the first job error closes admission and all accepted
// jobs have been drained.
pub fn (mut e Executor) run() ! {
	e.begin_owner_pump(true)!
	defer {
		e.end_owner_pump()
	}
	for {
		e.mutex.lock()
		job := e.pop_job_locked() or {
			if !e.accepting {
				err := e.finish_terminal_locked()
				e.mutex.unlock()
				if err !is none {
					return err
				}
				return
			}
			job_ready := e.job_ready
			e.mutex.unlock()
			_ := <-job_ready
			continue
		}
		e.mutex.unlock()
		e.execute_job(job)
	}
}

// run_one executes at most one accepted job and reports whether a job ran.
pub fn (mut e Executor) run_one() !bool {
	e.begin_owner_pump(false)!
	defer {
		e.end_owner_pump()
	}
	e.mutex.lock()
	job := e.pop_job_locked() or {
		if !e.accepting {
			err := e.finish_terminal_locked()
			e.mutex.unlock()
			if err !is none {
				return err
			}
			return false
		}
		e.mutex.unlock()
		return false
	}
	e.mutex.unlock()

	job_err := e.execute_job(job)
	terminal_err := e.finish_if_drained()
	if job_err !is none {
		return job_err
	}
	if terminal_err !is none {
		return terminal_err
	}
	return true
}

// drain_pending executes up to max_jobs jobs that are already accepted.
pub fn (mut e Executor) drain_pending(max_jobs int) !int {
	if max_jobs <= 0 {
		return error(err_drain_limit_invalid)
	}
	e.begin_owner_pump(false)!
	defer {
		e.end_owner_pump()
	}
	mut ran := 0
	mut first_err := IError(none)
	for ran < max_jobs {
		e.mutex.lock()
		job := e.pop_job_locked() or {
			if !e.accepting {
				err := e.finish_terminal_locked()
				e.mutex.unlock()
				if err !is none {
					return err
				}
				if first_err !is none {
					return first_err
				}
				return ran
			}
			e.mutex.unlock()
			if first_err !is none {
				return first_err
			}
			return ran
		}
		e.mutex.unlock()

		job_err := e.execute_job(job)
		ran++
		if job_err !is none && first_err is none {
			first_err = job_err
		}
	}
	terminal_err := e.finish_if_drained()
	if terminal_err !is none {
		return terminal_err
	}
	if first_err !is none {
		return first_err
	}
	return ran
}

// stop closes admission and wakes any blocked submitters or owner run loop.
//
// It is idempotent and non-blocking. Accepted jobs are drained by the owner
// pump; stop() itself never executes user callbacks.
pub fn (mut e Executor) stop() {
	e.mutex.lock()
	e.close_admission_locked()
	if e.queue.len == 0 && e.active == 0 {
		e.finish_terminal_locked()
	}
	e.mutex.unlock()
}

// wait waits for a running owner pump to reach terminal state.
//
// A valid wait is one-shot. Calling it from the owner callback, or before a
// terminal pump is active, returns a stable precondition error without consuming
// that one allowed wait.
pub fn (mut e Executor) wait() ! {
	thread_id := sync.thread_id()
	e.mutex.lock()
	if e.owner_active && e.owner_thread_id == thread_id {
		e.mutex.unlock()
		return error(err_wait_owner_thread)
	}
	if e.waited {
		e.mutex.unlock()
		return error(err_wait_called)
	}
	if e.stopped {
		e.waited = true
		err := e.first_err
		e.mutex.unlock()
		if err !is none {
			return err
		}
		return
	}
	if !e.run_active {
		e.mutex.unlock()
		return error(err_wait_before_terminal)
	}
	e.waited = true
	done := e.done
	e.mutex.unlock()

	_ := <-done
	err := e.get_first_error()
	if err !is none {
		return err
	}
}

fn (mut e Executor) finish_if_drained() IError {
	e.mutex.lock()
	if !e.accepting && e.queue.len == 0 && e.active == 0 {
		err := e.finish_terminal_locked()
		e.mutex.unlock()
		return err
	}
	e.mutex.unlock()
	return none
}
