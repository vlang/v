module executor

import sync

// JobFn is the function signature executed by an Executor on the owner thread.
pub type JobFn = fn () !

// ExecutorConfig configures a bounded owner-thread executor.
@[params]
pub struct ExecutorConfig {
pub:
	queue_size int
}

struct ExecutorJobResult {
mut:
	err IError = none
}

struct ExecutorJob {
	f            JobFn @[required]
	result_ch    chan ExecutorJobResult
	has_result   bool
	report_error bool
}

// Executor serializes accepted jobs on whichever thread actively pumps it.
//
// The executor does not own a thread. The owner identity is recorded only while
// run(), run_one(), or drain_pending() is actively pumping callbacks.
@[heap]
pub struct Executor {
mut:
	queue_size int
	queue      []ExecutorJob
	active     int

	mutex        &sync.Mutex = sync.new_mutex()
	job_ready    chan int
	submit_ready chan int
	done         chan int

	accepting bool = true
	stopped   bool
	waited    bool

	pumping    bool
	run_active bool

	owner_active    bool
	owner_thread_id u64

	first_err IError = none
}

// new creates an Executor with a fixed bounded queue.
//
// The owner thread is not captured at construction time. It is recorded only
// while an owner pump method is actively running.
pub fn new(config ExecutorConfig) !&Executor {
	if config.queue_size <= 0 {
		return error(err_queue_size_invalid)
	}
	return &Executor{
		queue_size:   config.queue_size
		queue:        []ExecutorJob{cap: config.queue_size}
		mutex:        sync.new_mutex()
		job_ready:    chan int{cap: 1}
		submit_ready: chan int{cap: 1}
		done:         chan int{cap: 1}
		accepting:    true
	}
}

fn (mut e Executor) pop_job_locked() ?ExecutorJob {
	if e.queue.len == 0 {
		return none
	}
	job := e.queue[0]
	e.queue.delete(0)
	e.active++
	e.wake_submitters_locked()
	return job
}

fn (mut e Executor) wake_job_ready_locked() {
	if !e.job_ready.closed {
		e.job_ready.close()
	}
	e.job_ready = chan int{cap: 1}
}

fn (mut e Executor) wake_submitters_locked() {
	if !e.submit_ready.closed {
		e.submit_ready.close()
	}
	if e.accepting {
		e.submit_ready = chan int{cap: 1}
	}
}

fn (mut e Executor) close_admission_locked() {
	if e.accepting {
		e.accepting = false
		e.wake_submitters_locked()
		e.wake_job_ready_locked()
	}
}

fn (mut e Executor) finish_terminal_locked() IError {
	if !e.stopped {
		e.stopped = true
		e.close_admission_locked()
		if !e.done.closed {
			e.done.close()
		}
	}
	return e.first_err
}

fn (mut e Executor) get_first_error() IError {
	e.mutex.lock()
	err := e.first_err
	e.mutex.unlock()
	return err
}

fn (mut e Executor) set_first_error(err IError) {
	e.mutex.lock()
	e.set_first_error_locked(err)
	e.mutex.unlock()
}

fn (mut e Executor) set_first_error_locked(err IError) {
	if e.first_err is none {
		e.first_err = err
	}
	e.close_admission_locked()
}

fn (mut e Executor) execute_job(job ExecutorJob) IError {
	defer {
		e.finish_job()
	}
	first_err_before := e.get_first_error()
	had_first_err := first_err_before !is none
	mut result := ExecutorJobResult{}
	job.f() or { result.err = err }
	if result.err is none && !had_first_err {
		err := e.get_first_error()
		if err !is none {
			result.err = err
		}
	}
	if result.err !is none && job.report_error {
		e.set_first_error(result.err)
	}
	if job.has_result {
		job.result_ch <- result
	}
	return result.err
}

fn (mut e Executor) finish_job() {
	e.mutex.lock()
	if e.active > 0 {
		e.active--
	}
	e.mutex.unlock()
}
