module async

import context
import sync

// PoolConfig configures a fixed-size concurrency pool.
//
// Both values must be positive. `workers` is the maximum number of jobs that
// can execute concurrently; `queue_size` is the bounded backlog accepted by
// try_submit() while all worker slots are busy.
@[params]
pub struct PoolConfig {
pub:
	workers    int
	queue_size int
}

// Pool limits concurrent JobFn execution with fixed worker slots and bounded backlog.
//
// The pool owns one derived context shared by all jobs. Closing the pool stops
// new submissions, waits for every accepted job, and returns the first job error
// if any.
@[heap]
pub struct Pool {
mut:
	ctx      context.Context
	cancel   context.CancelFn = unsafe { nil }
	tokens   chan bool
	wg       &sync.WaitGroup = sync.new_waitgroup()
	max_jobs int
	// Protects lifecycle flags, accepted job count, WaitGroup.add(), and
	// first_err. This lock is never held while a user JobFn runs.
	mutex     &sync.Mutex = sync.new_mutex()
	first_err IError      = none
	closed    bool
	waited    bool
	accepted  int
}

// new_pool creates a Pool with a background parent context.
pub fn new_pool(config PoolConfig) !&Pool {
	return new_pool_with_context(context.background(), config)
}

// new_pool_with_context creates a Pool with a context derived from parent.
//
// The worker limit and queue size are fixed for the pool lifetime. Parent
// cancellation is cooperative: jobs must observe `ctx.done()` and return.
pub fn new_pool_with_context(parent context.Context, config PoolConfig) !&Pool {
	if config.workers <= 0 {
		return error(err_pool_workers_invalid)
	}
	if config.queue_size <= 0 {
		return error(err_pool_queue_size_invalid)
	}
	ctx, cancel := new_cancel_context(parent)
	mut pool := &Pool{
		ctx:      context.Context(ctx)
		cancel:   cancel
		tokens:   chan bool{cap: config.workers}
		wg:       sync.new_waitgroup()
		max_jobs: config.workers + config.queue_size
		mutex:    sync.new_mutex()
	}
	for _ in 0 .. config.workers {
		pool.tokens <- true
	}
	return pool
}

// try_submit accepts f if the pool is open and its bounded backlog has capacity.
//
// It never blocks for queue space. A full backlog returns `async: pool queue is
// full`, making backpressure explicit for callers.
pub fn (mut p Pool) try_submit(f JobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	p.mutex.lock()
	if p.closed {
		p.mutex.unlock()
		return error(err_pool_closed)
	}
	if p.accepted >= p.max_jobs {
		p.mutex.unlock()
		return error(err_pool_queue_full)
	}
	p.accepted++
	// add() is protected by the same mutex as wait(), so callers cannot race an
	// accepted job against pool shutdown.
	p.wg.add(1)
	p.mutex.unlock()
	// The JobFn is passed directly to the spawned wrapper instead of being
	// stored in a channel. That keeps closure ownership with V's normal spawn
	// path while the token channel below still enforces the fixed worker limit.
	spawn run_pool_job(mut p, f)
}

// wait closes the pool to new submissions, drains accepted jobs, and waits for completion.
//
// wait is one-shot. It returns the first job error observed by any accepted job.
pub fn (mut p Pool) wait() ! {
	p.mutex.lock()
	if p.waited {
		p.mutex.unlock()
		return error(err_pool_wait_called)
	}
	p.waited = true
	p.closed = true
	p.mutex.unlock()

	p.wg.wait()
	p.cancel()
	err := p.get_first_error()
	if err !is none {
		return err
	}
}

// close is an explicit lifecycle alias for wait().
//
// It rejects later submissions and waits for all accepted jobs before returning.
pub fn (mut p Pool) close() ! {
	p.wait()!
}

fn run_pool_job(mut p Pool, f JobFn) {
	defer {
		p.finish_accepted_job()
		p.wg.done()
	}
	// The token channel is a bounded semaphore. At most `workers` accepted jobs
	// can pass this point and run user code concurrently.
	_ := <-p.tokens
	defer {
		p.tokens <- true
	}
	mut job_ctx := p.ctx
	f(mut job_ctx) or { p.set_first_error(err) }
}

fn (mut p Pool) finish_accepted_job() {
	p.mutex.lock()
	p.accepted--
	p.mutex.unlock()
}

fn (mut p Pool) set_first_error(err IError) {
	p.mutex.lock()
	if p.first_err is none {
		p.first_err = err
	}
	p.mutex.unlock()
}

fn (mut p Pool) get_first_error() IError {
	p.mutex.lock()
	err := p.first_err
	p.mutex.unlock()
	return err
}
