module util

// TODO: remove workaround once fixed in compiler
struct SharedIntWorkaround {
pub mut:
	value int
}

pub struct WorkerPool[T, Y] {
mut:
	workers   []thread
	queue_len shared SharedIntWorkaround
	ch_in     chan T
	ch_out    chan Y
}

pub fn WorkerPool.new[T, Y]() &WorkerPool[T, Y] {
	return &WorkerPool[T, Y]{
		ch_in:  chan T{cap: 1000}
		ch_out: chan Y{cap: 1000}
	}
}

pub fn (mut wp WorkerPool[T, Y]) add_worker(t thread) {
	wp.workers << t
}

pub fn (mut wp WorkerPool[T, Y]) active_jobs() int {
	return lock wp.queue_len {
		wp.queue_len.value
	}
}

pub fn (mut wp WorkerPool[T, Y]) get_job() !T {
	return <-wp.ch_in or { none }
}

pub fn (mut wp WorkerPool[T, Y]) push_result(result Y) {
	wp.ch_out <- result
	wp.job_done()
}

pub fn (mut wp WorkerPool[T, Y]) job_done() {
	lock wp.queue_len {
		wp.queue_len.value--
	}
}

pub fn (mut wp WorkerPool[T, Y]) queue_job(job T) {
	wp.ch_in <- job
	lock wp.queue_len {
		wp.queue_len.value++
	}
}

pub fn (mut wp WorkerPool[T, Y]) queue_jobs(jobs []T) {
	for job in jobs {
		wp.queue_job(job)
	}
}

pub fn (mut wp WorkerPool[T, Y]) wait_for_results() []Y {
	mut results := []Y{}
	for wp.active_jobs() > 0 {
		result := <-wp.ch_out
		results << result
	}

	wp.ch_in.close()
	wp.ch_out.close()
	wp.workers.wait()

	return results
}
