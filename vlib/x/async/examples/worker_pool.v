import context
import time
import x.async as xasync

fn main() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	started := chan int{cap: 2}
	release := chan bool{cap: 2}
	job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- 1
		_ := <-release
	}

	pool.try_submit(job)!
	if !wait_started(started) {
		eprintln('pool job did not start')
		exit(1)
	}

	pool.try_submit(job)!
	pool.try_submit(job) or { println('backpressure: ${err.msg()}') }

	release <- true
	if !wait_started(started) {
		eprintln('pool job did not start')
		exit(1)
	}
	release <- true

	pool.close()!
	println('pool drained')
}

fn wait_started(started chan int) bool {
	select {
		_ := <-started {
			return true
		}
		1 * time.second {
			return false
		}
	}
	return false
}
