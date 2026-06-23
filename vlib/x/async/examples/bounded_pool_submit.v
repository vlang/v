import context
import time
import x.async as xasync

fn main() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	started := chan bool{cap: 2}
	release := chan bool{cap: 2}
	accepted := chan bool{cap: 1}
	ran := chan bool{cap: 1}
	context_ran := chan bool{cap: 1}
	blocking_job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
	}

	pool.try_submit(blocking_job)!
	if !wait_bool(started) {
		eprintln('first pool job did not start')
		exit(1)
	}
	pool.try_submit(blocking_job)!

	submitter := spawn fn [mut pool, accepted, ran] () {
		pool.submit_with_timeout(1 * time.second, fn [ran] (mut ctx context.Context) ! {
			_ = ctx
			ran <- true
		}) or {
			eprintln('bounded submit failed: ${err.msg()}')
			accepted <- false
			return
		}
		accepted <- true
	}()

	release <- true
	if !wait_bool(accepted) {
		eprintln('bounded submit was not accepted after capacity opened')
		exit(1)
	}

	release <- true
	if !wait_bool(ran) {
		eprintln('bounded submit job did not run')
		exit(1)
	}

	parent_ctx, cancel := xasync.with_cancel()
	defer {
		cancel()
	}
	pool.submit_with_context(parent_ctx, fn [context_ran] (mut ctx context.Context) ! {
		_ = ctx
		context_ran <- true
	})!
	if !wait_bool(context_ran) {
		eprintln('context-bounded submit job did not run')
		exit(1)
	}

	pool.close()!
	submitter.wait()
	println('timeout and context bounded submits completed')
}

fn wait_bool(ch chan bool) bool {
	select {
		ok := <-ch {
			return ok
		}
		1 * time.second {
			return false
		}
	}
	return false
}
