import time
import x.executor

fn main() {
	mut ex := executor.new(queue_size: 1)!
	ran := chan string{cap: 2}

	ex.try_post(fn [ran] () ! {
		ran <- 'first'
	})!

	ex.try_post(fn () ! {}) or { println('try_post backpressure: ${err.msg()}') }

	ex.post_with_timeout(20 * time.millisecond, fn () ! {}) or {
		println('timeout bounded admission: ${err.msg()}')
	}

	if !ex.run_one()! {
		eprintln('queued job did not run')
		exit(1)
	}
	println(<-ran)

	ex.stop()
	if ex.run_one()! {
		eprintln('unexpected job after stop')
		exit(1)
	}
	ex.wait()!
}
