import x.executor

fn main() {
	mut ex := executor.new(queue_size: 2)!
	ran := chan bool{cap: 1}
	run_result := chan string{cap: 1}

	ex.try_post(fn [mut ex, ran] () ! {
		ex.stop()
		ran <- true
	})!

	runner := spawn fn [mut ex, run_result] () {
		ex.run() or {
			run_result <- err.msg()
			return
		}
		run_result <- 'ok'
	}()

	did_run := <-ran
	if !did_run {
		eprintln('owner stop job did not run')
		exit(1)
	}
	msg := <-run_result
	if msg != 'ok' {
		eprintln('run returned error: ${msg}')
		exit(1)
	}

	ex.try_post(fn () ! {}) or { println('submit after stop: ${err.msg()}') }
	ex.wait()!
	runner.wait()
}
