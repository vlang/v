import x.executor

fn main() {
	mut ex := executor.new(queue_size: 4)!
	result := chan string{cap: 1}
	run_result := chan string{cap: 1}

	runner := spawn fn [mut ex, run_result] () {
		ex.run() or {
			run_result <- err.msg()
			return
		}
		run_result <- 'ok'
	}()

	caller := spawn fn [mut ex, result] () {
		ex.run_sync(fn [result] () ! {
			result <- 'ran on owner loop'
		}) or { result <- 'run_sync failed: ${err.msg()}' }
	}()

	msg := <-result
	if msg != 'ran on owner loop' {
		eprintln(msg)
		exit(1)
	}
	println(msg)

	ex.stop()
	run_msg := <-run_result
	if run_msg != 'ok' {
		eprintln('run failed: ${run_msg}')
		exit(1)
	}
	ex.wait()!
	caller.wait()
	runner.wait()
}
