import x.executor

fn main() {
	mut ex := executor.new(queue_size: 4)!
	updates := chan string{cap: 1}

	ex.try_post(fn [updates] () ! {
		updates <- 'owner state updated'
	})!

	ran := ex.run_one()!
	if !ran {
		eprintln('executor did not run the queued job')
		exit(1)
	}

	println(<-updates)
	ex.stop()
	if ex.run_one()! {
		eprintln('unexpected job after stop')
		exit(1)
	}
	ex.wait()!
}
