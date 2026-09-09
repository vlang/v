import x.executor

fn main() {
	mut ex := executor.new(queue_size: 8)!
	events := chan int{cap: 6}

	for i in 0 .. 6 {
		ex.try_post(fn [events, i] () ! {
			events <- i
		})!
	}

	mut frame := 0
	mut drained_total := 0
	for drained_total < 6 {
		frame++
		drained := ex.drain_pending(2)!
		if drained == 0 {
			eprintln('frame ${frame} did not drain any work')
			exit(1)
		}
		drained_total += drained
		println('frame ${frame}: drained ${drained} owner jobs')
	}

	mut checksum := 0
	for _ in 0 .. 6 {
		checksum += <-events
	}
	println('checksum=${checksum}')

	ex.stop()
	if ex.drain_pending(1)! != 0 {
		eprintln('unexpected job after stop')
		exit(1)
	}
	ex.wait()!
}
