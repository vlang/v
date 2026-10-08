module main

import x.progress
import time

fn main() {
	total := 200
	mut bar := progress.Bar.new(total,
		text:         'copying '
		style:        progress.BlockStyle{}
		show_rate:    true
		show_elapsed: true
		leave:        true // keep the finished bar on screen
	)
	bar.start()
	for _ in 0 .. total {
		time.sleep(10 * time.millisecond) // pretend to work
		bar.inc()
	}
	bar.wait()
}
