module main

import x.progress
import time

fn main() {
	// A spinner is driven like a bar (start / finish / wait), but it has no
	// total, so it runs until you say the work is over.
	mut sp := progress.Spinner.new(
		text:         'resolving dependencies '
		frames:       progress.spinner_line()
		show_elapsed: true
		leave:        true // keep it on screen, with a done mark, when finished
	)
	sp.start()
	time.sleep(2 * time.second) // pretend to work
	sp.set_text('linking ')
	time.sleep(1 * time.second)
	sp.finish()
	sp.wait()
}
