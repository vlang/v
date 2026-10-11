module main

import x.progress
import time

// Each worker advances its own bar and the shared "total" bar. Bars are safe
// to use from many threads at once.
fn download(mut bar progress.Bar, mut total progress.Bar, name string, chunks int, delay time.Duration) {
	for i in 0 .. chunks {
		time.sleep(delay)
		bar.set_text('${name:-8}${i + 1:3}/${chunks:-3} ')
		bar.inc()
		total.inc()
		if i == chunks / 2 {
			// A plain println() would tear the bars; this prints above them.
			bar.println('${name}: halfway there')
		}
	}
	bar.set_text('${name:-8}')
}

fn main() {
	mut mb := progress.MultiBar.new()

	// Spinners and bars mix freely; they are drawn in the order added.
	mut conn := mb.add_spinner(
		text:   'connecting '
		on_end: 'connected'
	)
	mut status := mb.add_spinner(
		text:         'syncing '
		frames:       progress.spinner_braille()
		show_elapsed: true
		leave:        true
	)
	mut total := mb.add_bar(300,
		text:         'total   '
		style:        progress.BlockStyle{}
		show_elapsed: true
		leave:        true
	)
	mut a := mb.add_bar(100,
		text:   'alpha   '
		on_end: 'alpha   done'
	)
	mut b := mb.add_bar(120,
		text:      'bravo   '
		on_end:    'bravo   done'
		show_rate: true
	)
	mut c := mb.add_bar(80,
		text:   'charlie '
		on_end: 'charlie done'
	)

	mb.start()

	time.sleep(800 * time.millisecond)
	conn.finish() // a spinner ends when you say so

	mut ts := []thread{}
	ts << spawn download(mut a, mut total, 'alpha', 100, 20 * time.millisecond)
	ts << spawn download(mut b, mut total, 'bravo', 120, 12 * time.millisecond)
	ts << spawn download(mut c, mut total, 'charlie', 80, 30 * time.millisecond)
	ts.wait()
	status.finish()

	mb.wait()
}
