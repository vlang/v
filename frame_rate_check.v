// run with `./frame_rate_test | pv -labe`, to find the real framerate at which the event loop is running
module main

import term_input as input
import time

fn event(e &input.Event, x voidptr) {
	if e.typ == .key_down && e.code == .escape { exit(0) }
	// time.usleep(2000)
}

fn frame(x voidptr) {
	println('FRAME')
}

mut ti := input.init(
	event_fn: event
	frame_fn: frame
	frame_rate: 60
)
ti.run()
