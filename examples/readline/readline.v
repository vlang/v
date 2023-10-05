module main

import readline

fn main() {
	run() or { panic('${err}') }
}

fn run() ! {
	mut r := readline.Readline{}
	r.enable_raw_mode_nosig()
	defer {
		r.disable_raw_mode()
	}

	for {
		entered := r.read_char()!
		if entered == `q` {
			break
		}
		println('got ${entered}')
	}
	println('Goodbye.')
}
