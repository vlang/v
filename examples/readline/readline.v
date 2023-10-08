module main

import readline

fn main() {
	$if windows {
		eprintln('Skipping the test on Windows, as raw mode and read_char are not yet implemented.')
		return
	}
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
