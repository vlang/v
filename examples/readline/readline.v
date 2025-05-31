module main

import readline

fn main() {
	run() or { panic('${err}') }
}

fn run() ! {
	$if windows {
		eprintln('skipping on Windows, since raw mode and read_char are not yet implemented.')
		return
	} $else {
		// Explicit comptime block for other OSes than Windows is required to not break compilation on Windows.
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
}
