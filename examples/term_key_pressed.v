module main

import term
import time

fn main() {
	unbuffer_stdout()
	term.enable_echo(false)
	defer {
		term.enable_echo(true)
	}
	for {
		// non-blocking mode, with echo
		x := term.key_pressed(false, true)
		if x == 0 {
			// pressing Ctrl-D exits the loop
			break
		}
		if x > 0 {
			println(x)
		}
		time.sleep(16 * time.millisecond)
		print('\r${time.now()} | ')
	}
	println('done')
}
