module main

import term
import time

fn main() {
	unbuffer_stdout()
	println('Press Ctrl-D or ESC to exit.')

	term.enable_echo(false)
	for {
		print('\r${time.now()} | ')
		// non-blocking mode, without echo
		x := term.key_pressed(false, false)
		if x in [0, 4, 27] {
			// pressing Ctrl-D exits the loop
			break
		}
		if x > 0 {
			println(x)
		}
		time.sleep(16 * time.millisecond)
	}
	term.enable_echo(true)

	println('done')
}
