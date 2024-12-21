module main

import term
import time

fn main() {
	unbuffer_stdout()
	println('Press Ctrl-D or ESC to exit.')

	term.enable_echo(false)
	mut frame := 0
	for {
		print('\r${time.now()} | frame: ${frame:06} | ')
		x := term.key_pressed()
		if x in [i64(0), 4, 27] {
			// pressing Ctrl-D or ESC exits the loop
			// Ctrl-D mean EOF sometimes, so return 0
			break
		}
		if x > 0 {
			println('${x:08x}')
		}
		time.sleep(16 * time.millisecond)
		frame++
	}
	term.enable_echo(true)

	println('done')
}
