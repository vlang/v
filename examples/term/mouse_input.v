import time
import term
import term.input as ti

ti.setup({})
println('\x1b[1;1H\x1b[2J')
term.erase_del_clear()

for i in 0 .. 5000 {
	event, data := ti.read()
	match event {
		.mouse_down {
			// Uncomment this to clear the terminal on every click
			// println('\x1b[1;1H\x1b[2J')
			str := 'Hello there!'
			term.set_cursor_position(x: data.x - str.len / 2, y: data.y)
			println(term.bg_white(term.blue('Hello there!')))
		}
		else {}
	}
	term.set_cursor_position(x: 5, y: 3)
	println('Frame #$i')
	time.sleep_ms(16)
}
