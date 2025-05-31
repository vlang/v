module main

// Build this example with `v -live message.v`
import time
import v.live

struct App {
mut:
	x int
	c int
}

@[live]
fn print_message(mut app App) {
	i := live.info()
	println('Hello! Modify this message. OK reloads: ${i.reloads_ok:2d} | Total: ${i.reloads:2d} | app: ${voidptr(app)} | app.c: ${app.c:4} | app.x: ${app.x:12}')
	// app.x = app.x * 3 + 1 // try changing this to another value, while the program is running ...
	// app.x = 0
	app.c++
}

fn main() {
	unbuffer_stdout()
	println('=============================================================')
	println('== Modify the message below, while the program is running: ==')
	println('=============================================================')
	mut app := &App{}
	for {
		print_message(mut app)
		time.sleep(500 * time.millisecond)
	}
}
