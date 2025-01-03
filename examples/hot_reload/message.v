module main

// Build this example with `v -live message.v`
import time
import v.live

struct App {
mut:
	x       int
	counter int
}

@[live]
fn print_message(mut app App) {
	i := live.info()
	println('OK reloads: ${i.reloads_ok:4d} | Total reloads: ${i.reloads:4d} | Hello! Modify this message while the program is running. app: ${voidptr(app)} | app.x: ${app.x:6} | app.counter: ${app.counter:6}')
	// app.x = 99 // try changing this to another value, while the program is running ...
	app.counter++
}

fn main() {
	unbuffer_stdout()
	mut app := &App{}
	for {
		print_message(mut app)
		time.sleep(500 * time.millisecond)
	}
}
