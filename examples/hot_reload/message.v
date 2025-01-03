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
	info := live.info()
	println('OK reloads: ${info.reloads_ok:4d} | Total reloads: ${info.reloads:4d} | Hello! Modify this message while the program is running.')
	eprintln('>> app: ${voidptr(app)} | g_live_reload_info: ${voidptr(g_live_reload_info)}')
	app.x = 9991 // try changing this to another value, while the program is running ...
	app.counter++
	dump(app)
}

fn main() {
	unbuffer_stdout()
	mut app := &App{}
	for {
		print_message(mut app)
		time.sleep(500 * time.millisecond)
	}
}
