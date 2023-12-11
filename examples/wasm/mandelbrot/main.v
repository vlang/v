module main

import vweb
import os

const http_port = 3001

struct App {
	vweb.Context
}

fn main() {
	vweb.run(new_app(), http_port)
}

fn new_app() &App {
	mut app := &App{}

	os.execute_or_panic('v -b wasm -os browser mandelbrot.wasm.v')

	app.mount_static_folder_at(os.resource_abs_path('./'), '/')
	return app
}

@['/'; get]
pub fn (mut app App) controller_mandelbrot() !vweb.Result {
	file := os.read_file('mandelbrot.html') or { panic(err) }
	return app.html(file)
}
