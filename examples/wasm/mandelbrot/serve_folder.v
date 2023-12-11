module main

import vweb
import os

struct App {
	vweb.Context
}

fn main() {
	os.chdir(os.dir(@FILE))!
	cmd := '${os.quoted_path(@VEXE)} -b wasm -os browser mandelbrot.wasm.v'
	println('>> compiling mandelbrot.wasm.v, using: ${cmd}')
	os.execute_or_panic(cmd)
	mut app := App{}
	app.mount_static_folder_at(os.resource_abs_path('.'), '/')
	vweb.run(app, 3001)
}

pub fn (mut app App) index() vweb.Result {
	return app.html(os.read_file('mandelbrot.html') or { '' })
}
