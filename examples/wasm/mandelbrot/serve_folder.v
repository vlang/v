module main

import veb
import os

pub struct Context {
	veb.Context
}

struct App {
	veb.StaticHandler
}

fn main() {
	os.chdir(os.dir(@FILE))!
	cmd := '${os.quoted_path(@VEXE)} -b wasm -os browser mandelbrot.wasm.v'
	println('>> compiling mandelbrot.wasm.v, using: ${cmd}')
	os.execute_or_panic(cmd)
	mut app := &App{}
	app.handle_static('.', true)!
	veb.run[App, Context](mut app, 3001)
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.html(os.read_file('mandelbrot.html') or { '' })
}
