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
	cmd := '${os.quoted_path(@VEXE)} -b wasm -os browser change_color_by_id.wasm.v'
	println('>> compiling change_color_by_id.wasm.v, using: ${cmd}')
	os.exec_or_panic([@VEXE, '-b', 'wasm', '-os', 'browser', 'change_color_by_id.wasm.v'])
	mut app := &App{}
	app.handle_static('.', true)!
	veb.run[App, Context](mut app, 3001)
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.html(os.read_file('change_color_by_id.html') or { '' })
}
