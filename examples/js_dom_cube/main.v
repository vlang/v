module main

import os
import veb

const http_port = 3001
const vexe = os.quoted_path(os.getenv_opt('VEXE') or { @VEXE })

pub struct Context {
	veb.Context
}

pub struct App {
	veb.StaticHandler
}

fn main() {
	os.chdir(os.dir(@FILE))!
	mut app := &App{}
	veb.run_at[App, Context](mut app, port: http_port)!
}

// before_accept_loop builds cube.js before the server starts and registers the static assets.
pub fn (mut app App) before_accept_loop() {
	os.execute_or_panic('${vexe} -b js_browser cube.js.v')
	app.serve_static('/cube.js', 'cube.js') or { panic(err) }
	app.serve_static('/index.html', 'index.html') or { panic(err) }
}
