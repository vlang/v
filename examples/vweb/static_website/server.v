module main

import x.vweb
import os

pub struct Context {
	vweb.Context
}

pub struct App {
	vweb.StaticHandler
}

fn main() {
	// make sure that the working folder is the one, containing the executable,
	// so that 'dist' is a valid relative path from it later:
	os.chdir(os.dir(os.executable()))!
	mut app := &App{}
	app.handle_static('dist', true)!
	vweb.run[App, Context](mut app, 8080)
}
