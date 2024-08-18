module main

import veb
import os

pub struct Context {
	veb.Context
}

pub struct App {
	veb.StaticHandler
}

fn main() {
	// make sure that the working folder is the one, containing the executable,
	// so that 'dist' is a valid relative path from it later:
	os.chdir(os.dir(os.executable()))!
	mut app := &App{}
	app.handle_static('dist', true)!
	veb.run[App, Context](mut app, 8080)
}
