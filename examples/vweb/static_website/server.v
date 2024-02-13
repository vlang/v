module main

import x.vweb

pub struct Context {
	vweb.Context
}

pub struct App {
	vweb.StaticHandler
}

fn main() {
	os.chdir(os.dir(os.executable()))!
	port := 8080

	mut app := &App{}
	app.handle_static('dist', true)!
	vweb.run[App, Context](mut app, port)
}
