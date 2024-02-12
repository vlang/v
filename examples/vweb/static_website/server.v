module main

import x.vweb

pub struct Context {
	vweb.Context
}

pub struct App {
	vweb.StaticHandler
}

fn main() {
	folder := 'dist'
	port := 8080

	mut app := &App{}
	app.handle_static(folder, true)!
	vweb.run[App, Context](mut app, port)
}
