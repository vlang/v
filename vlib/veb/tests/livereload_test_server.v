module main

// A small app for livereload_test.v: `livereload_test_server <port> <static dir>`.
import os
import veb

pub struct Context {
	veb.Context
}

pub struct App {
	veb.StaticHandler
}

pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.html('<html><body><p id="greeting">hello</p></body></html>')
}

pub fn (app &App) plain(mut ctx Context) veb.Result {
	return ctx.text('plain text')
}

@['/form'; post]
pub fn (app &App) form(mut ctx Context) veb.Result {
	return ctx.html('<html><body>posted</body></html>')
}

fn main() {
	port := os.args[1].int()
	mut app := &App{}
	app.handle_static(os.args[2], true)!
	veb.run_at[App, Context](mut app, family: .ip, host: '127.0.0.1', port: port) or { panic(err) }
}
