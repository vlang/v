module livereload_fixture

import veb

pub struct Context {
	veb.Context
}

pub struct App {
	veb.StaticHandler
}

// index serves a page that can receive the live-reload script.
pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.html('<html><body><p id="greeting">hello</p></body></html>')
}

// plain serves text without HTML injection.
pub fn (app &App) plain(mut ctx Context) veb.Result {
	return ctx.text('plain text')
}

// form serves an HTML response to a POST request.
@['/form'; post]
pub fn (app &App) form(mut ctx Context) veb.Result {
	return ctx.html('<html><body>posted</body></html>')
}

// run serves the live-reload fixture on loopback using the current build flags.
pub fn run(port int, static_dir string) ! {
	mut app := &App{}
	app.handle_static(static_dir, true)!
	veb.run_at[App, Context](mut app, family: .ip, host: '127.0.0.1', port: port)!
}
