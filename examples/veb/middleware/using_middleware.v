module main

import vweb

// for another example see vlib/vweb/tests/middleware_test_server.v
const http_port = 8080

struct App {
	vweb.Context
	middlewares map[string][]vweb.Middleware
mut:
	is_authenticated bool
}

fn main() {
	mut app := new_app()
	vweb.run(app, http_port)
}

fn new_app() &App {
	mut app := &App{
		middlewares: {
			// chaining is allowed, middleware will be evaluated in order
			'/admin/': [other_func1, other_func2]
			'/early':  [middleware_early]
		}
	}

	// do stuff with app
	// ...
	return app
}

@['/']
pub fn (mut app App) index() vweb.Result {
	println('Index page')
	title := 'Home Page'

	content := $tmpl('templates/index.html')
	base := $tmpl('templates/base.html')
	return app.html(base)
}

@[middleware: check_auth]
@['/admin/secrets']
pub fn (mut app App) secrets() vweb.Result {
	println('Secrets page')
	title := 'Secret Admin Page'

	content := $tmpl('templates/secret.html')
	base := $tmpl('templates/base.html')
	return app.html(base)
}

@['/admin/:sub']
pub fn (mut app App) dynamic(sub string) vweb.Result {
	println('Dynamic page')
	title := 'Secret dynamic'

	content := sub
	base := $tmpl('templates/base.html')
	return app.html(base)
}

@['/early']
pub fn (mut app App) early() vweb.Result {
	println('Early page')
	title := 'Early Exit'

	content := $tmpl('templates/early.html')
	base := $tmpl('templates/base.html')
	return app.html(base)
}

// is always executed first!
pub fn (mut app App) before_request() {
	app.is_authenticated = false
	println('0')
}

pub fn (mut app App) check_auth() bool {
	println('3')
	if app.is_authenticated == false {
		app.redirect('/')
	}
	return app.is_authenticated
}

fn other_func1(mut ctx vweb.Context) bool {
	println('1')
	return true
}

fn other_func2(mut ctx vweb.Context) bool {
	println('2')

	// ...
	return true
}

fn middleware_early(mut ctx vweb.Context) bool {
	println('4')
	ctx.text(':(')

	// returns false, so the middleware propagation is stopped and the user will see the text ":("
	return false
}
