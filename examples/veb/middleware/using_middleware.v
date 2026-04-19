module main

import veb

// for another example see vlib/veb/tests/middleware_test.v
const http_port = 8080

pub struct Context {
	veb.Context
mut:
	is_authenticated bool
}

struct App {
	veb.Middleware[Context]
}

fn main() {
	mut app := &App{}

	// chaining is allowed, middleware will be evaluated in order
	app.Middleware.use(handler: other_func1)
	app.Middleware.use(handler: other_func2)

	// route-specific middleware
	app.Middleware.route_use('/admin/:path...', handler: check_auth)
	app.Middleware.route_use('/early', handler: middleware_early)

	veb.run[App, Context](mut app, http_port)
}

@['/']
pub fn (app &App) index(mut ctx Context) veb.Result {
	println('Index page')
	title := 'Home Page'

	content := $tmpl('templates/index.html')
	base := $tmpl('templates/base.html')
	return ctx.html(base)
}

@['/admin/secrets']
pub fn (app &App) secrets(mut ctx Context) veb.Result {
	println('Secrets page')
	title := 'Secret Admin Page'

	content := $tmpl('templates/secret.html')
	base := $tmpl('templates/base.html')
	return ctx.html(base)
}

@['/admin/:sub']
pub fn (app &App) dynamic(mut ctx Context, sub string) veb.Result {
	println('Dynamic page')
	title := 'Secret dynamic'

	content := sub
	base := $tmpl('templates/base.html')
	return ctx.html(base)
}

@['/early']
pub fn (app &App) early(mut ctx Context) veb.Result {
	println('Early page')
	title := 'Early Exit'

	content := $tmpl('templates/early.html')
	base := $tmpl('templates/base.html')
	return ctx.html(base)
}

fn other_func1(mut ctx Context) bool {
	println('1')
	return true
}

fn other_func2(mut ctx Context) bool {
	println('2')
	return true
}

fn check_auth(mut ctx Context) bool {
	println('3')
	if ctx.is_authenticated == false {
		ctx.redirect('/')
	}
	return ctx.is_authenticated
}

fn middleware_early(mut ctx Context) bool {
	println('4')
	ctx.text(':(')
	// returns false, so the middleware propagation is stopped and the user will see the text ":("
	return false
}
