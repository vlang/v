module main

import veb

struct Base {
	veb.Middleware[Context]
	veb.Controller
}

struct Context {
	veb.Context
pub mut:
	who string
}

struct App {
	Base
}

struct NoAuthCtrl {
	Base
}

struct ScopedCtrl {
	Base
}

@['/ping'; get]
fn (mut c NoAuthCtrl) ping(mut ctx Context) veb.Result {
	return ctx.text('pong')
}

@['/me'; get]
fn (mut c ScopedCtrl) me(mut ctx Context) veb.Result {
	return ctx.text('me')
}

pub enum Access {
	no_auth
	scoped
}

fn scoped_mw(mut ctx Context) bool {
	ctx.who = 'scoped'
	return true
}

fn (mut app App) register_routes[T](mut ctrl T, url_path string, access Access) {
	if access == .scoped {
		ctrl.use(veb.MiddlewareOptions[Context]{
			handler: scoped_mw
			after:   false
		})
	}
	app.register_controller[T, Context](url_path, mut ctrl) or { panic(err) }
	ctrl.route_use('${url_path}/*', veb.encode_auto[Context]())
}

fn test_generic_controller_registration_keeps_slice_element_type() {
	mut app := App{}
	app.register_routes(mut &NoAuthCtrl{}, '/noauth', .no_auth)
	app.register_routes(mut &ScopedCtrl{}, '/scoped', .scoped)
	assert app.controllers.len == 2
}
