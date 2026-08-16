// Regression test for https://github.com/vlang/v/issues/28019
// The veb README documented host-scoped controllers via
// `register_controller('host', '/', mut app)`, but that method only takes
// `(path, mut app)`. The correct API is `register_host_controller`. This test
// locks that method's signature/behavior in.
import veb

pub struct Context {
	veb.Context
}

pub struct HostApp {}

pub fn (app &HostApp) index(mut ctx Context) veb.Result {
	return ctx.text('host app')
}

pub struct RootApp {
	veb.Controller
}

pub fn (app &RootApp) index(mut ctx Context) veb.Result {
	return ctx.text('root app')
}

fn test_register_host_controller_registers_with_host() {
	mut host_app := &HostApp{}
	mut app := &RootApp{}
	app.register_host_controller[HostApp, Context]('example.com', '/', mut host_app)!
	assert app.controllers.len == 1
	assert app.controllers[0].host == 'example.com'
	assert app.controllers[0].path == '/'
}
