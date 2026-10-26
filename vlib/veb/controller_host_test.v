module veb

// Coverage for the free `controller_host` function. `register_host_controller`
// is already covered (vlib/veb/tests/register_host_controller_test.v), but it
// asserts only what the method appends to `Controller.controllers`. This locks
// the value `controller_host` returns: the path the inner controller was built
// with, and the host it was given, plus the fact that the handler is populated.
//
// The handler is never called, so no socket and no `not_found` resolution.
struct ChCtx {
	veb.Context
}

struct ChSubApp {}

pub fn (app &ChSubApp) index(mut ctx ChCtx) veb.Result {
	return ctx.text('sub')
}

struct ChMainApp {
	veb.Controller
}

pub fn (app &ChMainApp) index(mut ctx ChCtx) veb.Result {
	return ctx.text('main')
}

fn test_controller_host_sets_the_host_on_the_controller_it_returns() {
	mut sub := &ChSubApp{}

	ctrl := controller_host[ChSubApp, ChCtx]('example.com', '/', mut sub)!

	assert ctrl.path == '/'
	assert ctrl.host == 'example.com'
	assert !isnil(ctrl.handler)
}

fn test_controller_host_keeps_the_path_it_was_given() {
	mut sub := &ChSubApp{}

	ctrl := controller_host[ChSubApp, ChCtx]('api.example.com', '/api/v1/', mut sub)!

	assert ctrl.path == '/api/v1/'
	assert ctrl.host == 'api.example.com'
}

fn test_controller_host_returns_a_handle_that_can_be_registered() {
	mut sub := &ChSubApp{}
	mut app := &ChMainApp{}

	ctrl := controller_host[ChSubApp, ChCtx]('example.com', '/', mut sub)!
	app.controllers << ctrl

	assert app.controllers.len == 1
	assert app.controllers[0].host == 'example.com'
	assert app.controllers[0].path == '/'
}
