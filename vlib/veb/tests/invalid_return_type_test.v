import veb

pub struct WrongApp {}

struct WrongContext {
	veb.Context
}

pub struct FallibleApp {}

struct FallibleContext {
	veb.Context
}

// A handler may return `veb.Result` or `!veb.Result`; anything else with route
// attributes is still a route-definition error.
fn test_invalid_return_type_is_detected() {
	mut app := &WrongApp{}

	veb.generate_routes[WrongApp, WrongContext](app) or {
		assert err.msg().contains('invalid return type'), 'Expected error about invalid return type, got: ${err.msg()}'
		assert err.msg().contains('get_task'), 'Expected error to mention method name, got: ${err.msg()}'
		return
	}

	assert false, 'Expected generate_routes to return an error for invalid return type'
}

fn test_result_return_type_is_accepted() {
	// `!veb.Result` used to be rejected the same way; it now generates a route.
	mut app := &FallibleApp{}
	routes := veb.generate_routes[FallibleApp, FallibleContext](app) or { panic(err) }
	assert 'get_task' in routes
}

@['/test'; get]
pub fn (app &WrongApp) get_task(mut ctx WrongContext) string {
	return 'Hello'
}

@['/task'; get]
pub fn (app &FallibleApp) get_task(mut ctx FallibleContext) !veb.Result {
	return ctx.text('Hello')
}
