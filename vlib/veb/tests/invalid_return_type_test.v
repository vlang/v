import veb

pub struct App {}

struct Context {
	veb.Context
}

// Test that methods with route attributes but wrong return type are caught
fn test_invalid_return_type_is_detected() {
	mut app := &App{}

	// This should fail because get_task has route attributes but returns !veb.Result
	veb.generate_routes[App, Context](app) or {
		assert err.msg().contains('invalid return type'), 'Expected error about invalid return type, got: ${err.msg()}'
		assert err.msg().contains('get_task'), 'Expected error to mention method name, got: ${err.msg()}'
		return
	}

	assert false, 'Expected generate_routes to return an error for invalid return type'
}

@['/test'; get]
pub fn (app &App) get_task(mut ctx Context) !veb.Result {
	return ctx.text('Hello')
}
