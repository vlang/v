struct Context {}

struct App {
mut:
	called bool
}

fn (mut app App) before_request(mut ctx Context) {
	app.called = true
}

interface HasBeforeRequest[X] {
mut:
	before_request(mut ctx X)
}

fn check_before_request[A, X](mut app A) bool {
	mut ctx := Context{}
	$if A is HasBeforeRequest[X] {
		app.before_request(mut ctx)
		return true
	} $else {
		return false
	}
}

fn test_comptime_if_is_generic_interface_with_generic_type_param() {
	mut app := App{}
	assert check_before_request[App, Context](mut app)
	assert app.called
}
