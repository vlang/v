import veb

struct Context {
	veb.Context
}

struct App {
	veb.Controller
}

struct SubController {}

fn (app &SubController) index(mut ctx Context) veb.Result {
	return ctx.text('sub')
}

fn make_default_value[T]() &T {
	return &T{}
}

fn test_generic_default_initializes_embedded_struct_in_its_c_field() {
	value := make_default_value[Context]()
	assert value.client_fd == -1
	assert int(value.return_type) == 0
}

fn test_generic_controller_closure_initializes_embedded_context_in_its_c_field() {
	mut app := &App{}
	mut controller := &SubController{}
	app.register_controller[SubController, Context]('/sub', mut controller)!
}
