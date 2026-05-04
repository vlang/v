module main

import mymod

struct RequestContext {}

fn request_id() mymod.MiddlewareFn {
	return fn (next mymod.HandlerFn) mymod.HandlerFn {
		return fn [next] (c mymod.Context) Error {
			return next(c)
		}
	}
}

fn test_function_types_imported_from_a_module_can_return_each_other() {
	middleware := request_id()
	mut app := mymod.new()
	app.use(middleware)
	handler := middleware(fn (_ mymod.Context) Error {
		return Error{}
	})
	err := handler(RequestContext{})
	assert err.code() == 0
}
