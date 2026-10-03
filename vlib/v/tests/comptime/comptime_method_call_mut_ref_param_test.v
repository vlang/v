struct Context {
mut:
	value int
}

struct App {}

fn (_ App) by_ref(mut ctx &Context) {
	ctx.value += 10
}

fn (_ App) by_value(mut ctx Context) {
	ctx.value++
}

fn call_methods(app App, mut user_context Context) {
	$for method in App.methods {
		if method.name == 'by_ref' {
			mut p := &user_context
			app.$method(mut p)
		} else if method.name == 'by_value' {
			app.$method(mut user_context)
		}
	}
}

fn test_comptime_method_call_mut_ref_param() {
	app := App{}
	mut ctx := Context{}
	call_methods(app, mut ctx)
	assert ctx.value == 11
}
