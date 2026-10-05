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

fn call_methods_generic[A](app A, mut user_context Context) {
	$for method in A.methods {
		if method.name == 'by_ref' {
			mut p := &user_context
			app.$method(mut p)
		} else if method.name == 'by_value' {
			app.$method(mut user_context)
		}
	}
}

fn test_comptime_method_call_mut_ref_param_generic() {
	mut ctx := Context{}
	call_methods_generic(App{}, mut ctx)
	assert ctx.value == 11
}

fn call_by_value_with_local_and_ref(app App) int {
	mut local := Context{}
	mut p := &local
	$for method in App.methods {
		if method.name == 'by_value' {
			app.$method(mut local)
			app.$method(mut p)
		}
	}
	return local.value
}

fn test_comptime_method_call_mut_value_param_with_local_and_ref() {
	assert call_by_value_with_local_and_ref(App{}) == 2
}

fn forward_generic_methods[A](app A, mut ctx Context) {
	call_methods_generic[A](app, mut ctx)
}

fn forward_generic_methods_container[A](apps A, mut ctx Context) {
	forward_generic_methods(apps[0], mut ctx)
}

fn forward_generic_methods_deep[A](app A, mut ctx Context) {
	forward_generic_methods_container[[]A]([app], mut ctx)
}

fn forward_generic_methods_cycle[A](app A, mut ctx Context, depth int) {
	if depth > 0 {
		forward_generic_methods_cycle[A](app, mut ctx, depth - 1)
	} else {
		forward_generic_methods_deep[A](app, mut ctx)
	}
}

fn test_comptime_method_call_mut_ref_param_forwarded() {
	mut ctx := Context{}
	forward_generic_methods(App{}, mut ctx)
	assert ctx.value == 11
	forward_generic_methods_deep(App{}, mut ctx)
	assert ctx.value == 22
	forward_generic_methods_cycle(App{}, mut ctx, 3)
	assert ctx.value == 33
}

fn (app App) forward_methods[X](mut ctx Context) {
	call_methods_generic[App](app, mut ctx)
}

fn forward_generic_receiver_methods[A](app A, mut ctx Context) {
	app.forward_methods[int](mut ctx)
}

fn test_comptime_method_call_mut_ref_param_generic_method_forwarded() {
	mut ctx := Context{}
	forward_generic_receiver_methods(App{}, mut ctx)
	assert ctx.value == 11
}

struct OtherApp {}

fn (_ OtherApp) forward_methods[X](mut ctx Context) {
	ctx.value += 7
}

fn test_comptime_method_call_forwarding_uses_each_concrete_receiver() {
	mut ctx := Context{}
	forward_generic_receiver_methods(OtherApp{}, mut ctx)
	assert ctx.value == 7
	forward_generic_receiver_methods(App{}, mut ctx)
	assert ctx.value == 18
}
