struct UnboundApp {
mut:
	base int
}

type UnboundAlias = UnboundApp

struct UnboundBox[T] {
	value T
}

fn (box &UnboundBox[T]) get() T {
	return box.value
}

fn UnboundApp.make(base int) UnboundApp {
	return UnboundApp{ base: base }
}

fn (app &UnboundApp) one(x int) int {
	return app.base + x
}

fn (app UnboundApp) value(x int) int {
	return app.base - x
}

fn (mut app UnboundApp) add(x int) int {
	app.base += x
	return app.base
}

fn invoke_unbound(f fn (&UnboundApp, int) int, app &UnboundApp, x int) int {
	return f(app, x)
}

fn reflected_unbound_read[T](app &T) int {
	mut result := 0
	$for method in T.methods {
		$if method.name == 'one' {
			f := T.$method
			result = f(app, 2)
		}
	}
	return result
}

fn reflected_unbound_write[T](mut app T) int {
	mut result := 0
	$for method in T.methods {
		$if method.name == 'add' {
			f := T.$method
			result = f(mut app, 3)
		}
	}
	return result
}

fn test_unbound_instance_method_values_keep_receiver_parameters() {
	mut app := UnboundApp{ base: 10 }
	f := UnboundApp.one
	assert f(&app, 1) == 11
	assert invoke_unbound(UnboundApp.one, &app, 2) == 12
	g := UnboundApp.value
	assert g(app, 3) == 7
	a := UnboundAlias.one
	assert a(&app, 4) == 14
	h := UnboundApp.add
	assert h(mut app, 5) == 15
	assert app.base == 15
	assert reflected_unbound_read[UnboundApp](&app) == 17
	assert reflected_unbound_write[UnboundApp](mut app) == 18
	assert app.base == 18
	factory := UnboundApp.make
	assert factory(20).base == 20
	bound := app.add
	assert bound(2) == 20
	assert app.base == 20
}

fn test_bound_method_value_on_generic_receiver() {
	box := UnboundBox[int]{ value: 42 }
	get := box.get
	assert get() == 42
}
