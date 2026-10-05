struct UnboundApp {
	base int
}

fn (app &UnboundApp) one(x int) int {
	return app.base + x
}

struct MutableUnboundApp {
mut:
	value int
}

fn (mut app MutableUnboundApp) increment(x int) int {
	app.value += x
	return app.value
}

fn test_unbound_method_has_receiver_parameter() {
	f := UnboundApp.one
	assert f(&UnboundApp{10}, 1) == 11
}

fn test_unbound_mutable_method_changes_receiver() {
	f := MutableUnboundApp.increment
	mut app := MutableUnboundApp{}
	assert f(mut app, 2) == 2
	assert app.value == 2
}

fn reflected_table[T](app &T) int {
	mut result := 0
	$for method in T.methods {
		if method.name == 'one' {
			f := T.$method
			result = f(app, 1)
		}
	}
	return result
}

fn test_reflected_unbound_method_value() {
	assert reflected_table(&UnboundApp{10}) == 11
}
