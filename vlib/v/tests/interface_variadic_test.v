interface Element {
	method(params ...f64) string
}

struct Foo {}

fn (f &Foo) method(params ...f64) string {
	return params.str()
}

fn test_variadic_array_decompose() {
	mut a := []Element{}
	a << Foo{}

	input := [0.0, 1.0]
	assert a[0].method(...input) == '[0, 1]'
	assert a[0].method(...[0.0, 1.0]) == '[0, 1]'
}

fn test_variadic_multiple_args() {
	mut a := []Element{}
	a << Foo{}

	assert a[0].method(0.0, 1.0) == '[0, 1]'
}
