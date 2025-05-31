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
	assert a[0].method(...input) == '[0.0, 1.0]'
	assert a[0].method(...[0.0, 1.0]) == '[0.0, 1.0]'
}

fn test_variadic_multiple_args() {
	mut a := []Element{}
	a << Foo{}

	assert a[0].method(0.0, 1.0) == '[0.0, 1.0]'
}

interface Animal {}

struct Cat {}

struct Dog {}

fn test_variadic_interface_fn_arg() {
	c := Cat{}
	d := Dog{}
	check_animals(c, d)
}

fn check_animals(animals ...Animal) {
	assert animals[0] is Cat
	assert animals[1] is Dog
}

// For issue: 16286 passing nothing to a variatic parameter on an interface method gives builder error
interface Bar {
	get(...int) []int
}

struct Baz {}

fn (b Baz) get(n ...int) []int {
	if n.len == 0 {
		return [-1]
	} else {
		return n
	}
}

fn test_empty_args_call_interface_methods() {
	b := Baz{}
	assert b.get() == [-1]

	b_values := Bar(Baz{})
	assert b_values.get(1, 2, 3) == [1, 2, 3]

	b_empty := Bar(Baz{})
	assert b_empty.get() == [-1]
}
