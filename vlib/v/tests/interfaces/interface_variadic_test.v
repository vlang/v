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

// For issue 27326: passing a local variable declared inside a loop to a
// variadic interface parameter caused a `pointer expected` C error, because
// the value was promoted to auto-heap at the use site but not at its
// declaration, producing inconsistent pointer indirection in the generated C.
interface Value {}

fn collect(params ...Value) int {
	return params.len
}

fn test_variadic_interface_arg_declared_in_loop() {
	mut total := 0
	for i := 0; i < 3; i++ {
		id := [u8(1), 2, 3]
		code := 'hello ${i}'
		total += collect(id, code)
	}
	assert total == 6
}

type ValueSum = Cat | Dog

// For issue 27326: a variable smartcast via `if x is T` / `match` resolves to a
// synthetic smartcast scope var, not its real declaration. Promoting that
// synthetic var to auto-heap instead of the declaration must not desync the
// pointer indirection when the value is passed to a variadic interface param.
fn test_variadic_interface_arg_smartcast() {
	mut total := 0
	s := ValueSum(Cat{})
	if s is Cat {
		total += collect(s)
	}
	v := Value(Cat{})
	if v is Cat {
		total += collect(v)
	}
	match v {
		Cat { total += collect(v) }
		else {}
	}

	assert total == 3
}

// For issue 27326: a variable that is both declared in a nested scope (a `for`
// loop body) and smartcast via `if x is T` must still resolve to its real
// nested declaration, not the synthetic smartcast var nor only the function
// scope, otherwise the auto-heap promotion desyncs the pointer indirection.
fn test_variadic_interface_arg_smartcast_in_loop() {
	mut total := 0
	for _ in 0 .. 3 {
		s := ValueSum(Cat{})
		if s is Cat {
			total += collect(s)
		}
	}
	for _ in 0 .. 2 {
		v := Value(Cat{})
		if v is Cat {
			total += collect(v)
		}
	}
	assert total == 5
}

fn maybe_value() ?Cat {
	return Cat{}
}

// For issue 27326: an option value unwrapped via an if-guard or `if x != none`
// and then passed to a variadic interface parameter is auto-heap promoted, so
// its declaration is emitted as an option pointer. The unwrapped `.data` read
// at the call site must use the matching pointer indirection (`->data`);
// previously cgen emitted value-style access and failed C compilation.
fn test_variadic_interface_arg_option_unwrap() {
	mut total := 0
	if x := maybe_value() {
		total += collect(x)
	}
	x := maybe_value()
	if x != none {
		total += collect(x)
	}
	for _ in 0 .. 2 {
		y := maybe_value()
		if y != none {
			total += collect(y)
		}
	}
	assert total == 4
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
