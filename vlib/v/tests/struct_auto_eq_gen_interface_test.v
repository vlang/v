interface IExample {
	thing() bool
}

type Foo = int

fn (n Foo) thing() bool {
	return true
}

struct Test {
	a IExample
}

fn new() Test {
	return Test{Foo(123)}
}

fn test_struct_auto_eq_gen_interface_case() {
	w1 := new()
	w2 := new()
	assert w1 == w2
}
