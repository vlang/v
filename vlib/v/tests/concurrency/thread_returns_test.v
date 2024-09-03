struct Foo {
mut:
	field1 thread !int
	field2 thread ?int
	field3 thread (int, int)
}

fn Foo.new() Foo {
	mut foo := Foo{
		field1: spawn get_error()
		field2: spawn get_none()
		field3: spawn get_multi_returns()
	}
	return foo
}

fn (mut foo Foo) bar() {
	if _ := foo.field1.wait() {
		assert false
	}
	if _ := foo.field2.wait() {
		assert false
	}
	a, b := foo.field3.wait()
	assert a == 1 && b == 2
}

fn get_error() !int {
	return error('error')
}

fn get_none() ?int {
	return none
}

fn get_multi_returns() (int, int) {
	return 1, 2
}

fn test_main() {
	mut foo := Foo.new()
	foo.bar()
}
