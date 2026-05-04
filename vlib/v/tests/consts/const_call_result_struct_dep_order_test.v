enum Foo {
	foo
}

struct Something {}

const something = new(.foo) or { panic(err.str()) }

const bar = {
	Foo.foo: 'some_string'
}

fn new(foo Foo) !Something {
	println(bar[foo])
	return Something{}
}

fn test_const_call_result_struct_dep_order() {
	assert something == Something{}
	assert bar[.foo] == 'some_string'
}
