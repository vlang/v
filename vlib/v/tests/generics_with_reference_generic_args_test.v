module main

fn foo<T>(val T) ?T {
	return val
}

struct Bar {
	num int
}

fn test_generics_with_reference_generic_args() {
	ret := foo<&Bar>(&Bar{ num: 123 }) or { panic(err) }
	println(ret)
	assert ret.num == 123
}
