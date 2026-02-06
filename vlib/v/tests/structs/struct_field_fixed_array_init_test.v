struct Foo {
mut:
	bar [2]string
}

fn test_struct_field_fixed_array_init() {
	foo := Foo{}
	println(foo.bar[0])
	assert foo.bar[0] == ''
}
