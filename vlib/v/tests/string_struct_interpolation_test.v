struct Foo {
	bar int
mut:
	str string
}

fn (f Foo) baz() string {
	return 'baz'
}

fn test_string_method_interpolation() {
	foo := Foo{}
	s := 'baz=$foo.baz()'
	assert s == 'baz=baz'
}

fn test_adding_to_mutable_string_field() {
	mut foo := Foo{10, 'hi'}
	assert foo.bar == 10
	assert foo.str == 'hi'
	foo.str += '!'
	eprintln(foo.str)
	assert foo.str == 'hi!'
}
