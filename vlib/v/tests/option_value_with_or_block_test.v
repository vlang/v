struct Foo {
mut:
	x string
	y ?string
}

fn test_option_value_with_or_block() {
	a := ?string(none)
	mut foo := Foo{}
	foo.x = a or { 'test' }
	foo.y = a or { 'test' }
	println(foo)
	assert foo.x == 'test'
	assert foo.y? == 'test'
}
