struct Foo {
mut:
	x string
	y ?string
}

fn test_main() {
	a := ?string(none)
	mut foo := Foo{}
	foo.x = a or { 'test' }
	foo.y = a or { 'test' }

	assert foo.x == 'test'
	assert foo.y? == 'test'
}
