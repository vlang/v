struct Foo {
	intv int
	cptr charptr
}

fn test_main() {
	println(Foo{ intv: 42 })
	assert true
}
