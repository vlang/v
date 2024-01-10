@[heap]
struct Foo {
	a string
	b int
}

fn ret() !(int, Foo) {
	return 0, Foo{}
}

fn test_multiret_with_result() {
	_, foo := ret()!
	println(foo)
	assert true
}
