struct Foo {
mut:
	bar shared [1024]bool
}

fn test_main() {
	_ := Foo{}
	assert true
}
