struct Foo {
	name string
	age  int
}

type Boo = &Foo

fn foo(f Boo) {
	println(f)
	dump(f)
}

fn test_main() {
	foo(name: '')
	assert true
}
