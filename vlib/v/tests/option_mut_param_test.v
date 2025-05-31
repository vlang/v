struct Abc {
	a int
}

fn foo(mut baz ?Abc) {
	baz = Abc{
		a: 3
	}
	println(baz)
	dump(baz)
}

fn test_main() {
	mut a := ?Abc{
		a: 2
	}
	assert a?.a == 2
	dump(a)
	foo(mut a)
	println('--')
	dump(a)
	assert a?.a == 3
}
