fn test_main() {
	struct Foobar {
		foo int
	}

	fb := Foobar{5}
	assert fb.foo == 5
	x()
}

fn x() {
	struct Foobar {
		baz string
	}

	fb := Foobar{'hello world!'}
	println(fb.baz)
	assert true
}
