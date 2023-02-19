struct Test {
	a ?int
}

fn test_main() {
	a := ?int(none)

	if a == none {
		assert true
	} else {
		assert false
	}

	b := Test{}
	if b.a == none {
		assert true
	} else {
		assert false
	}

	c := ?Test(Test{})
	if c?.a == none {
		assert true
	} else {
		assert false
	}
}
