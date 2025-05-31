struct Test {
	a ?string
	b ?int
	c ?u8
	d ?fn (int)
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

fn test_comptime() {
	t := Test{}
	$for f in Test.fields {
		println('${f.name}')
		if t.$(f.name) == none {
			assert true
		} else {
			assert false
		}
	}
}
