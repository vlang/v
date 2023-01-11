struct Test {
	a int
	b ?int
}

fn test_main() {
	hh := 1 // i64(4)
	$if hh is $Int {
		println('int')
		assert true
	} $else $if hh !is $Int {
		println('string')
		assert false
	}

	b := 1.2
	$if b is $Int {
		assert false
	} $else $if b is $Float {
		println('float')
		assert true
	}

	c := true
	$if c is bool {
		println('bool')
		assert true
	}

	d := Test{}
	$if d.b is ?int {
		println('?int')
		assert true
	}
	$if d.a is ?int {
		println('?int')
		assert false
	} $else $if d.a is int {
		println('int')
		assert true
	}

	$if d is Test {
		println('Test')
	}
}
