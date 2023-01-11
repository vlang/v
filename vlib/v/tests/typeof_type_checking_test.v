struct Test {
	a int
	b ?int
}

fn test_main() {
	hh := '' // i64(4)
	$if typeof(hh) is $Int {
		println('int')
		assert false
	} $else {
		println('string')
		assert true
	}

	b := 1
	$if typeof(b) is $Int {
		println('int')
		assert true
	}

	c := true
	$if typeof(c) is bool {
		println('bool')
		assert true
	}

	d := Test{}
	$if typeof(d.b) is ?int {
		println('?int')
		assert true
	}
}
