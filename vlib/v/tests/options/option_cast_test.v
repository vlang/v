struct Test {
	a int
}

type MySum = f64 | int

type MyAlias = Test

fn test_int() {
	a := ?int(1)
	if a != none {
		assert dump(a) == 1
		assert true
	} else {
		assert false
	}
}

fn test_struct() {
	b := ?Test{
		a: 1
	}
	if b != none {
		assert dump(b) == Test{
			a: 1
		}
		assert true
	} else {
		assert false
	}
}

fn test_string() {
	c := ?string('foo')
	if c != none {
		assert dump(c) == 'foo'
		assert true
	} else {
		assert false
	}
}

fn test_sum_type() {
	d := ?MySum(1.2)
	assert d != none

	if d != none {
		assert dump(d) == MySum(1.2)
		assert true
	} else {
		assert false
	}
}

fn test_alias() {
	d := ?MyAlias(Test{
		a: 1
	})
	assert d != none

	if d != none {
		assert dump(d) == Test{
			a: 1
		}
		assert true
	} else {
		assert false
	}
}
