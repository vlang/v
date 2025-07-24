import v.eval

fn test_if_return() {
	mut e := eval.create()

	ret := e.run('
	fn display(a int) int {
		if a == 100 {
			return 666
		} else {
			return 999
		}
	}
	display(110)')!

	dump(ret)
	assert ret[0].int_val() == 999

	ret1 := e.run('
	fn display(a int) int {
		if a == 100 {
			return 666
		} else {
			return 999
		}
	}
	display(100)')!

	dump(ret1)
	assert ret1[0].int_val() == 666
}
