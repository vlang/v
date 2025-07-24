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

fn test_if_infix_return_early() {
	mut e := eval.create()

	ret := e.run('
	fn display(a int, b int) int {
		if a == 100 && b == 100 {
			return 111
		} else if a == 100 && b != 100 {
			return 222
		} else if a == 100 && b > 100 {
			return 333
		} else if a == 100 && b < 100 {
			return 444
		} else if a == 100 || b == 100 {
			return 555
		} else if a == 100 || b != 100 {
			return 666
		} else if a == 100 || b > 100 {
			return 777
		} else if a == 100 || b < 100 {
			return 888
		}
		return 7171
	}
	display(200, 100)')!

	dump(ret)
	assert ret[0].int_val() == 555
}
