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
		mut k := false
		if !k {
			k = !k
		}
		println(k)
		if a == 100 && b == 100 {
			return 100
		} else if a == 100 && b != 100 {
			return 101
		} else if a == 100 && b > 100 {
			return 102
		} else if a == 100 && b < 100 {
			return 103
		} else if a == 100 && b >= 100 {
			return 104
		} else if a == 100 && b <= 100 {
			return 105
		} else if a == 100 || b == 100 {
			return 106
		} else if a == 100 || b != 100 {
			return 107
		} else if a == 100 || b > 100 {
			return 108
		} else if a == 100 || b < 100 {
			return 109
		} else if a == 100 || b >= 100 {
			return 110
		} else if a == 100 || b <= 100 {
			return 111
		}
		return 7171
	}
	display(200, 101)')!

	dump(ret)
	assert ret[0].int_val() == 107
}
