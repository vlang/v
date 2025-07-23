import v.eval

fn test_const() {
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
}
