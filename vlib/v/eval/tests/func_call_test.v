import v.eval

fn test_func_return() {
	mut e := eval.create()

	ret := e.run('
	fn sub(a int) int {
		if a > 105 {
			return 101
		}
		return 5151
	}
	
	fn key(b int) int {
		println(b)
		if b > 100 {
			return 3 + sub(sub(sub(b)))
		}
		return 7171
	}
	
	key(110)')!

	dump(ret)
	assert ret[0].int_val() == 104
}
