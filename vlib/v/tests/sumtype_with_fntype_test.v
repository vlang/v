type Expr = fn () int | fn (int) int

fn id(n int) int {
	return n
}

fn func() Expr {
	return id
}

fn test_sumtype_with_fntype() {
	f := func()

	f1 := f as fn (int) int
	println(123)
	assert f1(123) == 123

	if f is fn (int) int {
		ret := f(123)
		println(ret)
		assert ret == 123
	}

	match f {
		fn (int) int {
			ret := f(321)
			println(ret)
			assert ret == 321
		}
		else {}
	}
}
