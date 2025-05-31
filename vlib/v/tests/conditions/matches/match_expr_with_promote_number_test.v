struct Data {
	x f64
	y f64
}

fn (d Data) test(n int) f64 {
	return match n {
		0 { d.x }
		1 { d.y }
		else { 0 }
	}
}

fn test1(n int) f64 {
	return match n {
		0 { 1 }
		1 { 2 }
		else { 0 }
	}
}

fn test_match_expr_with_promote_number() {
	d := Data{
		x: 1
		y: 2
	}
	ret1 := d.test(2)
	println(ret1)
	assert ret1 == 0

	ret2 := test1(2)
	println(ret2)
	assert ret2 == 0
}
