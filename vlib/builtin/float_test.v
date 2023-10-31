fn test_float_decl() {
	// z := 1f
	// assert z > 0
	x1 := 1e10
	x2 := -2e16
	x3 := 1e-15
	x4 := -9e-4
	assert typeof(x1).name == 'f64'
	assert typeof(x2).name == 'f64'
	assert typeof(x3).name == 'f64'
	assert typeof(x4).name == 'f64'
	x5 := 4e108
	x6 := -7e99
	x7 := 3e-205
	x8 := -6e-147
	assert typeof(x5).name == 'f64'
	assert typeof(x6).name == 'f64'
	assert typeof(x7).name == 'f64'
	assert typeof(x8).name == 'f64'
	x9 := 312874834.77
	x10 := -22399994.06
	x11 := 0.0000000019
	x12 := -0.00000000008
	assert typeof(x9).name == 'f64'
	assert typeof(x10).name == 'f64'
	assert typeof(x11).name == 'f64'
	assert typeof(x12).name == 'f64'
	x13 := 34234234809890890898903213154353453453253253243432413232228908902183918392183902432432438980380123021983901392183921389083913890389089031.0
	x14 := -39999999999999999999222212128182813294989082302832183928343325325233253242312331324392839238239829389038097438248932789371837218372837293.8
	x15 := 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002
	x16 := -0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004
	assert typeof(x13).name == 'f64'
	assert typeof(x14).name == 'f64'
	assert typeof(x15).name == 'f64'
	assert typeof(x16).name == 'f64'
}

fn test_f32_equal_operator() {
	b := f32(1.0)
	mut a := f32(1.0)
	a += 0.0000019073486328125
	assert a != b
	a -= 0.0000019073486328125
	assert a == b
	assert -1 == 1 * -1
	assert -1.0 == 1.0 * -1.0
	a = 1
	a += 0.0000019073486328125
	a -= 0.0000019073486328125
	assert a == f32(1.0)
	a += 0.000001
	assert !(a < f32(1))
	assert !(a <= f32(1))
	assert a > f32(1)
	assert a >= 1
	assert a != 1
	f := 1.2
	ab := int(f)
	assert ab == 1
	e := f32(-1.602176634e-19)
	m := f32(9.1093837015e-31)
	assert e < m
	assert e <= m
	assert e != m
	assert !(e == m)
	assert m >= e
	assert m > e
}

fn test_f64_equal_operator() {
	b := 1.0
	mut a := 1.0
	a += 0.0000019073486328125
	assert a != b
	a -= 0.0000019073486328125
	assert a == b
	e := -1.602176634e-19
	m := 9.1093837015e-31
	assert e < m
	assert e <= m
	assert e != m
	assert !(e == m)
	assert m >= e
	assert m > e
}

fn test_f64_eq_epsilon() {
	a := 1.662248544459347e308
	b := 1.662248544459348e308
	x := 1.662248544459352e308
	assert a != b
	assert a.eq_epsilon(b)
	assert b.eq_epsilon(a)
	assert (-a).eq_epsilon(-b)
	assert (-b).eq_epsilon(-a)
	assert !a.eq_epsilon(x)
	assert !x.eq_epsilon(a)
	assert !a.eq_epsilon(-b)
	assert !(-a).eq_epsilon(b)
	c := 1.5367748374385438503
	d := -1.5367748374385447257
	z := 1.5367748378943546
	assert c != -d
	assert c.eq_epsilon(-d)
	assert d.eq_epsilon(-c)
	assert !c.eq_epsilon(z)
	assert !z.eq_epsilon(c)
	e := 2.531434251587394233e-308
	f := 2.531434251587395675e-308
	y := 2.531434251587398934e-308
	assert e != f
	assert e.eq_epsilon(f)
	assert (-f).eq_epsilon(-e)
	assert !e.eq_epsilon(y)
	assert !(-y).eq_epsilon(-e)
}

fn test_f32_eq_epsilon() {
	a := f32(3.244331e38)
	b := f32(3.244332e38)
	x := f32(3.244338e38)
	assert a != b
	assert a.eq_epsilon(b)
	assert b.eq_epsilon(a)
	assert (-a).eq_epsilon(-b)
	assert (-b).eq_epsilon(-a)
	assert !a.eq_epsilon(x)
	assert !(-x).eq_epsilon(-a)
	assert !a.eq_epsilon(-b)
	assert !(-a).eq_epsilon(b)
	c := f32(0.9546742)
	d := f32(-0.9546745)
	z := f32(0.9546754)
	assert c != -d
	assert c.eq_epsilon(-d)
	assert d.eq_epsilon(-c)
	assert !c.eq_epsilon(z)
	assert !z.eq_epsilon(c)
	e := f32(-1.5004390e-38)
	f := f32(-1.5004395e-38)
	y := f32(-1.5004409e-38)
	assert e != f
	assert e.eq_epsilon(f)
	assert (-f).eq_epsilon(-e)
	assert !e.eq_epsilon(y)
	assert !(-y).eq_epsilon(-e)
}

fn test_float_point_formatting_rounding() {
	float_1 := 462.18
	float_2 := 45.02227
	float_3 := 238.5
	float_4 := 239.5

	assert '${float_1:0.0f}' == '462'
	assert '${float_2:0.0f}' == '45'
	assert '${float_3:0.0f}' == '239'
	assert '${float_4:0.0f}' == '240'
	//
	//	
	assert '${239.5555551:0.0f}' == '240'
	assert '${239.5555551:0.1f}' == '239.6'
	assert '${239.5555551:0.2f}' == '239.56'
	assert '${239.5555551:0.3f}' == '239.556'
	assert '${239.5555551:0.4f}' == '239.5556'
	assert '${239.5555551:0.5f}' == '239.55556'
	assert '${239.5555551:0.6f}' == '239.555555'
	assert '${239.5555551:0.7f}' == '239.5555551'
	assert '${239.5555551:0.8f}' == '239.55555510'
	assert '${239.5555551:0.9f}' == '239.555555100'
	assert '${239.5555551:0.10f}' == '239.5555551000'
	assert '${239.5555551:0.11f}' == '239.55555510000'
	//
	assert '${239.5:0.0f}' == '240'
	assert '${239.55:0.1f}' == '239.6'
	assert '${239.555:0.2f}' == '239.56'
	assert '${239.5555:0.3f}' == '239.555' // Note: 5 ?
	assert '${239.55555:0.4f}' == '239.5556'
	assert '${239.555555:0.5f}' == '239.55555' // Note: 5 ?
	assert '${239.5555555:0.6f}' == '239.555556' // after this, it is all ending in 6
	assert '${239.55555555:0.7f}' == '239.5555556'
	assert '${239.555555555:0.8f}' == '239.55555556'
	assert '${239.5555555555:0.9f}' == '239.555555556'
	assert '${239.55555555555:0.10f}' == '239.5555555556'
	//
	assert '${239.5550:0.3f}' == '239.555'
	assert '${239.5551:0.3f}' == '239.555'
	assert '${239.5552:0.3f}' == '239.555'
	assert '${239.5553:0.3f}' == '239.555'
	assert '${239.5554:0.3f}' == '239.555'
	assert '${239.5555:0.3f}' == '239.555'
	assert '${239.5556:0.3f}' == '239.556' // rounding at last 6 ?
	assert '${239.5557:0.3f}' == '239.556'
	assert '${239.5558:0.3f}' == '239.556'
	assert '${239.5559:0.3f}' == '239.556'
	//
	assert '${239.5555551:0.6f}' == '239.555555'
	assert '${239.5555552:0.6f}' == '239.555555'
	assert '${239.5555553:0.6f}' == '239.555555'
	assert '${239.5555554:0.6f}' == '239.555555'
	assert '${239.5555555:0.6f}' == '239.555556'
	assert '${239.5555556:0.6f}' == '239.555556'
	assert '${239.5555557:0.6f}' == '239.555556'
	assert '${239.5555558:0.6f}' == '239.555556'
	assert '${239.5555559:0.6f}' == '239.555556'
	//
	assert '${239.55555555555:0.10f}' == '239.5555555556'
	assert '${239.55555555555:0.9f}' == '239.555555556'
	assert '${239.55555555555:0.8f}' == '239.55555556'
	assert '${239.55555555555:0.7f}' == '239.5555556'
	assert '${239.55555555555:0.6f}' == '239.555556'
	assert '${239.55555555555:0.5f}' == '239.55556'
	assert '${239.55555555555:0.4f}' == '239.5556'
	assert '${239.55555555555:0.3f}' == '239.556'
	assert '${239.55555555555:0.2f}' == '239.56'
	assert '${239.55555555555:0.1f}' == '239.6'
	assert '${239.55555555555:0.0f}' == '240'
	//
	assert '${-239.55555555555:0.10f}' == '-239.5555555556'
	assert '${-239.55555555555:0.9f}' == '-239.555555556'
	assert '${-239.55555555555:0.8f}' == '-239.55555556'
	assert '${-239.55555555555:0.7f}' == '-239.5555556'
	assert '${-239.55555555555:0.6f}' == '-239.555556'
	assert '${-239.55555555555:0.5f}' == '-239.55556'
	assert '${-239.55555555555:0.4f}' == '-239.5556'
	assert '${-239.55555555555:0.3f}' == '-239.556'
	assert '${-239.55555555555:0.2f}' == '-239.56'
	assert '${-239.55555555555:0.1f}' == '-239.6'
	assert '${-239.55555555555:0.0f}' == '-240'
}

fn test_float_zero_str() {
	f1 := f32(0.0)
	f2 := 0.0
	assert f1.str() == '0.0'
	assert '${f1}' == '0.0'
	assert f2.str() == '0.0'
	assert '${f2}' == '0.0'
}
