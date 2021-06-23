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
