fn test_anyfloat() {
	a := f64(5.765) + 45.75
	b := 12.3 + f64(3)
	c := f32(6.75) / 3.0
	d := 16.5 / f32(2)
	assert a == 51.515
	assert typeof(a).name == 'f64'
	assert b == f64(15.3)
	assert typeof(b).name == 'f64'
	assert c == 2.25
	assert typeof(c).name == 'f32'
	assert d == 8.25
	assert typeof(d).name == 'f32'
}

fn g(x f32) f32 {
	return x*x
}

fn fabs(x f32) f32 {
	return if x >= 0.0 { x } else { -x }
}

fn test_call() {
	c := 503
	r := g(f32(c) / 255.0)
	assert fabs(r - 3.890949634755863) <= 1.e-6
}
		
struct Tx {
	x f32
}

fn (s Tx) get() f32 {
	return s.x
}

fn test_struct_init() {
	c := 503
	d := Tx {
		x:  g(f32(c) / 255.0)
	}
	assert fabs(d.get() - 3.890949634755863) < 1.e-6
}

fn struct_init_return() Tx {
	c := 503
	return Tx {
		x:  g(f32(c) / 255.0)
	}
}

fn test_struct_init_return() {
	x := struct_init_return()
	assert fabs(fabs(x.get()) - 3.890949634755863) < 1.e-6
}

fn struct_init_ref_return() &Tx {
	c := 503
	return &Tx {
		x:  g(f32(c) / 255.0)
	}
}

fn test_struct_init_ref_return() {
	x := struct_init_ref_return()
	assert fabs(fabs(x.get()) - 3.890949634755863) < 1.e-6
}

fn test_f32_int() {
	x := f32(15.25)
	y := -3
	assert x + y == 12.25
	assert y + x == 12.25
	a := u32(34)
	assert a + x == 49.25
	b := i64(-17)
	c := 16.75
	assert c + b == -0.25
	d := u64(300)
	assert d + c == 316.75
}

fn test_rune() {
	a := 3
	mut b := rune(67)
	b = a
	assert b == rune(3)
	b = rune(67)
	mut x := 5
	x = int(b)
	assert x == 67
	c := b + a
	assert c == rune(70)
	assert typeof(c).name == 'rune'
	d := i64(12)
	e := b + d
	assert e == i64(79)
	assert typeof(e).name == 'i64'
}
