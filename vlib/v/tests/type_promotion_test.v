fn test_anyfloat() {
	a := f64(5.765) + 45.75
	b := 12.3 + f64(3)
	c := f32(6.75) / 3.0
	d := 16.5 / f32(2)
	assert a == 51.515
	assert typeof(a) == 'f64'
	assert b == f64(15.3)
	assert typeof(b) == 'f64'
	assert c == 2.25
	assert typeof(c) == 'f32'
	assert d == 8.25
	assert typeof(d) == 'f32'
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

