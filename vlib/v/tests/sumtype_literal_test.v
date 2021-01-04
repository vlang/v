type Foo = f32 | f64 | i16 | int | u64

fn test_assign_cast() {
	a := Foo(3)
	b := Foo(3.0)
	c := Foo(-7.25)
	d := Foo(i16(-4))
	e := Foo(f32(-0.0625))
	assert a is int
	assert b is f64
	assert b !is int
	assert c is f64
	assert d is i16
	assert e is f32
}

fn test_assign_mut() {
	mut a := Foo(0)
	mut b := Foo(i16(3))
	mut c := Foo(f32(1))
	a = 12.3
	b = -123456
	c = 33.
	assert a is f64
	assert b is int
	assert c is f64
}

fn test_call_arg() {
	assert get_type_idx(0.25) == 1
	assert get_type_idx(-7) == 3
	assert get_type_idx(u64(0)) == 4
	assert get_type_idx(f32(-0.125e-7)) == 0
}

fn get_type_idx(x Foo) int {
	match x {
		f32 {
			return 0
		}
		f64 {
			return 1
		}
		i16 {
			return 2
		}
		int {
			return 3
		}
		u64 {
			return 4
		}
	}
}

fn test_sumtype_return() {
	a := gen_foo(0)
	b := gen_foo(1)
	c := gen_foo(2)
	d := gen_foo(3)
	e := gen_foo(4)
	assert a is f32
	assert b is f64
	assert c is f64
	assert c !is int
	assert d is i16
	assert e is int
}

fn gen_foo(n int) Foo {
	if n == 0 {
		return f32(0.5)
	}
	if n == 1 {
		return -17.3e23
	}
	if n == 2 {
		return 32.
	}
	if n == 3 {
		return i16(13)
	}
	return 1
}
