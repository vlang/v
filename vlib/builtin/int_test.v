const (
	a = 3
	u = u64(1)
)

fn test_const() {
	b := (true && true) || false
	assert b == true
	assert a == 3
	assert u == u64(1)
	assert u == 1 // make sure this works without the cast
}

fn test_float_equal_operator() {
	mut a := f32(1)
	a += 0.000001
	a -= 0.000001
	assert a == 1
	assert !a.eqbit(1)
	assert !(a != 1)
	assert a.nebit(1)
	a += 0.000001
	assert !(a < 1)
	assert !a.ltbit(1)
	assert !(a <= 1)
	assert !a.lebit(1)
	assert a > 1
	assert a.gtbit(1)
	assert a >= 1
	assert a.gebit(1)

	a = f64(1)
	a += 0.000001
	a -= 0.000001
	assert a == 1
	assert !a.eqbit(1)
	assert !(a != 1)
	a += 0.000001
	assert !(a < 1)
	assert !a.ltbit(1)
	assert !(a <= 1)
	assert !a.lebit(1)
	assert a > 1
	assert a.gtbit(1)
	assert a >= 1
	assert a.gebit(1)
}

fn test_str_methods() {
	assert i8(1).str() == '1'
	assert i8(-1).str() == '-1'
	assert i16(1).str() == '1'
	assert i16(-1).str() == '-1'
	assert int(1).str() == '1'
	assert int(-1).str() == '-1'
	assert i64(1).str() == '1'
	assert i64(-1).str() == '-1'

	// assert byte(1).str() == '1'
	// assert byte(-1).str() == '255'
	assert u16(1).str() == '1'
	assert u16(-1).str() == '65535'
	assert u32(1).str() == '1'
	assert u32(-1).str() == '4294967295'
	assert u64(1).str() == '1'
	assert u64(-1).str() == '18446744073709551615'
}

/*
fn test_cmp() {
	assert 1 ≠ 2
	assert 1 ⩽ 2
	assert 1 ⩾ 0
}
*/
