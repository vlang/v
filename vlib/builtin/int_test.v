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

fn test_and() {
	c:=[1,2,3,4,5]
	assert c[0] & 1 != 0
	assert c[1] & 1 == 0
	assert c[2] & 1 != 0
	assert c[3] & 1 == 0
	assert c[4] & 1 != 0
}

fn test_i8_print() {
	b := i8(0)
	println(b)
	c := i16(7)
	println(c)
	d := u16(6)
	println(d)
	assert true
}

/*
fn test_cmp() {
	assert 1 ≠ 2
	assert 1 ⩽ 2
	assert 1 ⩾ 0
}
*/

type myint int
type mystring string

fn test_int_alias() {
	/*
	i := myint(2)
	s := mystring('hi')
	ss := s + '!'
	assert i + 10 == 12
	*/
}
