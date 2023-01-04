module big

import rand

fn test_lshift_in_place() {
	mut a := [u32(1), 1, 1, 1, 1]
	lshift_in_place(mut a, 1)
	assert a == [u32(2), 2, 2, 2, 2]
	lshift_in_place(mut a, 7)
	assert a == [u32(256), 256, 256, 256, 256]
	mut b := [u32(0x80000001), 0xc0000000, 0x80000000, 0x7fffffff]
	lshift_in_place(mut b, 1)
	assert b == [u32(2), 0x80000001, 1, 0xffffffff]
	mut c := [u32(0x00ffffff), 0xf0f0f0f0, 1, 0x3fffffff, 1]
	lshift_in_place(mut c, 2)
	assert c == [u32(0x3fffffc), 0xc3c3c3c0, 7, 0xfffffffc, 4]
}

fn test_rshift_in_place() {
	mut a := [u32(2), 2, 2, 2, 2]
	rshift_in_place(mut a, 1)
	assert a == [u32(1), 1, 1, 1, 1]
	a = [u32(256), 256, 256, 256, 256]
	rshift_in_place(mut a, 7)
	assert a == [u32(2), 2, 2, 2, 2]
	a = [u32(0), 0, 1]
	rshift_in_place(mut a, 1)
	assert a == [u32(0), 0x80000000, 0]
	mut b := [u32(3), 0x80000001, 1, 0xffffffff]
	rshift_in_place(mut b, 1)
	assert b == [u32(0x80000001), 0xc0000000, 0x80000000, 0x7fffffff]
	mut c := [u32(0x03ffffff), 0xc3c3c3c0, 7, 0xfffffffc, 4]
	rshift_in_place(mut c, 2)
	assert c == [u32(0x00ffffff), 0xf0f0f0f0, 1, 0x3fffffff, 1]
}

fn test_subtract_align_last_byte_in_place() {
	mut a := [u32(2), 2, 2, 2, 2]
	mut b := [u32(1), 1, 2, 1, 1]
	subtract_align_last_byte_in_place(mut a, b)
	assert a == [u32(1), 1, 0, 1, 1]

	a = [u32(0), 0, 0, 0, 1]
	b = [u32(0), 0, 1]
	subtract_align_last_byte_in_place(mut a, b)
	assert a == [u32(0), 0, 0, 0, 0]

	a = [u32(0), 0, 0, 0, 1, 13]
	b = [u32(1), 0, 1]
	mut c := []u32{len: a.len}
	mut d := [u32(0), 0, 0]
	d << b // to have same length
	subtract_digit_array(a, d, mut c)
	subtract_align_last_byte_in_place(mut a, b)
	assert a == [u32(0), 0, 0, u32(-1), 0, 12]
	assert c == a
}

fn test_greater_equal_from_end() {
	mut a := [u32(1), 2, 3, 4, 5, 6]
	mut b := [u32(3), 4, 5, 6]
	assert greater_equal_from_end(a, b) == true

	a = [u32(1), 2, 3, 4, 5, 6]
	b = [u32(1), 2, 3, 4, 5, 6]
	assert greater_equal_from_end(a, b) == true

	a = [u32(1), 2, 3, 4, 5, 6]
	b = [u32(2), 2, 3, 4, 5, 6]
	assert greater_equal_from_end(a, b) == false

	a = [u32(0), 0, 0, 4, 5, 6]
	b = [u32(4), 5, 6]
	assert greater_equal_from_end(a, b) == true

	a = [u32(0), 0, 0, 4, 5, 6]
	b = [u32(4), 6, 6]
	assert greater_equal_from_end(a, b) == false

	a = [u32(0), 0, 0, 4, 5, 5]
	b = [u32(4), 5, 6]
	assert greater_equal_from_end(a, b) == false
}

fn test_divide_digit_array_03() {
	a := [u32(0), 4]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u32(4)]
	assert r == []u32{len: 0}
}

fn test_divide_digit_array_04() {
	a := [u32(2), 4]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u32(4)]
	assert r == [u32(2)]
}

fn test_divide_digit_array_05() {
	a := [u32(2), 4, 5]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u32(4), 5]
	assert r == [u32(2)]
}

fn test_divide_digit_array_06() {
	a := [u32(2), 4, 5, 3]
	b := [u32(0), 0x8000]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u32(0xa0000), 0x60000]
	assert r == [u32(2), 4]
}

fn test_many_divisions() {
	for _ in 0 .. 100 {
		a := random_number(30)
		b := random_number(30)
		c := a * b
		assert c / a == b
		assert c / b == a
		q, r := a.div_mod(b)
		assert (q * b) + r == a
	}
}

fn random_number(length int) Integer {
	numbers := '0123456789'
	mut stri := ''
	for _ in 0 .. length {
		i := rand.intn(10) or { 0 }
		nr := numbers[i]
		stri = stri + nr.ascii_str()
	}
	res := integer_from_string(stri) or { panic('error in random_number') }
	return res
}
