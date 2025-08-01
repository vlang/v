module big

import rand

fn test_left_shift_in_place() {
	mut a := [u64(1), 1, 1, 1, 1]
	left_shift_in_place(mut a, 1)
	assert a == [u64(2), 2, 2, 2, 2]
	left_shift_in_place(mut a, 7)
	assert a == [u64(256), 256, 256, 256, 256]
	mut b := [u64(0x08000000_00000001), 0x0c000000_00000000, 0x08000000_00000000, 0x07ffffff_ffffffff]
	left_shift_in_place(mut b, 1)
	assert b == [u64(2), 0x08000000_00000001, 1, 0x0fffffff_ffffffff]
	mut c := [u64(0x00ffffff_ffffffff), 0x00f0f0f0_f0f0f0f0, 1, 0x03ffffff_ffffffff, 1]
	left_shift_in_place(mut c, 2)
	assert c == [u64(0x03ffffff_fffffffc), 0x03c3c3c3_c3c3c3c0, 4, 0x0fffffff_fffffffc, 4]
}

fn test_right_shift_in_place() {
	mut a := [u64(2), 2, 2, 2, 2]
	right_shift_in_place(mut a, 1)
	assert a == [u64(1), 1, 1, 1, 1]
	a = [u64(256), 256, 256, 256, 256]
	right_shift_in_place(mut a, 7)
	assert a == [u64(2), 2, 2, 2, 2]
	a = [u64(0), 0, 1]
	right_shift_in_place(mut a, 1)
	assert a == [u64(0), 0x08000000_00000000, 0]
	mut b := [u64(3), 0x08000000_00000001, 1, 0x0fffffff_ffffffff]
	right_shift_in_place(mut b, 1)
	assert b == [u64(0x08000000_00000001), 0x0c000000_00000000, 0x08000000_00000000,
		0x07ffffff_ffffffff]
	mut c := [u64(0x03ffffff), 0x03c3c3c3_c3c3c3c0, 7, 0xfffffffc, 4]
	right_shift_in_place(mut c, 2)
	assert c == [u64(0x00ffffff), 0x0cf0f0f0_f0f0f0f0, 1, 0x3fffffff, 1]
}

fn test_subtract_align_last_byte_in_place() {
	mut a := [u64(2), 2, 2, 2, 2]
	mut b := [u64(1), 1, 2, 1, 1]
	subtract_align_last_byte_in_place(mut a, b)
	assert a == [u64(1), 1, 0, 1, 1]

	a = [u64(0), 0, 0, 0, 1]
	b = [u64(0), 0, 1]
	subtract_align_last_byte_in_place(mut a, b)
	assert a == [u64(0), 0, 0, 0, 0]

	a = [u64(0), 0, 0, 0, 1, 13]
	b = [u64(1), 0, 1]
	mut c := []u64{len: a.len}
	mut d := [u64(0), 0, 0]
	d << b // to have same length
	subtract_digit_array(a, d, mut c)
	subtract_align_last_byte_in_place(mut a, b)
	assert a == [u64(0), 0, 0, u64(-1) & max_digit, 0, 12]
	assert c == a
}

fn test_greater_equal_from_end() {
	mut a := [u64(1), 2, 3, 4, 5, 6]
	mut b := [u64(3), 4, 5, 6]
	assert greater_equal_from_end(a, b) == true

	a = [u64(1), 2, 3, 4, 5, 6]
	b = [u64(1), 2, 3, 4, 5, 6]
	assert greater_equal_from_end(a, b) == true

	a = [u64(1), 2, 3, 4, 5, 6]
	b = [u64(2), 2, 3, 4, 5, 6]
	assert greater_equal_from_end(a, b) == false

	a = [u64(0), 0, 0, 4, 5, 6]
	b = [u64(4), 5, 6]
	assert greater_equal_from_end(a, b) == true

	a = [u64(0), 0, 0, 4, 5, 6]
	b = [u64(4), 6, 6]
	assert greater_equal_from_end(a, b) == false

	a = [u64(0), 0, 0, 4, 5, 5]
	b = [u64(4), 5, 6]
	assert greater_equal_from_end(a, b) == false
}

fn test_divide_digit_array_03() {
	a := [u64(0), 4]
	b := [u64(0), 1]
	mut q := []u64{cap: a.len - b.len + 1}
	mut r := []u64{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(4)]
	assert r == []u64{len: 0}
}

fn test_divide_digit_array_04() {
	a := [u64(2), 4]
	b := [u64(0), 1]
	mut q := []u64{cap: a.len - b.len + 1}
	mut r := []u64{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(4)]
	assert r == [u64(2)]
}

fn test_divide_digit_array_05() {
	a := [u64(2), 4, 5]
	b := [u64(0), 1]
	mut q := []u64{cap: a.len - b.len + 1}
	mut r := []u64{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(4), 5]
	assert r == [u64(2)]
}

fn test_divide_digit_array_06() {
	a := [u64(2), 4, 5, 3]
	b := [u64(0), 0x8000]
	mut q := []u64{cap: a.len - b.len + 1}
	mut r := []u64{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(0xa000_00000000), 0x6000_00000000]
	assert r == [u64(2), 4]
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
	res := integer_from_string(stri) or { panic(err) }
	return res
}
