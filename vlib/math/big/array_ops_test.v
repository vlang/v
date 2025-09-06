// vtest build: !do_not_test ?
module big

fn test_add_digit_array_01() {
	a := [u64(1), 1, 1]
	b := [u64(1), 1, 1]
	mut c := []u64{len: 4}
	add_digit_array(a, b, mut c)

	assert c == [u64(2), 2, 2]
}

fn test_add_digit_array_02() {
	a := [u64(1), u64(1) << (digit_bits - 1), 1]
	b := [u64(1), u64(1) << (digit_bits - 1), 1]
	mut c := []u64{len: 4}
	add_digit_array(a, b, mut c)

	assert c == [u64(2), 0, 3]
}

fn test_add_digit_array_03() {
	a := [u64(1), (u64(1) << (digit_bits - 1)) + u64(34), 1]
	b := [u64(242), u64(1) << (digit_bits - 1), 1]
	mut c := []u64{len: 4}
	add_digit_array(a, b, mut c)

	assert c == [u64(243), 34, 3]
}

fn test_add_digit_array_04() {
	a := [u64(0)]
	b := [u64(1), 3, 4]
	mut c := []u64{len: 4}
	add_digit_array(a, b, mut c)

	assert c == [u64(1), 3, 4]
}

fn test_add_digit_array_05() {
	a := [u64(1), 3, 4]
	b := [u64(0)]
	mut c := []u64{len: 4}
	add_digit_array(a, b, mut c)

	assert c == [u64(1), 3, 4]
}

fn test_add_digit_array_06() {
	a := [u64(46), 13, 462, 13]
	b := [u64(1), 3, 4]
	mut c := []u64{len: 5}
	add_digit_array(a, b, mut c)

	assert c == [u64(47), 16, 466, 13]
}

fn test_subtract_digit_array_01() {
	a := [u64(2), 2, 2, 2, 2]
	b := [u64(1), 1, 2, 1, 1]
	mut c := []u64{len: a.len}
	subtract_digit_array(a, b, mut c)

	assert c == [u64(1), 1, 0, 1, 1]
}

fn test_subtract_digit_array_02() {
	a := [u64(0), 0, 0, 0, 1]
	b := [u64(0), 0, 1]
	mut c := []u64{len: a.len}
	subtract_digit_array(a, b, mut c)

	assert c == [u64(0), 0, u64(-1) & max_digit, u64(-1) & max_digit]
}

fn test_subtract_digit_array_03() {
	a := [u64(0), 0, 0, 0, 1, 13]
	b := [u64(0), 0, 1]
	mut c := []u64{len: a.len}
	subtract_digit_array(a, b, mut c)

	assert c == [u64(0), 0, u64(-1) & max_digit, u64(-1) & max_digit, 0, 13]
}

fn test_subtract_digit_array_04() {
	a := [u64(0x2), 0x4, 0x5, 0x3]
	b := [u64(0x0), 0x0, 0x5, 0x3]
	mut c := []u64{len: a.len}
	subtract_digit_array(a, b, mut c)
	assert c == [u64(0x2), 0x4]
}

fn test_multiply_digit_array_01() {
	a := [u64(0), 0, 0, 1]
	b := [u64(0), 0, 1]
	mut c := []u64{len: a.len + b.len}
	multiply_digit_array(a, b, mut c)

	assert c == [u64(0), 0, 0, 0, 0, 1]
}

fn test_multiply_digit_array_02() {
	a := []u64{len: 0}
	b := [u64(0), 0, 1]
	mut c := []u64{len: a.len + b.len}
	multiply_digit_array(a, b, mut c)

	assert c == []

	c = []u64{len: a.len + b.len}
	multiply_digit_array(b, a, mut c)

	assert c == []
}

fn test_compare_digit_array_01() {
	a := [u64(0), 0, 2]
	b := [u64(0), 0, 4]

	assert compare_digit_array(a, b) < 0
	assert compare_digit_array(b, a) > 0
	assert compare_digit_array(a, a) == 0
	assert compare_digit_array(b, b) == 0
}

fn test_compare_digit_array_02() {
	a := [u64(0), 0, 2324, 0, 124]
	b := [u64(0), 0, 4, 0, 0, 1]

	assert compare_digit_array(a, b) < 0
	assert compare_digit_array(b, a) > 0
	assert compare_digit_array(a, a) == 0
	assert compare_digit_array(b, b) == 0
}

fn test_divide_digit_array_01() {
	a := [u64(14)]
	b := [u64(2)]
	mut q := []u64{len: 1}
	mut r := []u64{len: 1}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(7)]
	assert r == []u64{len: 0}
}

fn test_divide_digit_array_02() {
	a := [u64(14)]
	b := [u64(15)]
	mut q := []u64{len: 1}
	mut r := []u64{len: 1}

	divide_digit_array(a, b, mut q, mut r)
	assert q == []u64{len: 0}
	assert r == a
}

fn test_divide_digit_array_03() {
	a := [u64(0), 4]
	b := [u64(0), 1]
	mut q := []u64{len: a.len - b.len + 1}
	mut r := []u64{len: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(4)]
	assert r == []u64{len: 0}
}

fn test_divide_digit_array_04() {
	a := [u64(2), 4]
	b := [u64(0), 1]
	mut q := []u64{len: a.len - b.len + 1}
	mut r := []u64{len: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(4)]
	assert r == [u64(2)]
}

fn test_divide_digit_array_05() {
	a := [u64(3)]
	b := [u64(2)]
	mut q := []u64{len: a.len - b.len + 1}
	mut r := []u64{len: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(1)]
	assert r == [u64(1)]
}

fn test_left_and_right_shift() {
	a := [u64(1), 1, 1]
	mut r := [u64(2), 2, 2]
	mut b := []u64{len: 3, init: 0}
	shift_digits_left(a, 1, mut b)
	assert r == b
	shift_digits_right(r, 1, mut r)
	assert r == a
	shift_digits_left(r, 1, mut r)
	assert r == b

	mut c := [u64(0x0fff_ffff_ffff_ffff)]
	shift_digits_left(c, 16, mut c)
	assert c == [u64(0x0fff_ffff_ffff_0000), u64(0xffff)]
	shift_digits_right(c, 8, mut c)
	assert c == [u64(0x0fff_ffff_ffff_ff00), 0xff]
	shift_digits_right(c, 16, mut c)
	assert c == [u64(0x000f_ffff_ffff_ffff)]
	shift_digits_right(c, 16, mut c)
	assert c == [u64(0x000f_ffff_ffff)]
	shift_digits_right(c, 16, mut c)
	assert c == [u64(0x000f_ffff)]
	shift_digits_right(c, 40, mut c)
	assert c == []u64{len: 0}
}

fn test_or_digit_array() {
	a := [u64(10), 10, 10]
	b := [u64(5), 5, 5]
	mut c := []u64{len: 3, init: 0}
	bitwise_or_digit_array(a, b, mut c)
	assert c == [u64(15), 15, 15]

	bitwise_or_digit_array(a, a, mut c)
	assert c == a

	x := [u64(10), 10, 10, 42, 42]
	y := [u64(2), 2, 5, 2]
	mut d := []u64{len: 5, init: 0}
	bitwise_or_digit_array(y, x, mut d)
	assert d == [u64(10), 10, 15, 42, 42]
}
