module big

import rand

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
	a := [u64(2), 4, 5]
	b := [u64(0), 1]
	mut q := []u64{len: a.len - b.len + 1}
	mut r := []u64{len: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u64(4), 5]
	assert r == [u64(2)]
}

fn test_divide_digit_array_06() {
	a := [u64(2), 4, 5, 3]
	b := [u64(0), 0x8000]
	mut q := []u64{len: a.len - b.len + 1}
	mut r := []u64{len: a.len}

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
