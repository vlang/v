module big

fn test_add_in_place () {
	mut a := [u32(1), 2, 3]
	mut b := [u32(5), 6, 7]
	add_in_place(mut a, b)
	assert a == [u32(6), 8, 10]

	a = [u32(11), 10, 11, 12]
	b = [u32(1), 2]
	add_in_place(mut a, b)
	assert a == [u32(12), 12, 11, 12]

	a = []u32{cap: 4}
	a << u32(1)
	a << u32(2)
	b = [u32(3), 4, 5, 6]
	add_in_place(mut a, b)
	assert a == [u32(4), 6, 5, 6]
}

fn test_clear_first_bits_and_set_some() {
	mut a := [u32(0xffffffff), 0xffffffff, 0xffffffff, 0xffffffff]
	mut b := []u32{len: 4, init: 0}
	clear_first_bits_and_set_some(a, 16, mut b)
	assert b == [u32(0xffffffff), 0xffffffff, 0xffffffff, 0xffff]

	a = [u32(0xffffffff), 0xffffffff, 0x0000ffff, 0x800]
	b = []u32{len: 4, init: 0}
	clear_first_bits_and_set_some(a, 20, mut b)
	assert b == [u32(0xffffffff), 0xffffffff, 0x00e0ffff, 0x0]
}

fn test_neg_in_place () {
	mut a := [u32(1), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0xffffffff), 0xffffffff, 0xffffffff]
	a = [u32(2), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0xfffffffe), 0xffffffff, 0xffffffff]
	a = [u32(3), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0xfffffffd), 0xffffffff, 0xffffffff]

	a = [u32(0), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0), 0, 0]

	a = [u32(0), 0, 1]
	neg_in_place(mut a)
	assert a == [u32(0), 0, 0xffffffff]

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

// fn test_divide_digit_array_04() {
// 	a := [u32(2), 4]
// 	b := [u32(0), 1]
// 	mut q := []u32{cap: a.len - b.len + 1}
// 	mut r := []u32{cap: a.len}

// 	divide_digit_array(a, b, mut q, mut r)
// 	assert q == [u32(4)]
// 	assert r == [u32(2)]
// }

// fn test_divide_digit_array_05() {
// 	a := [u32(2), 4, 5]
// 	b := [u32(0), 1]
// 	mut q := []u32{cap: a.len - b.len + 1}
// 	mut r := []u32{cap: a.len}

// 	divide_digit_array(a, b, mut q, mut r)
// 	assert q == [u32(4), 5]
// 	assert r == [u32(2)]
// }

// fn test_divide_digit_array_06() {
// 	a := [u32(2), 4, 5, 3]
// 	b := [u32(0), 0x8000]
// 	mut q := []u32{cap: a.len - b.len + 1}
// 	mut r := []u32{cap: a.len}

// 	divide_digit_array(a, b, mut q, mut r)
// 	assert q == [u32(0xa0000), 0x60000]
// 	assert r == [u32(2), 4]
// }
