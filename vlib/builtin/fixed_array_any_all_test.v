fn test_any_all_of_ints() {
	ia := [1, 2, 3]!

	assert ia.any(it > 2)
	assert ia.any(|x| x > 2)
	assert [1, 2, 3]!.any(it > 2)
	assert [1, 2, 3]!.any(|x| x > 2)

	assert !ia.all(it > 1)
	assert !ia.all(|x| x > 1)
	assert ![1, 2, 3]!.all(it > 1)
	assert ![1, 2, 3]!.all(|x| x > 1)

	assert ia.any(it == 2)
	assert ia.any(|x| x == 2)
	assert [1, 2, 3]!.any(it == 2)
	assert [1, 2, 3]!.any(|x| x == 2)

	assert !ia.all(it == 3)
	assert !ia.all(|x| x == 3)
	assert ![1, 2, 3]!.all(it == 3)
	assert ![1, 2, 3]!.all(|x| x == 3)
}

fn test_any_all_of_strings() {
	sa := ['a', 'b', 'c']!

	assert sa.any(it == 'b')
	assert sa.any(|x| x == 'b')
	assert ['a', 'b', 'c']!.any(it == 'b')
	assert ['a', 'b', 'c']!.any(|x| x == 'b')

	assert !sa.all(it == 'c')
	assert !sa.all(|x| x == 'c')
	assert !['a', 'b', 'c']!.all(it == 'c')
	assert !['a', 'b', 'c']!.all(|x| x == 'c')
}

fn test_any_all_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!

	assert pa.any(it == voidptr(45))
	assert pa.any(|x| x == voidptr(45))
	assert [voidptr(123), voidptr(45), voidptr(99)]!.any(it == voidptr(45))
	assert [voidptr(123), voidptr(45), voidptr(99)]!.any(|x| x == voidptr(45))

	assert !pa.all(it == voidptr(123))
	assert !pa.all(|x| x == voidptr(123))
	assert ![voidptr(123), voidptr(45), voidptr(99)]!.all(it == voidptr(123))
	assert ![voidptr(123), voidptr(45), voidptr(99)]!.all(|x| x == voidptr(123))
}

fn a() {}

fn b() {}

fn c() {}

fn v() {}

fn test_any_all_of_fns() {
	fa := [a, b, c]!

	assert fa.any(it == b)
	assert fa.any(|x| x == b)
	assert [a, b, c]!.any(it == b)
	assert [a, b, c]!.any(|x| x == b)

	assert !fa.all(it == v)
	assert !fa.all(|x| x == v)
	assert ![a, b, c]!.all(it == v)
	assert ![a, b, c]!.all(|x| x == v)
}
