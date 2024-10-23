fn test_any_all_of_ints() {
	ia := [1, 2, 3]!

	assert ia.any(it > 2)
	assert ia.any(|x| x > 2)

	assert !ia.all(it > 1)
	assert !ia.all(|x| x > 1)

	assert ia.any(it == 2)
	assert ia.any(|x| x == 2)

	assert !ia.all(it == 3)
	assert !ia.all(|x| x == 3)
}

fn test_any_all_of_strings() {
	sa := ['a', 'b', 'c']!

	assert sa.any(it == 'b')
	assert sa.any(|x| x == 'b')

	assert !sa.all(it == 'c')
	assert !sa.all(|x| x == 'c')
}

fn test_any_all_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!

	assert pa.any(it == voidptr(45))
	assert pa.any(|x| x == voidptr(45))

	assert !pa.all(it == voidptr(123))
	assert !pa.all(|x| x == voidptr(123))
}

fn a() {}

fn b() {}

fn c() {}

fn v() {}

fn test_any_all_of_fns() {
	fa := [a, b, c]!

	assert fa.any(it == b)
	// assert fa.any(|x| x == b)

	assert !fa.all(it == v)
	// assert !fa.all(|x| x == v)
}
