fn test_any_all_of_ints() {
	ia := [1, 2, 3]!
	mut ii := ia.any(it > 2)
	println(ii)
	assert ii

	ii = ia.all(it > 1)
	println(ii)
	assert !ii

	ii = ia.any(it == 2)
	println(ii)
	assert ii

	ii = ia.all(it == 3)
	println(ii)
	assert !ii
}

fn test_any_all_of_strings() {
	sa := ['a', 'b', 'c']!
	mut si := sa.any(it == 'b')
	println(si)
	assert si

	si = sa.all(it == 'c')
	println(si)
	assert !si
}

fn test_any_all_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!
	mut pi := pa.any(it == voidptr(45))
	println(pi)
	assert pi

	pi = pa.all(it == voidptr(123))
	println(pi)
	assert !pi
}

fn a() {}

fn b() {}

fn c() {}

fn v() {}

fn test_any_all_of_fns() {
	fa := [a, b, c]!
	mut fi := fa.any(it == b)
	println(fi)
	assert fi

	fi = fa.all(it == v)
	println(fi)
	assert !fi
}
