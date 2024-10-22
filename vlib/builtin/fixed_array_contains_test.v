fn test_contains_of_ints() {
	ia := [1, 2, 3]!
	mut ii := ia.contains(2)
	dump(ii)
	assert ii

	ii = ia.contains(5)
	dump(ii)
	assert !ii
}

fn test_contains_of_strings() {
	sa := ['a', 'b', 'c']!
	mut si := sa.contains('b')
	dump(si)
	assert si

	si = sa.contains('v')
	dump(si)
	assert !si
}

fn test_contains_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!
	mut pi := pa.contains(voidptr(45))
	dump(pi)
	assert pi

	pi = pa.contains(unsafe { nil })
	dump(pi)
	assert !pi
}

fn a() {}

fn b() {}

fn c() {}

fn v() {}

fn test_contains_of_fns() {
	fa := [a, b, c]!
	mut fi := fa.contains(b)
	dump(fi)
	assert fi

	fi = fa.contains(v)
	dump(fi)
	assert !fi
}
