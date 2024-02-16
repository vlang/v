fn test_index_of_ints() {
	ia := [1, 2, 3]
	ii := ia.index(2)
	dump(ii)
	assert ii == 1
}

fn test_index_of_strings() {
	sa := ['a', 'b', 'c']
	si := sa.index('b')
	dump(si)
	assert si == 1
}

fn test_index_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]
	pi := pa.index(voidptr(45))
	dump(pi)
	assert pi == 1
}

//////////

fn a() {}

fn b() {}

fn c() {}

fn test_index_of_fns() {
	fa := [a, b, c]
	fi := fa.index(b)
	dump(fi)
	assert fi == 1
}
