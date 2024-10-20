fn test_index_of_ints() {
	ia := [1, 2, 3]!
	mut ii := ia.index(2)
	dump(ii)
	assert ii == 1

	ii = ia.index(5)
	dump(ii)
	assert ii == -1
}

fn test_index_of_strings() {
	sa := ['a', 'b', 'c']!
	mut si := sa.index('b')
	dump(si)
	assert si == 1

	si = sa.index('v')
	dump(si)
	assert si == -1
}

fn test_index_of_voidptrs() {
	pa := [voidptr(123), voidptr(45), voidptr(99)]!
	mut pi := pa.index(voidptr(45))
	dump(pi)
	assert pi == 1

	pi = pa.index(unsafe { nil })
	dump(pi)
	assert pi == -1
}

fn a() {}

fn b() {}

fn c() {}

fn v() {}

fn test_index_of_fns() {
	fa := [a, b, c]!
	mut fi := fa.index(b)
	dump(fi)
	assert fi == 1

	fi = fa.index(v)
	dump(fi)
	assert fi == -1
}
