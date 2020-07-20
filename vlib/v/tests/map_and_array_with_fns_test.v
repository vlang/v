fn foo(a string) int {
	return 10 + a.len
}

fn foo2(a string) int {
	return 20 + a.len
}

fn test_array_of_fns() {
	mut a := [foo, foo2]
	assert a.len == 2
	f0 := a[0]
	assert f0('xx') == 12
	f1 := a[1]
	assert f1('yyy') == 23
	a[0], a[1] = a[1], a[0]
	f2 := a[0]
	assert f2('zzzz') == 24
	f3 := a[1]
	assert f3('aaaaa') == 15
	mut b := [foo]
	b[0] = a[0]
	f4 := b[0]
	assert f4('bbbbbb') == 26
}

fn test_map_of_fns() {
	mut a := {'one':foo, 'two':foo2}
	assert a.len == 2
	f0 := a['one']
	assert f0('xx') == 12
}
