fn foo(a, b string) int {
	return 10 + a.len + b.len
}

fn foo2(a, b string) int {
	return 20 + a.len + b.len
}

fn test_array_with_fns() {
	mut a := [foo, foo2]
	assert a.len == 2
	assert (a != [foo, foo2]) == false
	assert (foo in a) == true
	f0 := a[0]
	assert f0('xx', '') == 12
	f1 := a[1]
	assert f1('yyy', '') == 23
	a[0], a[1] = a[1], a[0]
	f2 := a[0]
	assert f2('zzzz', '') == 24
	f3 := a[1]
	assert f3('aaaaa', '') == 15
	mut b := [foo]
	assert (foo2 !in b) == true
	b[0] = a[0]
	f4 := b[0]
	assert f4('bbbbbb', '') == 26
	for func in b {
		assert func('ccccccc', '') == 27
	}
	b = []
	b << foo
	b << [foo2]
	assert (b == [foo, foo2]) == true
	f5 := b[0]
	assert f5('dddddddd', '') == 18
}

fn test_map_with_fns() {
	mut a := {'one':foo, 'two':foo2}
	assert a.len == 2
	assert (a == {'one':foo, 'two':foo2}) == true
	f0 := a['one']
	assert f0('xx', '') == 12
	f1 := a['two']
	assert f1('yyy', '') == 23
	a['one'], a['two'] = a['two'], a['one']
	f2 := a['one']
	assert f2('zzzz', '') == 24
	f3 := a['two']
	assert f3('aaaaa', '') == 15
	mut b := {'one':foo}
	b['one'] = a['one']
	f4 := b['one']
	assert f4('bbbbbb', '') == 26
	for _, func in b {
		assert func('ccccccc', '') == 27
	}
}
