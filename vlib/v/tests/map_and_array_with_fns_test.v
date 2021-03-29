fn foo(a string, b string) int {
	return 10 + a.len + b.len
}

fn foo2(a string, b string) int {
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
	mut a := map{
		'one': foo
		'two': foo2
	}
	assert a.len == 2
	assert (a == map{
		'one': foo
		'two': foo2
	}) == true
	f0 := a['one']
	assert f0('xx', '') == 12
	f1 := a['two']
	assert f1('yyy', '') == 23
	a['one'], a['two'] = a['two'], a['one']
	f2 := a['one']
	assert f2('zzzz', '') == 24
	f3 := a['two']
	assert f3('aaaaa', '') == 15
	mut b := map{
		'one': foo
	}
	b['one'] = a['one']
	f4 := b['one']
	assert f4('bbbbbb', '') == 26
	for _, func in b {
		assert func('ccccccc', '') == 27
	}
}

fn foo3(a string) int {
	return 10 + a.len
}

fn foo4(a string) int {
	return 20 + a.len
}

fn test_map_and_array_with_fns_typeof_and_direct_call() {
	a := [foo3]
	assert typeof(a).name == '[]fn (string) int'
	assert a[0]('hello') == 15
	b := map{
		'one': foo3
	}
	assert typeof(b).name == 'map[string]fn (string) int'
	assert b['one']('hi') == 12
}

fn bar1(mut a []fn (string) int) int {
	a[0] = foo4
	return a[0]('hello')
}

fn bar2(a []fn (string) int) int {
	return a[0]('hello')
}

fn test_array_of_fns_as_argument() {
	mut a1 := [foo3]
	assert bar1(mut a1) == 25
	a2 := [foo3]
	assert bar2(a2) == 15
}

fn bar3(m map[string]fn (string) int) int {
	return m['fn']('hi')
}

fn bar4(mut m map[string]fn (string) int) int {
	m['fn'] = foo4
	return m['fn']('hi')
}

fn test_map_of_fns_as_argument() {
	m1 := map{
		'fn': foo3
	}
	assert bar3(m1) == 12
	mut m2 := map{
		'fn': foo3
	}
	assert bar4(mut m2) == 22
}
