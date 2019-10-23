struct VaTestGroup {
	name string
}

fn variadic_test(name string, groups ...VaTestGroup) {
	assert groups.len == 2
	assert groups[0].name == 'users' 
	assert groups[1].name == 'admins'
}

fn test_fn_variadic() {
	group1 := VaTestGroup{name: 'users'}
	group2 := VaTestGroup{name: 'admins'}
	variadic_test('joe', group1, group2)
}

fn variadic_test_generic<T>(a int, b ...T) T {
	assert a == 111
	b1 := b[0]
	b2 := b[1]
	assert b1 == 'hello' 
	assert b2 == 'v'
	return '$b1 $b2'
}

fn test_fn_variadic_generic() {
	assert variadic_test_generic(111, 'hello', 'v') == 'hello v'
}
