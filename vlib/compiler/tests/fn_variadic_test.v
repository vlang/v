struct VaTestGroup {
	name string
}

// basic
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

// generic
fn variadic_test_generic<T>(a int, b ...T) T {
	b1 := b[0]
	b2 := b[1]
	return '$a $b1 $b2'
}

fn test_fn_variadic_generic() {
	assert variadic_test_generic(111, 'hello', 'v') == '111 hello v'
}

// forwarding
fn variadic_forward_a(a ...string) string {
	return variadic_forward_b(a)
}

fn variadic_forward_b(a ...string) string {
	a0 := a[0]
	a1 := a[1]
	a2 := a[2]
	return '$a0$a1$a2'
}

fn test_fn_variadic_forward() {
	assert variadic_forward_a('a', 'b', 'c') == 'abc'
}
