struct VaTestGroup {
	name string
}

fn variadic_test_a(name string, groups ...VaTestGroup) {
	assert groups.len == 2
	assert groups[0].name == 'users' 
	assert groups[1].name == 'admins'
}

fn test_fn_variadic() {
	group1 := VaTestGroup{name: 'users'}
	group2 := VaTestGroup{name: 'admins'}
	variadic_test_a('joe', group1, group2)
}
