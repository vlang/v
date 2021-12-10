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
	group1 := VaTestGroup{
		name: 'users'
	}
	group2 := VaTestGroup{
		name: 'admins'
	}
	variadic_test('joe', group1, group2)
}

/*
// QTODO
// generic
fn variadic_test_generic<T>(a int, b ...T) T {
	b1 := b[0]
	b2 := b[1]
	return '$a $b1 $b2'
}

fn test_fn_variadic_generic() {
	assert variadic_test_generic(111, 'hello', 'v') == '111 hello v'
}
*/
// forwarding
fn variadic_forward_a(a ...string) string {
	return variadic_fn_a(...a)
}

fn variadic_fn_a(a ...string) string {
	a0 := a[0]
	a1 := a[1]
	a2 := a[2]
	return '$a0$a1$a2'
}

fn test_fn_variadic_forward() {
	assert variadic_forward_a('a', 'b', 'c') == 'abc'
}

fn fn_variadic_with_arg_no_vargs(name string, groups ...VaTestGroup) {
	assert groups.len == 0
}

fn test_fn_variadic_with_arg_no_vargs() {
	fn_variadic_with_arg_no_vargs('marko')
}

fn fn_variadic_only_with_no_vargs(groups ...VaTestGroup) {
	assert groups.len == 0
}

fn test_variadic_only_with_no_vargs() {
	fn_variadic_only_with_no_vargs()
}

fn test_array_decomposition_to_vargs() {
	a := ['a', 'b', 'c']
	assert variadic_fn_a(...a) == 'abc'
}

struct VaTestStruct {
}

fn (a VaTestStruct) variadic_method(name string, groups ...VaTestGroup) {
	assert groups.len == 2
	assert groups[0].name == 'users'
	assert groups[1].name == 'admins'
}

fn (a VaTestStruct) variadic_method_no_args(name string, groups ...VaTestGroup) {
	assert groups.len == 0
}

fn test_fn_variadic_method() {
	a := VaTestStruct{}
	group1 := VaTestGroup{
		name: 'users'
	}
	group2 := VaTestGroup{
		name: 'admins'
	}
	a.variadic_method('marko', group1, group2)
}

fn test_fn_variadic_method_no_args() {
	a := VaTestStruct{}
	a.variadic_method_no_args('marko')
}
