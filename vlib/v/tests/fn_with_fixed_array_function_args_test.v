fn foo(a string) int {
	return 10 + a.len
}

fn foo2(a string) int {
	return 20 + a.len
}

fn bar1(mut a [1]fn (string) int) int {
        a[0] = foo2
	return a[0]('hello')
}

fn bar2(a [1]fn (string) int) int {
	return a[0]('hello')
}

fn test_fn_with_fixed_array_function_args() {
	mut a1 := [foo]!
	assert bar1(mut a1) == 25
	a2 := [foo]!
	assert bar2(a2) == 15
}
