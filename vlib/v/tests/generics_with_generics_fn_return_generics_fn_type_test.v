fn neg(a int) int {
	return -a
}

fn normal_v1(func fn (int) int) fn (int) int {
	assert typeof(func).name == typeof(neg).name
	return func
}

fn normal_v2(func fn (int) int) fn (int) int {
	f := func
	assert typeof(f).name == typeof(neg).name
	return f
}

fn generic_v1<T>(func T) T {
	assert T.name == typeof(neg).name
	assert typeof(func).name == typeof(neg).name
	return func
}

fn generic_v2<T>(func T) T {
	assert T.name == typeof(neg).name
	f := func
	assert typeof(f).name == typeof(neg).name
	return f
}

fn mixed_v1<T>(func T) fn (int) int {
	assert T.name == typeof(neg).name
	assert typeof(func).name == typeof(neg).name
	return func
}

fn mixed_v2<T>(func T) fn (int) int {
	assert T.name == typeof(neg).name
	f := func
	assert typeof(f).name == typeof(neg).name
	return f
}

fn test_generics_with_generics_fn_return_type() {
	mut f := neg
	assert f(1) == -1

	f = normal_v1(neg)
	assert f(2) == -2
	f = normal_v2(neg)
	assert f(3) == -3

	f = generic_v1(neg)
	assert f(4) == -4
	f = generic_v2(neg)
	assert f(5) == -5

	f = mixed_v1(neg)
	assert f(6) == -6
	f = mixed_v2(neg)
	assert f(7) == -7
}

fn neg_a(a int) int {
	return -a
}

fn neg_b(b int) int {
	return -b
}

// assume that "generic" is a heavy function and should be shared
// by *all* callers in this file
[noinline]
fn generic<T>(func T) T {
	return func
}

fn test_generics_with_generics_fn_return_type_v2() {
	mut f := neg_a
	assert f(1) == -1

	f = generic(neg_a)
	assert f(2) == -2
	f = generic(neg_b)
	assert f(3) == -3

	f = generic(fn (a int) int {
		return -a
	})
	assert f(4) == -4
	f = generic(fn (b int) int {
		return -b
	})
	assert f(5) == -5

	f = fn (a int) int {
		return -a
	}
	f = generic(f)
	assert f(6) == -6
	f = fn (b int) int {
		return -b
	}
	f = generic(f)
	assert f(7) == -7
}
