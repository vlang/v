fn call<T>(v T) {
}

fn simple<T>(p T) T {
	return p
}

fn test_infer() {
	call(3)
	i := 4
	r := simple(i)
	assert r == 4
}

fn test_explicit_calls_should_also_work() {
	call<int>(2)
	assert true
	simple<int>(5)
	assert true
}

fn get_type_name<T>(x T) string {
	return T.name
}

fn test_literal() {
	assert get_type_name(1) == 'int'
	assert get_type_name(1.0) == 'f64'
}

//
fn choose4<T>(a T, b T, c T, d T) T {
	// Note: a similar construct is used in prime31's via engine
	return a
}

fn test_calling_generic_fn_with_many_params() {
	x := choose4(1, 2, 3, 4)
	assert x == 1
	y := choose4<string>('abc', 'xyz', 'def', 'ghi')
	assert y == 'abc'
}
