fn foo<T>() string {
	x := fn () string {
		return 'ok'
	}
	return x()
}

fn test_generic_fn_with_anon_fn() {
	ret := foo<int>()
	assert ret == 'ok'
}
