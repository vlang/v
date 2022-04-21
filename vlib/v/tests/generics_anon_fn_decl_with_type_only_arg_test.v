fn test_generics_anon_fn_decl_with_type_only_arg() {
	ret := func_b<int>(11, 22, add)
	println(ret)
	assert ret == 33
}

fn add(a int, b int) int {
	return a + b
}

fn func_b<T>(x T, y T, f fn (T, T) T) T {
	return f(x, y)
}
