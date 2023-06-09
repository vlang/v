fn f[T](src map[string]T) T {
	return src['a']
}

fn test_generic_fn_infer_map_arg() {
	r1 := f({
		'a': 1
	})
	println(r1)
	assert r1 == 1

	r2 := f({
		'a': 'hello'
	})
	println(r2)
	assert r2 == 'hello'
}
