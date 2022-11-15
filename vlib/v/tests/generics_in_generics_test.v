fn gen_fn2<T>(e T) string {
	return '${e.str()}'
}

fn gen_fn<T>(e T) string {
	return gen_fn2<T>(e)
}

fn test_generics_in_generics() {
	assert gen_fn(u64(42)) == '42'
	assert gen_fn('42') == '42'
}
