fn test_generic_fn_infer_fn_type_using_ref_arg() {
	ret := call_generic_fn(fn (a &int) bool {
		return *a > 0
	}, 1)
	assert ret
}

fn call_generic_fn<T>(cb fn (&T) bool, input T) bool {
	dump(cb(&input))
	ret := cb(&input)
	return ret
}
