import rand

pub fn random_number() i64 {
	return rand.i64_in_range(111111, 999999) or { rand.i64() }
}

fn test_return_option_call_in_non_option_fn() {
	ret := random_number()
	println(ret)
	assert true
}
