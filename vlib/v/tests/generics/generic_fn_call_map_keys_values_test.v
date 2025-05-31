fn generic[T](a T) {
	assert a.keys() == ['aa', 'bb']
	assert a.values() == [11, 22]
}

fn test_generic_fn_call_map_keys_values() {
	a := {
		'aa': 11
		'bb': 22
	}
	generic(a)
}
