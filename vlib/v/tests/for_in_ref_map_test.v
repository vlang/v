fn test_for_in_ref_array_pointer() {
	mut mp := {
		'a': 1
		'b': 2
		'c': 3
		'd': 4
		'e': 5
	}
	mut rets := map[string]&int{}
	mut expects := map[string]&int{}

	for k, mut val in mp {
		expects[k] = unsafe { val }
	}
	for k, val in &mp {
		rets[k] = unsafe { val }
	}

	for k, val in &mp {
		assert voidptr(val) == voidptr(expects[k])
	}
	assert rets == expects
}
