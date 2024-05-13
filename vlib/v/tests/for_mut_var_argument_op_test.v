fn test_for_mut_var_assign_add_string() {
	mut arr := ['a', 'b', 'c']
	for mut c in arr {
		c += 'd'
	}
	assert arr == ['ad', 'bd', 'cd']
}

fn test_for_mut_var_assign_minus_int() {
	mut arr := [12, 13, 14]
	for mut c in arr {
		c -= 2
	}
	assert arr == [10, 11, 12]
}
