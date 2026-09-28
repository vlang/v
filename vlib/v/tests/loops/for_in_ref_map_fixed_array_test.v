fn test_referenced_map_fixed_array_values_retain_entry_storage() {
	mut entries := {
		'a': [1, 2]!
	}
	mut retained := voidptr(0)
	for _, mut value in &entries {
		retained = voidptr(value)
	}
	unsafe {
		assert (&int(retained))[0] == 1
		(&int(retained))[1] = 5
	}
	assert entries['a'][1] == 5
	entries['a'] = [3, 4]!
	unsafe {
		assert (&int(retained))[0] == 3
	}
}
