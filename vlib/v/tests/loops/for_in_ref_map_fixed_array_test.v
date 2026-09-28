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

fn test_referenced_map_fixed_array_value_pointer_can_be_rebound() {
	mut entries := {
		'a': [1, 2]!
	}
	mut replacement := [3, 4]!
	for _, mut value in &entries {
		// The borrowed stack array remains alive throughout the loop.
		unsafe {
			value = &replacement
			assert voidptr(value) == voidptr(&replacement)
			value[1] = 9
		}
	}
	assert entries['a'] == [1, 2]!
	assert replacement == [3, 9]!
}

fn test_mutable_map_fixed_array_value_assignment_updates_entry() {
	mut entries := {
		'a': [1, 2]!
	}
	for _, mut value in entries {
		value = [3, 4]!
	}
	assert entries['a'] == [3, 4]!
}

fn test_mutable_array_fixed_array_value_assignment_updates_entry() {
	mut rows := [[1, 2]!]
	for mut row in rows {
		row = [3, 4]!
	}
	assert rows[0] == [3, 4]!
	mut fixed := [[1, 2]!]!
	for mut row in fixed {
		row = [5, 6]!
	}
	assert fixed[0] == [5, 6]!
}
