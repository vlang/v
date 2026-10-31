fn verify_nested_reference_values(mut row []int, refs []&int) {
	assert refs.len == 2
	unsafe {
		assert refs[0] == &row[0]
		assert refs[1] == &row[1]
		assert *refs[0] == 3
		*refs[1] = 9
	}
	assert row == [3, 9]
}

fn test_ref_map_pointer_array_entries() {
	mut row := [3, 5]
	mut entries := {
		'a': &row
	}
	mut refs := []&int{}
	for _, mut values in &entries {
		for item in values { refs << item }
	}
	verify_nested_reference_values(mut row, refs)
}

fn test_ref_array_pointer_array_entries() {
	mut row := [3, 5]
	mut entries := [&row]
	mut refs := []&int{}
	for mut values in &entries {
		for item in values { refs << item }
	}
	verify_nested_reference_values(mut row, refs)
}

fn test_ref_fixed_array_pointer_array_entries() {
	mut row := [3, 5]
	mut entries := [&row]!
	// The fixed-array storage remains live throughout the loop.
	mut entry_pointer := unsafe { &entries }
	mut refs := []&int{}
	for _, mut values in entry_pointer {
		for item in values { refs << item }
	}
	verify_nested_reference_values(mut row, refs)
}

fn test_ref_map_pointer_fixed_array_entries() {
	mut row := [3, 5]!
	// The borrowed fixed array remains live throughout the loop.
	row_pointer := unsafe { &row }
	mut entries := {
		'a': row_pointer
	}
	mut refs := []&int{}
	for _, mut values in &entries {
		for item in values { refs << item }
	}
	assert refs.len == 2
	unsafe { *refs[1] = 9 }
	assert row == [3, 9]!
}

fn test_ref_array_pointer_map_entries() {
	mut row := {
		'a': 3
	}
	mut entries := [&row]
	mut refs := []&int{}
	for mut values in &entries {
		for _, item in values { refs << item }
	}
	assert refs.len == 1
	unsafe { *refs[0] = 9 }
	assert row['a'] == 9
}

fn test_ref_map_parenthesized_pointer_array_entries() {
	mut row := [3, 5]
	mut entries := {
		'a': &row
	}
	mut refs := []&int{}
	// vfmt off
	for _, mut values in (&entries) {
		for item in ((values)) { refs << item }
	}
	// vfmt on
	verify_nested_reference_values(mut row, refs)
}

fn test_value_map_pointer_array_entries_keep_value_iteration() {
	mut row := [3, 5]
	mut entries := {
		'a': &row
	}
	mut copied := []int{}
	for _, mut values in entries {
		for item in values { copied << item }
	}
	assert copied == [3, 5]
	assert row == [3, 5]
}

fn test_shadowed_reference_binding_in_literal() {
	mut row := [3, 5]
	mut entries := {
		'a': &row
	}
	mut retained := []&int{}
	for _, mut values in &entries {
		sum := fn (mut values []int) int {
			mut total := 0
			for item in values { total += item }
			return total
		}
		mut numbers := [2, 4]
		assert sum(mut numbers) == 6
		for item in values { retained << item }
	}
	unsafe { *retained[1] = 9 }
	assert row == [3, 9]
}

fn test_captured_reference_container_keeps_nested_references() {
	mut row := [3, 5]
	mut entries := {
		'a': &row
	}
	for _, mut values in &entries {
		read := fn [values] () []&int {
			mut refs := []&int{}
			for item in values { refs << item }
			return refs
		}
		refs := read()
		verify_nested_reference_values(mut row, refs)
	}
}

fn test_optional_map_entries_through_reference_variable() {
	mut entries := {
		'a': ?int(3)
		'b': ?int(none)
	}
	mut reference := &entries
	mut total := 0
	for _, mut value in reference {
		total += value or { continue }
	}
	assert total == 3
	total = 0
	// vfmt off
	for _, mut value in (reference) {
		total += value or { continue }
	}
	// vfmt on
	assert total == 3
	total = 0
	for _, mut value in &entries {
		total += value or { continue }
	}
	assert total == 3
}

fn test_mutable_optional_map_entries_keep_entry_storage() {
	mut entries := {
		'a': ?int(3)
	}
	for _, mut value in entries {
		value = 9
	}
	assert (entries['a'] or { 0 }) == 9
}
