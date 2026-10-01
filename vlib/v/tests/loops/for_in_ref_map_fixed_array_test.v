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

fn test_optional_map_value_through_reference_variable_stays_by_value() {
	mut entries := {
		'a': ?int(41)
	}
	mut ref := &entries
	mut seen := false
	for _, mut value in ref {
		value = ?int(42)
		if number := value {
			assert number == 42
			seen = true
		}
	}
	assert seen
	if number := entries['a'] {
		assert number == 41
	} else {
		assert false
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

fn update_parenthesized_mutable_map(mut entries map[string][2]int) {
	// vfmt off
	for _, mut value in (entries) {
		value = [3, 4]!
	}
	// vfmt on
}

fn test_parenthesized_mutable_map_fixed_array_value_updates_entry() {
	mut entries := {
		'a': [1, 2]!
	}
	update_parenthesized_mutable_map(mut entries)
	assert entries['a'] == [3, 4]!
	mut maps := [entries]
	for mut nested in maps {
		// vfmt off
		for _, mut value in (nested) {
			value = [5, 6]!
		}
		// vfmt on
	}
	assert maps[0]['a'] == [5, 6]!
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

fn replace_parenthesized_map_values(mut entries map[string]int) {
	// Preserve the parentheses that exercise iterable classification.
	// vfmt off
	for _, mut value in ((entries)) {
		value = 41
	}
	// vfmt on
}

fn replace_parenthesized_fixed_array_map_values(mut entries map[string][2]int) {
	// vfmt off
	for _, mut value in ((entries)) {
		value = [5, 6]!
	}
	// vfmt on
}

fn test_parenthesized_mutable_map_parameters_update_entries() {
	mut entries := {
		'a': 1
	}
	replace_parenthesized_map_values(mut entries)
	assert entries['a'] == 41
	mut arrays := {
		'a': [1, 2]!
	}
	replace_parenthesized_fixed_array_map_values(mut arrays)
	assert arrays['a'] == [5, 6]!
}

fn test_parenthesized_outer_mutable_map_binding_updates_entries() {
	mut maps := [{
		'a': 1
	}]
	for mut entries in maps {
		// vfmt off
		for _, mut value in ((entries)) {
			value = 51
		}
		// vfmt on
	}
	assert maps[0]['a'] == 51
	mut arrays := [{
		'a': [1, 2]!
	}]
	for mut entries in arrays {
		// vfmt off
		for _, mut value in ((entries)) {
			value = [7, 8]!
		}
		// vfmt on
	}
	assert arrays[0]['a'] == [7, 8]!
}

fn test_parenthesized_explicit_map_reference_keeps_pointer_rebinding() {
	mut entries := {
		'a': [1, 2]!
	}
	mut replacement := [3, 4]!
	for _, mut value in (&entries) {
		// The borrowed stack array remains alive throughout this loop.
		unsafe {
			value = &replacement
			value[1] = 9
		}
	}
	assert entries['a'] == [1, 2]!
	assert replacement == [3, 9]!
}

struct ExplicitMutableMapItem {
mut:
	number int
}

fn copy_explicit_mutable_map_value(mut entries &map[string]ExplicitMutableMapItem) ExplicitMutableMapItem {
	// vfmt off
	for _, value in (entries) {
		mut copied := value
		copied.number += 4
		return copied
	}
	// vfmt on
	return ExplicitMutableMapItem{}
}

fn assign_explicit_mutable_map_rows(mut entries &map[string][2]int) {
	for _, mut row in entries {
		row = [7, 8]!
	}
}

fn sum_explicit_mutable_map_values(mut entries &map[string]int) int {
	mut total := 0
	for _, value in entries {
		total += value
	}
	return total
}

fn test_explicit_mutable_map_parameters_keep_value_iteration() {
	mut items := {
		'a': ExplicitMutableMapItem{ number: 3 }
	}
	mut item_pointer := &items
	copied := copy_explicit_mutable_map_value(mut item_pointer)
	assert copied.number == 7
	assert items['a'].number == 3
	mut rows := {
		'a': [1, 2]!
	}
	mut row_pointer := &rows
	assign_explicit_mutable_map_rows(mut row_pointer)
	assert rows['a'] == [7, 8]!
	mut numbers := {
		'a': 3
		'b': 4
	}
	mut number_pointer := &numbers
	assert sum_explicit_mutable_map_values(mut number_pointer) == 7
}

fn test_nested_iteration_of_pointer_valued_referenced_map_keeps_references() {
	mut numbers := [1, 2]
	mut entries := {
		'values': &numbers
	}
	for _, mut values in &entries {
		for number in values {
			unsafe {
				assert *number > 0
			}
		}
	}
}
