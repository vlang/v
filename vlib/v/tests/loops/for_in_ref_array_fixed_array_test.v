fn test_mutable_referenced_array_fixed_array_element_can_be_rebound() {
	mut rows := [[1, 2]!]
	mut replacement := [3, 4]!
	for mut row in &rows {
		// The replacement remains alive for the entire loop.
		unsafe {
			row = &replacement
			assert voidptr(row) == voidptr(&replacement)
			row[1] = 9
		}
	}
	assert rows[0] == [1, 2]!
	assert replacement == [3, 9]!
}

fn test_parenthesized_referenced_array_fixed_array_element_can_be_rebound() {
	mut rows := [[1, 2]!]
	mut replacement := [3, 4]!
	for mut row in (&rows) {
		// The replacement remains alive for the entire loop.
		unsafe {
			row = &replacement
			row[1] = 9
		}
	}
	assert rows[0] == [1, 2]!
	assert replacement == [3, 9]!
}

fn test_referenced_fixed_array_rows_can_be_rebound() {
	mut rows := [[1, 2]!]!
	mut replacement := [3, 4]!
	// The array remains alive for the entire loop.
	mut rows_ref := unsafe { &rows }
	for mut row in rows_ref {
		// The replacement remains alive for the entire loop.
		unsafe {
			row = &replacement
			row[1] = 9
		}
	}
	assert rows[0] == [1, 2]!
	assert replacement == [3, 9]!
}

fn assign_mutable_fixed_array_rows(mut rows [][2]int) {
	for mut row in rows {
		row = [3, 4]!
	}
}

fn test_mutable_array_parameter_still_updates_fixed_array_elements() {
	mut rows := [[1, 2]!]
	assign_mutable_fixed_array_rows(mut rows)
	assert rows[0] == [3, 4]!
}

fn test_nested_iteration_of_pointer_valued_referenced_array_keeps_references() {
	mut numbers := [1, 2]
	mut groups := [&numbers]
	for mut values in &groups {
		for number in values {
			unsafe {
				assert *number > 0
			}
		}
	}
}
