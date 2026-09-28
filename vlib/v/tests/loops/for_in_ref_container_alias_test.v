type FixedEntryMapRef = &map[string][2]int
type NestedFixedEntryMapRef = FixedEntryMapRef
type FixedEntryMap = map[string][2]int
type AliasRows = [][2]int
type AliasRowsRef = &AliasRows
type NestedAliasRowsRef = AliasRowsRef
type AliasFixedRows = [1][2]int
type AliasFixedRowsRef = &AliasFixedRows

fn test_pointer_alias_map_binding_can_be_rebound() {
	mut entries := {
		'a': [1, 2]!
	}
	mut replacement := [3, 4]!
	mut reference := FixedEntryMapRef(&entries)
	for _, mut value in reference {
		// The replacement array remains live throughout the borrowed iteration.
		unsafe {
			value = &replacement
			assert voidptr(value) == voidptr(&replacement)
			value[1] = 9
		}
	}
	assert entries['a'] == [1, 2]!
	assert replacement == [3, 9]!

	mut nested := NestedFixedEntryMapRef(reference)
	for _, mut value in nested {
		// The replacement array remains live throughout the borrowed iteration.
		unsafe {
			value = &replacement
			assert voidptr(value) == voidptr(&replacement)
			value[1] = 11
		}
	}
	assert entries['a'] == [1, 2]!
	assert replacement == [3, 11]!
	// vfmt off
	for _, mut value in ((reference)) {
		// The replacement array remains live throughout the borrowed iteration.
		unsafe {
			value = &replacement
			assert voidptr(value) == voidptr(&replacement)
			value[1] = 13
		}
	}
	// vfmt on
	assert entries['a'] == [1, 2]!
	assert replacement == [3, 13]!
}

fn test_value_alias_map_binding_still_updates_entry() {
	mut entries := FixedEntryMap({
		'a': [1, 2]!
	})
	for _, mut value in entries { value = [5, 6]! }
	assert entries['a'] == [5, 6]!
}

fn test_pointer_alias_array_binding_can_be_rebound() {
	mut rows := AliasRows([[1, 2]!])
	mut replacement := [3, 4]!
	mut reference := AliasRowsRef(&rows)
	for mut row in reference {
		// The replacement remains live throughout the borrowed iteration.
		unsafe {
			row = &replacement
			assert voidptr(row) == voidptr(&replacement)
			row[1] = 9
		}
	}
	assert rows[0] == [1, 2]!
	assert replacement == [3, 9]!
	mut nested := NestedAliasRowsRef(reference)
	for mut row in nested {
		// The replacement remains live throughout the borrowed iteration.
		unsafe {
			row = &replacement
			assert voidptr(row) == voidptr(&replacement)
			row[1] = 11
		}
	}
	assert rows[0] == [1, 2]!
	assert replacement == [3, 11]!
}

fn test_pointer_alias_fixed_array_binding_can_be_rebound() {
	mut rows := AliasFixedRows([[1, 2]!]!)
	mut replacement := [3, 4]!
	// Both arrays remain live throughout the borrowed iteration.
	mut reference := unsafe { AliasFixedRowsRef(&rows) }
	for mut row in reference {
		unsafe {
			row = &replacement
			assert voidptr(row) == voidptr(&replacement)
			row[1] = 9
		}
	}
	assert rows[0] == [1, 2]!
	assert replacement == [3, 9]!
}

fn update_mutable_map_alias(mut entries FixedEntryMapRef) int {
	for _, mut value in entries {
		value = [7, 8]!
	}
	// vfmt off
	for _, mut value in ((entries)) {
		value = [11, 12]!
	}
	// vfmt on
	mut sum := 0
	for _, value in entries {
		sum += value[0] + value[1]
	}
	return sum
}

fn update_mutable_array_alias(mut rows NestedAliasRowsRef) int {
	// vfmt off
	for mut row in ((rows)) {
		row = [5, 6]!
	}
	// vfmt on
	mut sum := 0
	for row in rows {
		sum += row[0] + row[1]
	}
	return sum
}

fn update_mutable_fixed_array_alias(mut rows AliasFixedRowsRef) int {
	for mut row in rows {
		row = [9, 10]!
	}
	mut sum := 0
	for row in rows {
		sum += row[0] + row[1]
	}
	return sum
}

fn test_mutable_pointer_alias_parameters_iterate_container_values() {
	mut entries := {
		'a': [1, 2]!
	}
	mut entry_ref := FixedEntryMapRef(&entries)
	assert update_mutable_map_alias(mut entry_ref) == 23
	assert entries['a'] == [11, 12]!
	mut rows := AliasRows([[1, 2]!])
	mut row_ref := NestedAliasRowsRef(&rows)
	assert update_mutable_array_alias(mut row_ref) == 11
	assert rows[0] == [5, 6]!
	mut fixed_rows := AliasFixedRows([[1, 2]!]!)
	// The array remains live throughout the mutable parameter call.
	mut fixed_ref := unsafe { AliasFixedRowsRef(&fixed_rows) }
	assert update_mutable_fixed_array_alias(mut fixed_ref) == 19
	assert fixed_rows[0] == [9, 10]!
}
