module main

struct NestedFixedCallRows {
	rows [2][4]int
}

fn make_nested_fixed_call_row(base int) [4]int {
	return [base, base + 1, base + 2, base + 3]!
}

fn test_nested_fixed_array_call_copy_lifetime() {
	rows := [make_nested_fixed_call_row(1), make_nested_fixed_call_row(9)]!
	assert rows[0] == [1, 2, 3, 4]!
	assert rows[1] == [9, 10, 11, 12]!

	wrapped := NestedFixedCallRows{
		rows: [make_nested_fixed_call_row(17), make_nested_fixed_call_row(25)]!
	}
	assert wrapped.rows[0] == [17, 18, 19, 20]!
	assert wrapped.rows[1] == [25, 26, 27, 28]!
}
