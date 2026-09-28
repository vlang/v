@[translated]
module main

fn read_translated_element(value &int) int {
	return unsafe { *value }
}

fn test_translated_fixed_array_pointer_arithmetic() {
	values := [3, 5, 7]!
	start := values + 0
	next := values + 1
	assert unsafe { *next } == 5
	assert next - start == 1
	assert next - values == 1
	assert values - next == -1
	assert (1 + values) - values == 1
	assert read_translated_element(values + 2) == 7
	assert read_translated_element(next - 1) == 3
}

fn translated_array_offset() !int {
	return 1
}

fn translated_branch_array_offset(flag bool) !int {
	values := [3, 5, 7]!
	branch_values := (if flag { [3, 5, 7]! } else { [11, 13, 17]! }) + 1
	if_offset := values + (if flag { translated_array_offset()! } else { 2 })
	match_offset := values + (match flag {
		true { translated_array_offset()! }
		else { 2 }
	})
	assert unsafe { *if_offset } == unsafe { *match_offset }
	assert unsafe { *branch_values } == if flag { 5 } else { 13 }
	return unsafe { *if_offset }
}

fn test_translated_fixed_array_branch_offsets() {
	assert translated_branch_array_offset(true)! == 5
	assert translated_branch_array_offset(false)! == 7
}
