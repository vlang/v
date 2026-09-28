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

fn translated_make_array() [3]int {
	return [7, 11, 13]!
}

fn test_translated_returned_array_pointer_survives_assignment() {
	second := translated_make_array() + 1
	assert unsafe { *second } == 11
	third := 2 + translated_make_array()
	assert unsafe { *third } == 13
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

type TranslatedVec3 = [3]int

fn (left TranslatedVec3) + (right TranslatedVec3) TranslatedVec3 {
	return TranslatedVec3([left[0] + right[0], left[1] + right[1], left[2] + right[2]]!)
}

fn test_translated_fixed_array_operator_overload() {
	left := TranslatedVec3([1, 2, 3]!)
	right := TranslatedVec3([4, 5, 6]!)
	result := left + right
	assert result == TranslatedVec3([5, 7, 9]!)
}

fn translated_ordered_offset(mut calls []int) int {
	calls << 1
	return 1
}

fn translated_ordered_array(mut calls []int) [3]int {
	calls << 2
	return [7, 11, 13]!
}

struct TranslatedOrderState {
mut:
	offset int
}

fn translated_array_after_offset_change(mut state TranslatedOrderState) [3]int {
	state.offset = 2
	return [7, 11, 13]!
}

fn test_translated_array_arithmetic_preserves_operand_order() {
	mut calls := []int{}
	second := translated_ordered_offset(mut calls) + translated_ordered_array(mut calls)
	assert calls == [1, 2]
	assert unsafe { *second } == 11
	calls.clear()
	third := (translated_ordered_offset(mut calls) + 1) + translated_ordered_array(mut calls)
	assert calls == [1, 2]
	assert unsafe { *third } == 13
	calls.clear()
	reversed := translated_ordered_array(mut calls) + translated_ordered_offset(mut calls)
	assert calls == [2, 1]
	assert unsafe { *reversed } == 11
	mut state := TranslatedOrderState{}
	first := state.offset + translated_array_after_offset_change(mut state)
	assert state.offset == 2
	assert unsafe { *first } == 7
}
