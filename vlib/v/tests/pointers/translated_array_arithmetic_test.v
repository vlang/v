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
	second := 1 + right
	assert unsafe { *second } == 5
	third := 2 + translated_make_vec()
	assert unsafe { *third } == 13
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

fn translated_make_vec() TranslatedVec3 {
	return TranslatedVec3([7, 11, 13]!)
}

fn translated_ordered_index(mut calls []int) int {
	calls << 1
	return 0
}

fn translated_rhs_offset(mut calls []int) int {
	calls << 2
	return 1
}

fn translated_ordered_pointer(value &[3]int, mut calls []int) &[3]int {
	calls << 1
	return value
}

fn test_translated_array_lvalue_addresses_are_evaluated_before_offsets() {
	mut calls := []int{}
	mut arrays := [[3, 5, 7]!, [11, 13, 17]!]!
	indexed := arrays[translated_ordered_index(mut calls)] + translated_rhs_offset(mut calls)
	assert calls == [1, 2]
	assert unsafe { *indexed } == 5
	unsafe { *indexed = 19 }
	assert arrays[0][1] == 19
	calls.clear()
	dereferenced := (*translated_ordered_pointer(&arrays[1], mut calls)) + translated_rhs_offset(mut calls)
	assert calls == [1, 2]
	assert unsafe { *dereferenced } == 13
	unsafe { *dereferenced = 23 }
	assert arrays[1][1] == 23
	calls.clear()
	propagated := arrays[translated_ordered_index(mut calls)] + translated_result_rhs_offset(mut calls)!
	assert calls == [1, 2]
	assert unsafe { *propagated } == 19
}

fn translated_result_rhs_offset(mut calls []int) !int {
	return translated_rhs_offset(mut calls)
}

const translated_rows_n = 2
const translated_rows_m = 1 + 1

fn translated_named_rows_difference(left [2][translated_rows_n]int, right [2][translated_rows_m]int) int {
	return left - right
}

fn test_translated_array_subtraction_compares_evaluated_lengths() {
	rows := [[3, 5]!, [7, 11]!]!
	assert translated_named_rows_difference(rows, rows) == 0
}

struct TranslatedArrayHolder {
mut:
	values [3]int
}

type TranslatedArrayHolderPtr = &TranslatedArrayHolder

fn translated_ordered_holder(value &TranslatedArrayHolder, mut calls []int) TranslatedArrayHolderPtr {
	calls << 1
	return value
}

fn test_translated_pointer_backed_array_selectors_keep_original_storage() {
	mut holder := TranslatedArrayHolder{ values: [3, 5, 7]! }
	mut calls := []int{}
	selected := translated_ordered_holder(&holder, mut calls).values + translated_result_rhs_offset(mut calls)!
	assert calls == [1, 2]
	assert unsafe { *selected } == 5
	unsafe { *selected = 19 }
	assert holder.values[1] == 19
	calls.clear()
	mut rows := [[3, 5, 7]!, [11, 13, 17]!]!
	indexed := translated_ordered_rows(&rows, mut calls)[0] + translated_result_rhs_offset(mut calls)!
	assert calls == [1, 2]
	unsafe { *indexed = 23 }
	assert rows[0][1] == 23
	calls.clear()
	reversed := translated_rhs_offset(mut calls) + translated_ordered_holder(&holder, mut calls).values
	assert calls == [2, 1]
	unsafe { *reversed = 29 }
	assert holder.values[1] == 29
}

fn translated_ordered_rows(value &[2][3]int, mut calls []int) &[2][3]int {
	calls << 1
	return value
}

fn test_translated_array_alias_with_unmatched_operator_still_decays() {
	mut values := TranslatedVec3([3, 5, 7]!)
	second := values + 1
	assert unsafe { *second } == 5
	unsafe { *second = 19 }
	assert values[1] == 19
	assert values - second == -1
	assert second - values == 1
	returned := translated_make_vec() + 1
	assert unsafe { *returned } == 11
	summed := values + TranslatedVec3([1, 2, 3]!)
	assert summed == TranslatedVec3([4, 21, 10]!)
}

fn translated_return_array_element() &int {
	return translated_make_array() + 1
}

fn translated_return_reverse_array_element() &int {
	return 2 + translated_make_array()
}

fn translated_return_branch_array_element(flag bool) &int {
	return (if flag { translated_make_array() } else { [17, 19, 23]! }) + 1
}

@[aligned: 512]
struct TranslatedEscapingAligned {
	value int
}

fn translated_make_aligned_values() [2]TranslatedEscapingAligned {
	return [TranslatedEscapingAligned{ value: 29 }, TranslatedEscapingAligned{ value: 31 }]!
}

fn translated_return_aligned_element() &TranslatedEscapingAligned {
	return translated_make_aligned_values() + 1
}

fn test_translated_array_arithmetic_storage_survives_escape() {
	second := translated_return_array_element()
	third := translated_return_reverse_array_element()
	assert read_translated_element(second) == 11
	assert read_translated_element(third) == 13
	assert read_translated_element(translated_return_branch_array_element(true)) == 11
	assert read_translated_element(translated_return_branch_array_element(false)) == 19
	mut outside := unsafe { &int(nil) }
	if second != unsafe { nil } {
		outside = translated_make_array() + 1
	}
	assert read_translated_element(outside) == 11
	aligned := translated_return_aligned_element()
	assert usize(voidptr(aligned)) % 512 == 0
	assert aligned.value == 31
}
