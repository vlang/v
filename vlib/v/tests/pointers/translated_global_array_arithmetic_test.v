@[has_globals; translated]
module main

type GlobalDecayOffset = int

__global decay_values = [3, 5, 7]!
__global decay_aliased_offset_second = GlobalDecayOffset(1) + decay_values
__global decay_second = decay_values + 1
__global decay_third = 2 + decay_values
__global decay_offset = 1
__global decay_offset_second = decay_offset + decay_values

struct GlobalDecayHolder {
	values [3]int
}

__global decay_holder = GlobalDecayHolder{ values: [11, 13, 17]! }
__global decay_field_second = decay_holder.values + 1
__global decay_rows = [[19, 23, 29]!, [31, 37, 41]!]!
__global decay_row_second = decay_rows[1] + 1
__global decay_call_second = decay_values + global_decay_offset()
__global decay_left_call_second = global_decay_offset() + decay_values
__global decay_selected_row = 0
__global decay_ordered_second = global_decay_select_second() + decay_rows[decay_selected_row]
__global decay_value_calls = 0
__global decay_returned_second = make_global_decay_values(43) + 1
__global decay_returned_third = 2 + make_global_decay_values(47)
__global decay_literal_second = [53, 59, 61]! + 1
__global decay_returned_row = make_global_decay_rows() + 1
__global decay_if_second = if decay_offset == 1 {
	1 + make_global_decay_values(103)
} else {
	2 + make_global_decay_values(103)
}
__global decay_if_third = if decay_offset == 2 {
	1 + make_global_decay_values(107)
} else {
	2 + make_global_decay_values(107)
}
__global decay_match_third = match decay_offset {
	1 { 2 + make_global_decay_values(113) }
	else { 1 + make_global_decay_values(113) }
}
__global decay_block_second = unsafe {
	if decay_offset == 1 {
		1 + make_global_decay_values(127)
	} else {
		2 + make_global_decay_values(127)
	}
}

@[aligned: 512]
struct GlobalDecayAligned {
	value int
}

__global decay_returned_aligned = make_global_decay_aligned() + 1
__global decay_saved = unsafe { &int(nil) }

fn make_global_decay_values(start int) [3]int {
	decay_value_calls++
	return [start, start + 1, start + 2]!
}

fn make_global_decay_rows() [2][3]int {
	return [[67, 71, 73]!, [79, 83, 89]!]!
}

fn make_global_decay_aligned() [2]GlobalDecayAligned {
	return [GlobalDecayAligned{ value: 97 }, GlobalDecayAligned{ value: 101 }]!
}

fn global_decay_offset() int {
	return 1
}

fn global_decay_select_second() int {
	decay_selected_row = 1
	return 1
}

fn global_decay_read(value &int) int {
	return unsafe { *value }
}

fn global_decay_read_row(value &[3]int) int {
	return unsafe { value[0] }
}

fn test_translated_global_array_arithmetic() {
	assert global_decay_read(decay_aliased_offset_second) == 5
	assert global_decay_read(decay_second) == 5
	assert global_decay_read(decay_third) == 7
	assert global_decay_read(decay_offset_second) == 5
	assert global_decay_read(decay_field_second) == 13
	assert global_decay_read(decay_row_second) == 37
	assert global_decay_read(decay_call_second) == 5
	assert global_decay_read(decay_left_call_second) == 5
	assert decay_selected_row == 1
	assert global_decay_read(decay_ordered_second) == 37
	assert decay_value_calls == 6
	assert global_decay_read(decay_returned_second) == 44
	assert global_decay_read(decay_returned_third) == 49
	assert global_decay_read(decay_literal_second) == 59
	assert global_decay_read_row(decay_returned_row) == 79
	assert u64(voidptr(decay_returned_aligned)) % 512 == 0
	assert decay_returned_aligned.value == 101
	assert global_decay_read(decay_if_second) == 104
	assert global_decay_read(decay_if_third) == 109
	assert global_decay_read(decay_match_third) == 115
	assert global_decay_read(decay_block_second) == 128
}

fn save_global_decay_pointer() {
	mut values := [131, 137, 139]!
	decay_saved = values + 1
	values[1] = 149
}

fn save_global_decay_alias() {
	mut values := [151, 157, 163]!
	pointer := values + 1
	decay_saved = pointer
	values[1] = 167
}

fn save_mut_param_decay_pointer(mut pointer &int) {
	mut values := [173, 179, 181]!
	pointer = values + 1
	values[1] = 191
}

fn save_indirect_decay_pointer(pointer &&int) {
	mut values := [193, 197, 199]!
	unsafe { *pointer = values + 1 }
	values[1] = 211
}

fn test_translated_array_pointer_nonlocal_stores_keep_storage() {
	save_global_decay_pointer()
	assert global_decay_read(decay_saved) == 149
	save_global_decay_alias()
	assert global_decay_read(decay_saved) == 167
	mut pointer := unsafe { &int(nil) }
	save_mut_param_decay_pointer(mut pointer)
	assert global_decay_read(pointer) == 191
	save_indirect_decay_pointer(&pointer)
	assert global_decay_read(pointer) == 211
}
