@[has_globals; translated]
module main

__global decay_values = [3, 5, 7]!
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

fn test_translated_global_array_arithmetic() {
	assert global_decay_read(decay_second) == 5
	assert global_decay_read(decay_third) == 7
	assert global_decay_read(decay_offset_second) == 5
	assert global_decay_read(decay_field_second) == 13
	assert global_decay_read(decay_row_second) == 37
	assert global_decay_read(decay_call_second) == 5
	assert global_decay_read(decay_left_call_second) == 5
	assert decay_selected_row == 1
	assert global_decay_read(decay_ordered_second) == 37
}
