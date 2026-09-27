@[has_globals]
module main

struct GlobalArrayPointerCell {
mut:
	value int = 23
}

__global global_zero_array = &[4]int{}
__global global_literal_array = &[3, 5]!
__global global_cell_array = &[2]GlobalArrayPointerCell{}
__global global_nested_array = &[2][3]int{}

fn test_global_fixed_array_pointers_are_initialized() {
	assert unsafe { voidptr(global_zero_array) } != unsafe { nil }
	assert unsafe { voidptr(global_literal_array) } != unsafe { nil }
	assert unsafe { voidptr(global_cell_array) } != unsafe { nil }
	assert unsafe { voidptr(global_nested_array) } != unsafe { nil }
	// Indexing these pointer-backed arrays requires an unsafe block.
	unsafe {
		assert global_zero_array[0] == 0
		assert global_zero_array[3] == 0
		global_zero_array[3] = 42
		assert global_zero_array[3] == 42
		assert global_literal_array[0] == 3
		assert global_literal_array[1] == 5
		assert global_cell_array[0].value == 23
		assert global_cell_array[1].value == 23
		global_cell_array[0].value = 41
		assert global_cell_array[0].value == 41
		assert global_cell_array[1].value == 23
		assert global_nested_array[1][2] == 0
		global_nested_array[1][2] = 17
		assert global_nested_array[1][2] == 17
	}
}
