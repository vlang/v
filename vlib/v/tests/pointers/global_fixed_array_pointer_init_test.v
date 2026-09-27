@[has_globals]
module main

struct GlobalArrayPointerCell {
mut:
	value int = 23
}

@[aligned: 512]
struct GlobalAlignedArrayPointerCell {
	value int = 19
}

__global global_zero_array = &[4]int{}
__global global_literal_array = &[3, 5]!
__global global_cell_array = &[2]GlobalArrayPointerCell{}
__global global_nested_array = &[2][3]int{}
__global global_filled_array = &[4]int{init: 7}
__global global_index_array = &[4]int{init: index * 2}
__global global_aligned_array = &[2]GlobalAlignedArrayPointerCell{}
__global global_nested_aligned_array = &[2][2]GlobalAlignedArrayPointerCell{}
__global global_filled_aligned_array = &[2]GlobalAlignedArrayPointerCell{init: GlobalAlignedArrayPointerCell{
	value: index + 5
}}

fn test_global_fixed_array_pointers_are_initialized() {
	assert unsafe { voidptr(global_zero_array) } != unsafe { nil }
	assert unsafe { voidptr(global_literal_array) } != unsafe { nil }
	assert unsafe { voidptr(global_cell_array) } != unsafe { nil }
	assert unsafe { voidptr(global_nested_array) } != unsafe { nil }
	assert unsafe { voidptr(global_filled_array) } != unsafe { nil }
	assert unsafe { voidptr(global_index_array) } != unsafe { nil }
	assert u64(voidptr(global_aligned_array)) % 512 == 0
	assert u64(voidptr(global_nested_aligned_array)) % 512 == 0
	assert u64(voidptr(global_filled_aligned_array)) % 512 == 0
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
		assert global_filled_array[0] == 7
		assert global_filled_array[3] == 7
		assert global_index_array[0] == 0
		assert global_index_array[3] == 6
		assert global_aligned_array[1].value == 19
		assert global_nested_aligned_array[1][1].value == 19
		assert global_filled_aligned_array[0].value == 5
		assert global_filled_aligned_array[1].value == 6
	}
}
