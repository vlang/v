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

struct GlobalInheritedAlignedArrayPointerCell {
	inner GlobalAlignedArrayPointerCell
}

type GlobalArrayPointerAddress = [32]u8
type GlobalArrayPointerRow = [4]int
type GlobalArrayPointerValues = GlobalArrayPointerRow
type GlobalArrayPointerNestedRow = [3]int
type GlobalArrayPointerNestedRowAlias = GlobalArrayPointerNestedRow

__global global_row_calls = 0

fn make_global_array_pointer_row(index int) [3]int {
	global_row_calls++
	return [index * 10, index * 10 + 1, index * 10 + 2]!
}

fn free_global_aligned_array_pointer(value &[2]GlobalAlignedArrayPointerCell) {
	unsafe { free(value) }
}

fn free_global_optional_aligned_array_pointer(value &[2]?GlobalAlignedArrayPointerCell) {
	unsafe { free(value) }
}

__global global_zero_array = &[4]int{}
__global global_unsafe_array = unsafe { &[4]int{} }
__global global_parenthesized_array = &([4]int{})
__global global_inner_unsafe_array = &(unsafe { [4]int{} })
__global global_inner_unsafe_filled_array = &(unsafe { [2]int{init: index * 2 + 3} })
__global global_inner_unsafe_call_array = &(unsafe {
	[2][3]int{init: make_global_array_pointer_row(index + 4)}
})
__global global_literal_array = &[3, 5]!
__global global_cell_array = &[2]GlobalArrayPointerCell{}
__global global_nested_array = &[2][3]int{}
__global global_alias_array = &GlobalArrayPointerAddress{}
__global global_chained_alias_array = &GlobalArrayPointerValues{}
__global global_filled_array = &[4]int{init: 7}
__global global_index_array = &[4]int{init: index * 2}
__global global_nested_filled_array = &[2][3]int{init: [3]int{init: 7}}
__global global_nested_index_array = &[2][3]int{init: [3]int{init: index + 5}}
__global global_nested_call_array = &[2][3]int{init: make_global_array_pointer_row(index)}
__global global_nested_alias_call_array = &[2]GlobalArrayPointerNestedRowAlias{init: make_global_array_pointer_row(index)}
__global global_nested_literal_call_array = &[make_global_array_pointer_row(0),
	make_global_array_pointer_row(1)]!
__global global_deep_literal_call_array = &[
	[make_global_array_pointer_row(0), make_global_array_pointer_row(1)]!,
	[make_global_array_pointer_row(2), make_global_array_pointer_row(3)]!,
]!
__global global_aligned_array = &[2]GlobalAlignedArrayPointerCell{}
__global global_optional_aligned_array = &[2]?GlobalAlignedArrayPointerCell{}
__global global_nested_aligned_array = &[2][2]GlobalAlignedArrayPointerCell{}
__global global_inherited_aligned_array = &[2]GlobalInheritedAlignedArrayPointerCell{}
__global global_filled_aligned_array = &[2]GlobalAlignedArrayPointerCell{init: GlobalAlignedArrayPointerCell{
	value: index + 5
}}

fn test_global_fixed_array_pointers_are_initialized() {
	assert unsafe { voidptr(global_zero_array) } != unsafe { nil }
	assert unsafe { voidptr(global_unsafe_array) } != unsafe { nil }
	assert unsafe { voidptr(global_parenthesized_array) } != unsafe { nil }
	assert unsafe { voidptr(global_inner_unsafe_array) } != unsafe { nil }
	assert unsafe { voidptr(global_inner_unsafe_filled_array) } != unsafe { nil }
	assert unsafe { voidptr(global_inner_unsafe_call_array) } != unsafe { nil }
	assert unsafe { voidptr(global_literal_array) } != unsafe { nil }
	assert unsafe { voidptr(global_cell_array) } != unsafe { nil }
	assert unsafe { voidptr(global_nested_array) } != unsafe { nil }
	assert unsafe { voidptr(global_alias_array) } != unsafe { nil }
	assert unsafe { voidptr(global_chained_alias_array) } != unsafe { nil }
	assert unsafe { voidptr(global_filled_array) } != unsafe { nil }
	assert unsafe { voidptr(global_index_array) } != unsafe { nil }
	assert u64(voidptr(global_aligned_array)) % 512 == 0
	assert u64(voidptr(global_optional_aligned_array)) % 512 == 0
	assert u64(voidptr(global_nested_aligned_array)) % 512 == 0
	assert u64(voidptr(global_inherited_aligned_array)) % 512 == 0
	assert u64(voidptr(global_filled_aligned_array)) % 512 == 0
	assert global_row_calls == 12
	// Indexing these pointer-backed arrays requires an unsafe block.
	unsafe {
		assert global_zero_array[0] == 0
		assert global_zero_array[3] == 0
		global_zero_array[3] = 42
		assert global_zero_array[3] == 42
		global_unsafe_array[3] = 43
		assert global_unsafe_array[3] == 43
		assert global_parenthesized_array[0] == 0
		assert global_parenthesized_array[3] == 0
		assert global_inner_unsafe_array[0] == 0
		global_inner_unsafe_array[3] = 44
		assert global_inner_unsafe_array[3] == 44
		assert global_inner_unsafe_filled_array[0] == 3
		assert global_inner_unsafe_filled_array[1] == 5
		assert global_inner_unsafe_call_array[0][2] == 42
		assert global_inner_unsafe_call_array[1][2] == 52
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
		assert global_alias_array[0] == 0
		assert global_alias_array[31] == 0
		assert global_chained_alias_array[0][0] == 0
		assert global_chained_alias_array[0][3] == 0
		assert global_filled_array[0] == 7
		assert global_filled_array[3] == 7
		assert global_index_array[0] == 0
		assert global_index_array[3] == 6
		assert global_nested_filled_array[0][0] == 7
		assert global_nested_filled_array[1][2] == 7
		assert global_nested_index_array[0][0] == 5
		assert global_nested_index_array[1][2] == 7
		assert global_nested_call_array[0][0] == 0
		assert global_nested_call_array[0][2] == 2
		assert global_nested_call_array[1][0] == 10
		assert global_nested_call_array[1][2] == 12
		assert global_nested_alias_call_array[0][1] == 1
		assert global_nested_alias_call_array[1][2] == 12
		assert global_nested_literal_call_array[0][1] == 1
		assert global_nested_literal_call_array[1][2] == 12
		assert global_deep_literal_call_array[0][1][2] == 12
		assert global_deep_literal_call_array[1][1][2] == 32
		assert global_aligned_array[1].value == 19
		assert global_nested_aligned_array[1][1].value == 19
		assert global_inherited_aligned_array[1].inner.value == 19
		assert global_filled_aligned_array[0].value == 5
		assert global_filled_aligned_array[1].value == 6
	}
	free_global_aligned_array_pointer(global_aligned_array)
	free_global_optional_aligned_array_pointer(global_optional_aligned_array)
}
