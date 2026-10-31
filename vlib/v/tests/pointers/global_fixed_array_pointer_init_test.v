@[has_globals]
module main

struct GlobalArrayPointerCell {
mut:
	value int = 23
}

struct GlobalArrayPointerHolder {
mut:
	rows [2]GlobalArrayPointerNestedRowAlias
}

struct GlobalArrayPointerIndirectHolder {
	inner &GlobalArrayPointerHolder
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
type GlobalArrayPointerMatrix = [2]GlobalArrayPointerNestedRowAlias
type GlobalArrayPointerAlignedCells = [2]GlobalAlignedArrayPointerCell

__global global_row_calls = 0

fn make_global_array_pointer_row(index int) [3]int {
	global_row_calls++
	return [index * 10, index * 10 + 1, index * 10 + 2]!
}

fn make_global_array_pointer_aliased_row(index int) GlobalArrayPointerNestedRow {
	global_row_calls++
	return [index * 10, index * 10 + 1, index * 10 + 2]!
}

fn free_global_aligned_array_pointer(value &[2]GlobalAlignedArrayPointerCell) {
	unsafe { free(value) }
}

fn free_global_optional_aligned_array_pointer(value &[2]?GlobalAlignedArrayPointerCell) {
	unsafe { free(value) }
}

__global global_wrapper_calls = 0

fn global_array_wrapper_value(value int) int {
	global_wrapper_calls++
	return value
}

__global global_shared_array = [2]int{}
__global global_shared_array_reference = unsafe {
	global_array_wrapper_value(0)
	&global_shared_array
}

__global global_statement_array = unsafe {
	global_array_wrapper_value(0)
	&[4]int{}
}
__global global_inner_statement_array = &(unsafe {
	value := global_array_wrapper_value(13)
	[4]int{init: value + index}
})
__global global_nested_statement_array = unsafe {
	value := global_array_wrapper_value(20)
	offset := global_array_wrapper_value(3)
	&([value + offset, value + offset + 1]!)
}
__global global_local_index_array = unsafe {
	mut grid := [2]GlobalArrayPointerNestedRowAlias{}
	grid[1][0] = 37
	&grid[1]
}
__global global_local_selector_array = unsafe {
	mut holder := GlobalArrayPointerHolder{}
	holder.rows[1][0] = 41
	&holder.rows[1]
}
__global global_pointer_backing = GlobalArrayPointerHolder{}
__global global_pointer_backed_reference = unsafe {
	holder := GlobalArrayPointerIndirectHolder{ inner: &global_pointer_backing }
	&holder.inner.rows[1]
}
__global global_statement_aligned_array = unsafe {
	value := global_array_wrapper_value(30)
	&[2]GlobalAlignedArrayPointerCell{init: GlobalAlignedArrayPointerCell{ value: value + index }}
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
__global global_filled_alias_array = &GlobalArrayPointerRow{ init: 7 }
__global global_filled_chained_alias_array = &GlobalArrayPointerValues{ init: 8 }
__global global_nested_filled_alias_array = &GlobalArrayPointerMatrix{
	init: GlobalArrayPointerNestedRowAlias{
		init: 11
	}
}
__global global_alias_call_array = &GlobalArrayPointerMatrix{ init: make_global_array_pointer_row(4) }
__global global_filled_aligned_alias_array = &GlobalArrayPointerAlignedCells{
	init: GlobalAlignedArrayPointerCell{
		value: 17
	}
}
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
__global global_literal_aliased_rows = &[make_global_array_pointer_aliased_row(2),
	make_global_array_pointer_aliased_row(3)]!
__global global_deep_literal_aliased_rows = &[
	[make_global_array_pointer_aliased_row(4), make_global_array_pointer_aliased_row(5)]!,
	[make_global_array_pointer_aliased_row(6), make_global_array_pointer_aliased_row(7)]!,
]!
__global global_array_pick = 1
__global global_if_zero_array = if global_array_pick == 1 {
	&[4]int{}
} else {
	&[4]int{init: 7}
}
__global global_if_filled_array = if global_array_pick == 0 {
	&[4]int{}
} else {
	&[4]int{init: 9}
}
__global global_match_array = match global_array_pick {
	0 { &[4]int{} }
	1 { &GlobalArrayPointerRow{ init: 11 } }
	else { &[4]int{init: 13} }
}
__global global_conditional_shared_array = if global_array_pick == 1 {
	&global_shared_array
} else {
	&[2]int{init: 15}
}
__global global_if_aligned_array = if global_array_pick == 1 {
	&[2]GlobalAlignedArrayPointerCell{init: GlobalAlignedArrayPointerCell{ value: 17 }}
} else {
	&[2]GlobalAlignedArrayPointerCell{}
}

fn global_array_guard_value(success bool) ?int {
	if !success {
		return none
	}
	return 21
}

__global global_guard_array = if value := global_array_guard_value(true) {
	&[4]int{init: value + index}
} else {
	&[4]int{}
}
__global global_failed_guard_array = if value := global_array_guard_value(false) {
	&[4]int{init: value}
} else {
	&[4]int{init: 25}
}
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
	assert u64(voidptr(global_filled_aligned_alias_array)) % 512 == 0
	assert global_row_calls == 20
	assert u64(voidptr(global_if_aligned_array)) % 512 == 0
	assert voidptr(global_conditional_shared_array) == voidptr(&global_shared_array)
	// Indexing these pointer-backed arrays requires an unsafe block.
	unsafe {
		assert global_guard_array[3] == 24
		assert global_failed_guard_array[3] == 25
		assert global_if_zero_array[3] == 0
		assert global_if_filled_array[3] == 9
		assert global_match_array[3] == 11
		assert global_if_aligned_array[1].value == 17
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
		assert global_filled_alias_array[0] == 7
		assert global_filled_alias_array[3] == 7
		assert global_filled_array[0] == 7
		assert global_filled_array[3] == 7
		assert global_filled_chained_alias_array[0][0] == 8
		assert global_filled_chained_alias_array[0][3] == 8
		global_chained_alias_array[0][3] = 29
		global_chained_alias_array[0][3] += 2
		assert global_chained_alias_array[0][3] == 31
		global_filled_chained_alias_array[0][0] = 35
		global_filled_chained_alias_array[0][0] += 2
		assert global_filled_chained_alias_array[0][0] == 37
		assert global_nested_filled_alias_array[0][0] == 11
		assert global_nested_filled_alias_array[1][2] == 11
		assert global_alias_call_array[0][0] == 40
		assert global_alias_call_array[1][2] == 42
		assert global_filled_aligned_alias_array[0].value == 17
		assert global_filled_aligned_alias_array[1].value == 17
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
		assert global_literal_aliased_rows[0][1] == 21
		assert global_literal_aliased_rows[1][2] == 32
		assert global_deep_literal_aliased_rows[0][1][2] == 52
		assert global_deep_literal_aliased_rows[1][1][2] == 72
		assert global_aligned_array[1].value == 19
		assert global_nested_aligned_array[1][1].value == 19
		assert global_inherited_aligned_array[1].inner.value == 19
		assert global_filled_aligned_array[0].value == 5
		assert global_filled_aligned_array[1].value == 6
	}
	free_global_aligned_array_pointer(global_aligned_array)
	free_global_optional_aligned_array_pointer(global_optional_aligned_array)
}

fn test_global_fixed_array_pointer_unsafe_statements_keep_storage_and_scope() {
	assert global_wrapper_calls == 6
	assert voidptr(global_shared_array_reference) == voidptr(&global_shared_array)
	assert u64(voidptr(global_statement_aligned_array)) % 512 == 0
	unsafe {
		assert global_statement_array[3] == 0
		assert global_inner_statement_array[0] == 13
		assert global_inner_statement_array[3] == 16
		assert global_nested_statement_array[0] == 23
		assert global_nested_statement_array[1] == 24
		assert global_local_index_array[0] == 37
		assert global_local_selector_array[0] == 41
		global_pointer_backing.rows[1][0] = 53
		assert global_pointer_backed_reference[0] == 53
		assert voidptr(global_pointer_backed_reference) == voidptr(&global_pointer_backing.rows[1])
		assert global_statement_aligned_array[1].value == 31
		global_statement_array[3] = 45
		assert global_statement_array[3] == 45
		// All addressed literals own storage beyond the initializer's stack frame.
		free(global_statement_array)
		free(global_inner_statement_array)
		free(global_nested_statement_array)
		free(global_local_index_array)
		free(global_local_selector_array)
	}
	free_global_aligned_array_pointer(global_statement_aligned_array)
}
