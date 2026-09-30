#include <stdlib.h>

@[heap; typedef]
struct C.div_t {
mut:
	quot i32
	rem  i32
}

type HeapDivision = C.div_t

type NestedHeapDivision = HeapDivision

fn heap_c_alias_result(value i32) !HeapDivision {
	mut result := HeapDivision{}
	result.quot = value
	result.rem = 7
	return result
}

fn heap_c_alias_option(value i32) ?HeapDivision {
	mut result := HeapDivision{}
	result.quot = value
	result.rem = 7
	return result
}

fn deferred_heap_c_alias_result(value i32, mut seen []i32) !NestedHeapDivision {
	defer { seen << value }
	mut result := NestedHeapDivision{}
	result.quot = value
	result.rem = 7
	return result
}

fn test_heap_c_alias_returns_preserve_struct_values() {
	mut seen := []i32{}
	for value in [i32(0), 42, -7] {
		result := heap_c_alias_result(value) or { panic(err) }
		assert result.quot == value
		assert result.rem == 7
		optional := heap_c_alias_option(value) or { panic('unexpected none') }
		assert optional.quot == value
		assert optional.rem == 7
		deferred := deferred_heap_c_alias_result(value, mut seen) or { panic(err) }
		assert deferred.quot == value
		assert deferred.rem == 7
	}
	assert seen == [i32(0), 42, -7]
}
