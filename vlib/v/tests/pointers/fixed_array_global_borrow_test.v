@[has_globals]
module main

__global borrowed_global_values = [3, 5]!

fn borrowed_first(value &int) int {
	return unsafe { *value }
}

fn test_global_fixed_array_reference() {
	pointer := &borrowed_global_values[0]
	assert *pointer == 3
}

fn test_parenthesized_fixed_array_borrow() {
	values := [3, 5]!
	assert borrowed_first((&values[0])) == 3
}

fn test_indexed_pointer_cast_address() {
	value := 42
	pointer := unsafe { &(&int(voidptr(&value)))[0] }
	assert *pointer == 42
}
