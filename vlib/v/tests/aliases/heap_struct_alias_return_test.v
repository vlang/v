@[heap]
struct HeapReturnValue {
mut:
	value int
}

type HeapReturnAlias = HeapReturnValue

type NestedHeapReturnAlias = HeapReturnAlias

fn heap_alias_result(value int) !HeapReturnAlias {
	if value < 0 {
		return error_with_code('negative value', 37)
	}
	mut result := HeapReturnAlias{}
	result.value = value
	return result
}

fn heap_alias_option(value int) ?HeapReturnAlias {
	if value < 0 {
		return none
	}
	mut result := HeapReturnAlias{}
	result.value = value
	return result
}

fn nested_heap_alias_result(value int, mut seen []int) !NestedHeapReturnAlias {
	defer { seen << value }
	if value < 0 {
		return error_with_code('negative value', 37)
	}
	mut result := NestedHeapReturnAlias{}
	result.value = value
	return result
}

fn nested_heap_alias_option(value int, mut seen []int) ?NestedHeapReturnAlias {
	defer { seen << value }
	if value < 0 {
		return none
	}
	mut result := NestedHeapReturnAlias{}
	result.value = value
	return result
}

fn test_heap_alias_returns_preserve_values() {
	mut seen := []int{}
	for value in [0, 42] {
		result := heap_alias_result(value) or { panic(err) }
		assert result.value == value
		optional := heap_alias_option(value) or { panic('unexpected none') }
		assert optional.value == value
		deferred := nested_heap_alias_result(value, mut seen) or { panic(err) }
		assert deferred.value == value
		deferred_option := nested_heap_alias_option(value, mut seen) or { panic('unexpected none') }
		assert deferred_option.value == value
	}
	assert seen == [0, 0, 42, 42]
}

fn test_heap_alias_returns_preserve_errors_and_none() {
	mut seen := []int{}
	if _ := heap_alias_result(-1) {
		assert false, 'expected an error'
	} else {
		assert err.msg() == 'negative value'
		assert err.code() == 37
	}
	if _ := heap_alias_option(-1) {
		assert false, 'expected none'
	}
	if _ := nested_heap_alias_result(-1, mut seen) {
		assert false, 'expected an error'
	} else {
		assert err.msg() == 'negative value'
		assert err.code() == 37
	}
	if _ := nested_heap_alias_option(-1, mut seen) {
		assert false, 'expected none'
	}
	assert seen == [-1, -1]
}
