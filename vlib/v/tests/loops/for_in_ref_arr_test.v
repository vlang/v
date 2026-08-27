fn test_for_in_ref_val_ref_arr() {
	arr := [1, 2, 3, 4, 5]
	mut rets := []&int{}
	mut expects := unsafe { []&int{len: 5, init: &arr[index]} }

	for val in &arr {
		println(val)
		rets << val
	}

	for i, val in &arr {
		assert voidptr(val) == voidptr(rets[i])
	}
	assert rets == expects
}

fn test_for_in_ref_val_ref_arr_ident() {
	arr_ := [1, 2, 3, 4, 5]
	arr := &arr_
	mut rets := []&int{}
	mut expects := unsafe { []&int{len: 5, init: &arr_[index]} }

	for val in arr {
		rets << val
	}

	for i, val in arr {
		assert voidptr(val) == voidptr(rets[i])
	}
	assert rets == expects
}

struct ForInRefPointerItem {
	value int
}

fn test_for_in_ref_fixed_array_of_pointers() {
	first := &ForInRefPointerItem{
		value: 10
	}
	second := &ForInRefPointerItem{
		value: 20
	}
	items := [first, second]!

	// Referencing a stack-allocated fixed array requires an explicit unsafe scope.
	unsafe {
		for i, item in &items {
			assert voidptr(item) == voidptr(items[i])
			assert item.value == (i + 1) * 10
		}
	}
}

fn test_for_in_ref_fixed_array_of_optional_pointers() {
	first := &ForInRefPointerItem{
		value: 10
	}
	second := &ForInRefPointerItem{
		value: 20
	}
	mut items := unsafe { [2]?&ForInRefPointerItem{} }
	items[0] = first
	items[1] = second
	mut values := []int{}

	// Referencing a stack-allocated fixed array requires an explicit unsafe scope.
	unsafe {
		for optional_item in &items {
			item := optional_item or { continue }
			values << item.value
		}
	}
	assert values == [10, 20]
}

fn for_in_ref_first_callback() int {
	return 10
}

fn for_in_ref_second_callback() int {
	return 20
}

type ForInRefCallback = fn () int

fn test_for_in_ref_fixed_array_of_functions() {
	callbacks := [for_in_ref_first_callback, for_in_ref_second_callback]!
	mut results := []int{}

	// Referencing a stack-allocated fixed array requires an explicit unsafe scope.
	unsafe {
		for callback in &callbacks {
			direct_callback := *callback
			results << direct_callback()
		}
	}
	assert results == [10, 20]
}

fn test_for_in_mut_fixed_array_of_function_aliases() {
	mut callbacks := [ForInRefCallback(for_in_ref_first_callback),
		ForInRefCallback(for_in_ref_second_callback)]!
	for mut callback in callbacks {
		callback = ForInRefCallback(for_in_ref_second_callback)
	}
	assert callbacks[0]() == 20
	assert callbacks[1]() == 20
}
