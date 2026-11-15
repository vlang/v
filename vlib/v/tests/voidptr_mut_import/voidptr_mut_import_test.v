import voidptr_mut_import.imported

struct RawPointerHolder {
mut:
	ptr voidptr
}

fn test_raw_pointer_mut_argument_updates_imported_struct() {
	mut item := imported.Item{
		value: 1
	}
	mut raw := voidptr(&item)
	imported.fill(mut raw)
	assert item.value == 42
	assert raw == voidptr(&item)
}

fn test_null_raw_pointer_reaches_imported_mut_parameter() {
	mut raw := unsafe { nil }
	assert imported.is_null(mut raw)
}

fn test_raw_pointer_field_mut_argument_updates_imported_struct() {
	mut item := imported.Item{
		value: 1
	}
	mut holder := RawPointerHolder{
		ptr: voidptr(&item)
	}
	imported.fill(mut holder.ptr)
	assert item.value == 42
	assert holder.ptr == voidptr(&item)
	holder.ptr = unsafe { nil }
	assert imported.is_null(mut holder.ptr)
}

fn fill_and_retain_raw_pointer(item &imported.Item) &voidptr {
	mut raw := voidptr(item)
	imported.fill(mut raw)
	return &raw
}

fn test_heap_promoted_raw_pointer_mut_argument_updates_original_storage() {
	mut item := imported.Item{
		value: 1
	}
	cell := fill_and_retain_raw_pointer(&item)
	assert item.value == 42
	assert unsafe { *cell } == voidptr(&item)
}

fn record_pointer_index(mut order []int, index int) int {
	order << index
	return index
}

fn test_raw_pointer_arguments_evaluate_indices_once_in_source_order() {
	mut first := imported.Item{}
	mut second := imported.Item{}
	mut pointers := [voidptr(&first), voidptr(&second)]
	mut order := []int{}
	imported.fill_pair(mut pointers[record_pointer_index(mut order, 0)],
		mut pointers[record_pointer_index(mut order, 1)])
	assert order == [0, 1]
	assert first.value == 41
	assert second.value == 42
	assert pointers == [voidptr(&first), voidptr(&second)]
}

fn test_mut_voidptr_parameter_still_receives_the_pointer_slot() {
	mut item := imported.Item{}
	mut raw := imported.RawPointer(voidptr(&item))
	imported.clear_pointer(mut raw)
	assert isnil(voidptr(raw))
}
