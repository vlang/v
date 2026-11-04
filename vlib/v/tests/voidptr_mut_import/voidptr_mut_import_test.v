import voidptr_mut_import.imported

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

fn test_mut_voidptr_parameter_still_receives_the_pointer_slot() {
	mut item := imported.Item{}
	mut raw := imported.RawPointer(voidptr(&item))
	imported.clear_pointer(mut raw)
	assert isnil(voidptr(raw))
}
