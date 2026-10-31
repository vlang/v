type OutputHandle = voidptr

fn output_handle_is_null(output &OutputHandle) bool {
	return output == unsafe { nil }
}

fn output_handle_matches(output &OutputHandle, expected &OutputHandle) bool {
	return output == expected
}

fn replace_output_handle(mut output &OutputHandle, replacement &OutputHandle) {
	output = replacement
}

fn mutable_handle_has_storage(mut handle OutputHandle) bool {
	return unsafe { &handle } != unsafe { nil }
}

fn test_null_pointer_to_handle_argument() {
	assert output_handle_is_null(unsafe { nil })
	assert output_handle_is_null(unsafe { &OutputHandle(nil) })
	assert output_handle_is_null(unsafe { &OutputHandle(voidptr(0)) })
	output := unsafe { &OutputHandle(nil) }
	assert output_handle_is_null(output)
}

fn test_mutable_null_handle_argument_has_storage() {
	assert mutable_handle_has_storage(mut unsafe { OutputHandle(nil) })
	assert mutable_handle_has_storage(mut unsafe { voidptr(nil) })
	assert mutable_handle_has_storage(mut unsafe { nil })
}

fn test_unsafe_pointer_to_handle_argument_preserves_address() {
	mut handle := OutputHandle(unsafe { nil })
	output := &handle
	assert output_handle_matches(unsafe { &OutputHandle(voidptr(output)) }, output)
	mut destination := unsafe { &OutputHandle(nil) }
	replace_output_handle(mut destination, output)
	assert output_handle_matches(destination, output)
}
