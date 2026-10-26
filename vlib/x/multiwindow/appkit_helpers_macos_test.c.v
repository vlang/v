module multiwindow

import macos

fn C.free(ptr voidptr)

fn test_appkit_string_helpers_preserve_payload_and_copy_storage() {
	assert macos.utf8_string(v_multiwindow_appkit_string(unsafe { nil })) == ''
	assert v_multiwindow_appkit_copy_cstring(unsafe { nil }) == unsafe { nil }
	for value in ['', 'Window title', 'Fenêtre 日本語'] {
		native := v_multiwindow_appkit_string(&char(value.str))
		assert native != unsafe { nil }
		assert macos.utf8_string(native) == value
		copy := v_multiwindow_appkit_copy_cstring(&char(value.str))
		assert copy != unsafe { nil }
		assert voidptr(copy) != voidptr(value.str)
		assert unsafe { copy.vstring() } == value
		C.free(copy)
	}
}

fn test_appkit_content_size_helper_clamps_only_to_positive_minimum() {
	assert v_multiwindow_appkit_max_int(40, 80) == 80
	assert v_multiwindow_appkit_max_int(80, 80) == 80
	assert v_multiwindow_appkit_max_int(120, 80) == 120
	assert v_multiwindow_appkit_max_int(0, 80) == 80
	assert v_multiwindow_appkit_max_int(-10, 80) == 80
	assert v_multiwindow_appkit_max_int(40, 0) == 40
	assert v_multiwindow_appkit_max_int(-10, 0) == -10
	assert v_multiwindow_appkit_max_int(-10, -5) == -10
}
