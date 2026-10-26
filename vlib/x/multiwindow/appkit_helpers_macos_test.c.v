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
