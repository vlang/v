module main

import os

fn C.RegCreateKeyExW(hkey voidptr, sub_key &u16, reserved u32, class_name &u16, options u32, access u32, security_attributes voidptr, result voidptr, disposition voidptr) i32
fn C.RegDeleteKeyW(hkey voidptr, sub_key &u16) i32

fn test_append_path_entry_preserves_existing_entries() {
	existing := r'C:\Tools;\\server\share\bin;%USERPROFILE%\bin'
	entry := r'D:\v\.bin'
	assert append_path_entry(existing, entry, ';', true) == '${existing};${entry}'
}

fn test_append_path_entry_detects_case_insensitive_match() {
	existing := r'C:\Tools;D:\V\.BIN\'
	assert append_path_entry(existing, r'd:\v\.bin', ';', true) == existing
}

fn test_append_path_entry_handles_empty_and_trailing_delimiter() {
	entry := r'D:\v\.bin'
	assert append_path_entry('', entry, ';', true) == entry
	assert append_path_entry(r'C:\Tools;', entry, ';', true) == r'C:\Tools;D:\v\.bin'
}

fn test_registry_path_larger_than_old_limit_is_preserved() {
	key_path := 'Software\\VlangVsymlinkTest_${os.getpid()}'
	mut reg_key := os.hkey_current_user
	create_result := C.RegCreateKeyExW(os.hkey_current_user, key_path.to_wide(), 0, 0,
		C.REG_OPTION_VOLATILE, 1 | 2, 0, voidptr(&reg_key), 0)
	assert create_result == 0
	if create_result != 0 {
		return
	}
	defer {
		C.RegCloseKey(reg_key)
		C.RegDeleteKeyW(os.hkey_current_user, key_path.to_wide())
	}
	mut entries := []string{}
	for i in 0 .. 500 {
		entries << 'C:\\Tools\\${i}'
	}
	entries << 'C:\\Путь'
	original := entries.join(';')
	assert original.len > 4095
	set_reg_value(reg_key, 'Path', original) or {
		assert false, err.msg()
		return
	}
	stored := get_reg_value(reg_key, 'Path') or {
		assert false, err.msg()
		return
	}
	assert stored == original
}
