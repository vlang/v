module os

// FormatMessageW ends a system message with a line break. get_error_msg returns
// the message without it, and removes nothing else: the text keeps its period.
fn test_get_error_msg_drops_only_the_trailing_line_break() {
	// ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND, ERROR_ACCESS_DENIED,
	// ERROR_INVALID_PARAMETER, ERROR_ALREADY_EXISTS, ERROR_PRIVILEGE_NOT_HELD
	for code in [2, 3, 5, 87, 183, 1314] {
		raw_ptr := ptr_win_get_error_msg(u32(code))
		assert raw_ptr != 0, 'no system message for code ${code}'
		// FormatMessageW allocated a UTF-16 buffer; read it before it is freed.
		raw := wide_ptr_to_string(unsafe { &u16(raw_ptr) })
		C.LocalFree(raw_ptr)
		msg := get_error_msg(code)
		assert msg != '', 'code ${code}'
		assert !msg.ends_with('\n') && !msg.ends_with('\r'), 'code ${code}: ${msg.bytes()}'
		assert raw == msg + '\r\n', 'code ${code}: ${raw.bytes()}'
	}
}

// The errors of the functions built on get_error_msg end with the system
// message, so they must not end with a line break either.
fn test_windows_api_errors_have_no_trailing_line_break() {
	root := join_path(vtmp_dir(), 'windows_error_msg_${getpid()}')
	mkdir_all(root)!
	defer {
		rmdir_all(root) or {}
	}
	origin := join_path(root, 'origin.txt')
	write_file(origin, 'origin')!
	mut msgs := []string{}
	mkdir(join_path(root, 'missing', 'sub')) or { msgs << 'mkdir: ${err.msg()}' }
	link(join_path(root, 'missing.txt'), join_path(root, 'new.txt')) or {
		msgs << 'link: ${err.msg()}'
	}
	symlink(origin, join_path(root, 'missing', 'link.txt')) or { msgs << 'symlink: ${err.msg()}' }
	get_long_path(join_path(root, 'MISSIN~1.TXT')) or { msgs << 'get_long_path: ${err.msg()}' }
	assert msgs.len == 4, msgs.str()
	for msg in msgs {
		assert !msg.ends_with('\n') && !msg.ends_with('\r'), '${msg.bytes()}'
	}
}
