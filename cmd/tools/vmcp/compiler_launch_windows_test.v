module main

import os

fn test_invalid_windows_executable_returns_an_error_and_the_caller_survives() {
	root := os.join_path(os.vtmp_dir(), 'mcp invalid exe review ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	invalid := os.join_path(root, 'invalid.exe')
	os.write_file(invalid, 'this is not a PE executable')!
	ws := Workspace{ compiler: invalid, root: root }
	failed := run_compiler(&ws, ['version'])
	assert !failed.started(), failed.output
	assert failed.exit_code != 0
	assert failed.launch_error.starts_with('CreateProcessW:'), failed.launch_error
	valid := Workspace{ compiler: @VEXE, root: root }
	continued := run_compiler(&valid, ['version'])
	assert continued.started(), continued.launch_error
	assert continued.exit_code == 0, continued.output
}

fn test_windows_launcher_reports_an_invalid_work_folder() {
	root := os.join_path(os.vtmp_dir(), 'mcp invalid cwd review ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	file := os.join_path(root, 'not_a_directory')
	os.write_file(file, 'file')!
	// Exercise CreateProcessW's cwd rejection beyond run_compiler's preflight.
	for directory in [file, os.join_path(root, 'missing')] {
		failed := run_compiler_windows(@VEXE, ['version'], directory)
		assert !failed.started(), failed.output
		assert failed.exit_code != 0
		assert failed.launch_error.starts_with('CreateProcessW:'), failed.launch_error
	}
}

fn test_windows_capture_decodes_utf16_and_preserves_utf8() {
	assert windows_compiler_output('plain UTF-8 ✓') == 'plain UTF-8 ✓'
	assert windows_compiler_output('\xff\xfer\x00e\x00a\x00d\x00') == 'read'
	assert windows_compiler_output('\xfe\xff\x00r\x00e\x00a\x00d') == 'read'
	raw := '\xe9'
	wide := raw.to_wide(from_ansi: true)
	defer { unsafe { free(wide) } }
	assert windows_compiler_output(raw) == unsafe { string_from_wide(wide) }
}

fn test_windows_compiler_argv_quotes_are_literal_and_handle_backslashes() {
	assert windows_compiler_arg('') == '""'
	assert windows_compiler_arg('one argument') == '"one argument"'
	assert windows_compiler_arg(r'a"b') == r'"a\"b"'
	assert windows_compiler_arg('C:\\trailing\\') == '"C:\\trailing\\\\"'
	assert windows_compiler_arg(r'back\"quote') == r'"back\\\"quote"'
	assert windows_compiler_command_line('C:\\compiler dir\\v.exe', [
		'',
		'%PATH%',
		'!literal!',
		'$literal',
		'& ; |',
	]) == '"C:\\compiler dir\\v.exe" "" "%PATH%" "!literal!" "$literal" "& ; |"'
}
