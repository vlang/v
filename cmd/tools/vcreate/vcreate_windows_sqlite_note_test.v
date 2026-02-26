module main

fn test_should_print_windows_web_sqlite_note() {
	assert should_print_windows_web_sqlite_note(.web, 'windows', false)
	assert !should_print_windows_web_sqlite_note(.web, 'windows', true)
	assert !should_print_windows_web_sqlite_note(.bin, 'windows', false)
	assert !should_print_windows_web_sqlite_note(.lib, 'windows', false)
	assert !should_print_windows_web_sqlite_note(.web, 'linux', false)
}
