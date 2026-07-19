module vbugreport

fn test_new_stored_c_error_report_extracts_sql_fields() {
	report := new_stored_c_error_report('/tmp/v/program.tmp.c', 'linux', 'clang', '0.5.1 abcdef0',
		'amd64', 'autofree gc:boehm', '/tmp/v/program.tmp.c:12:7: error: unknown type name "Foo"', [
		'void main__main(void) {',
		'\tFoo x;',
	], [
		'foo := Foo{}',
	], 'fn main() {\n\tfoo := Foo{}\n}')
	assert report.c_file_name == 'program.tmp.c'
	assert report.target_os == 'linux'
	assert report.ccompiler == 'clang'
	assert report.v_version == '0.5.1 abcdef0'
	assert report.arch == 'amd64'
	assert report.build_options == 'autofree gc:boehm'
	assert report.error_string == 'error: unknown type name "Foo"'
	assert report.lines == 'void main__main(void) {\n\tFoo x;'
	assert report.v_lines == 'foo := Foo{}'
	assert report.v_source == 'fn main() {\n\tfoo := Foo{}\n}'
}

fn test_new_stored_c_error_report_handles_windows_c_file_path() {
	report := new_stored_c_error_report('C:\\tmp\\program.tmp.c', 'windows', 'msvc', 'V 0.5.1',
		'amd64', '', 'error: syntax error', []string{}, []string{}, '')
	assert report.c_file_name == 'program.tmp.c'
}

fn test_c_error_string_skips_warning_lines() {
	c_output := 'warning: unused command line argument\n  warning: note before error\nerror: invalid conversion'
	assert c_error_string(c_output) == 'error: invalid conversion'
}

fn test_c_error_string_handles_msvc_error_codes() {
	c_output := 'C:\\tmp\\program.tmp.c(19): error C2143: syntax error'
	assert c_error_string(c_output) == 'error: C2143: syntax error'
}

fn test_c_error_string_preserves_fatal_error_diagnostics() {
	c_output := '/tmp/program.tmp.c:7:1: fatal error: missing.h: No such file or directory'
	assert c_error_string(c_output) == 'error: fatal error: missing.h: No such file or directory'
}

fn test_c_error_string_returns_empty_without_error_line() {
	assert c_error_string('warning: unused command line argument\nnote: build stopped') == ''
}
