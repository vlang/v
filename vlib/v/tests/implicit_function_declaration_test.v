// Test for implicit C function declaration detection
// This test verifies that V correctly catches implicit C function declarations

module main

import os

// `test_imp_c_function.v` filename can't contains `implicit`
const test_file = os.join_path(os.vtmp_dir(), 'test_imp_c_function.v')

fn testsuite_begin() {
	os.rm(test_file) or {}
	// Test code that should fail due to implicit C function declaration
	test_code := 'module main
fn C.custom_undeclared_function()
fn main() {
        C.custom_undeclared_function()
}
'
	// Write the test code to the temporary file
	os.write_file(test_file, test_code) or {
		eprintln('FAIL: Unable to write test file')
		exit(1)
	}
}

fn testsuite_end() {
	os.rm(test_file) or { eprintln('Warning: Unable to delete temporary file') }
}

fn test_implicit_c_function_declaration() {
	result := os.execute('v ${test_file}')
	assert result.exit_code != 0
	println(result.output)
	assert result.output.contains('custom_undeclared_function')
	// C4013 is for msvc
	assert result.output.contains('implicit') || result.output.contains('C4013')
}
