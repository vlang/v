module types

import os
import v.parser
import v.pref

fn test_c_integer_constant_adopts_numeric_alias_call_context() {
	path := os.join_path(os.vtmp_dir(), 'v3_c_constant_context_${os.getpid()}.c.v')
	os.write_file(path, 'module main\n\ntype Flag = usize\n\nfn consume(value Flag) {}\n\nfn main() { consume(C.TEST_FLAG) }\n')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_windows_invalid_handle_constant_has_pointer_type() {
	path := os.join_path(os.vtmp_dir(), 'v3_invalid_handle_constant_${os.getpid()}.c.v')
	os.write_file(path, 'module main

fn C.GetStdHandle(kind int) voidptr

fn handle_is_valid(handle voidptr) bool {
	return handle != C.INVALID_HANDLE_VALUE && handle != C.NULL
}

fn main() {
	handle := C.GetStdHandle(C.STD_INPUT_HANDLE)
	invalid := handle == C.INVALID_HANDLE_VALUE
	valid := handle_is_valid(handle)
	_ = invalid
	_ = valid
}
')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
}
