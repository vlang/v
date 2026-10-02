module types

import os
import v.parser
import v.pref

fn c_fixed_array_argument_errors(name string, source string) []TypeError {
	path := os.join_path(os.vtmp_dir(), 'v3_c_fixed_array_${name}_${os.getpid()}.c.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc.errors.clone()
}

fn test_c_fixed_array_arguments_accept_element_and_void_pointers() {
	errors := c_fixed_array_argument_errors('pointers', 'module main
fn C.matrix(values [16]f32)
fn C.get_data() voidptr
fn main() {
	values := []f32{len: 16}
	fixed := [16]f32{}
	C.matrix(C.get_data())
	C.matrix(&values[0])
	C.matrix(fixed)
}
')
	assert errors.len == 0, errors.str()
}

fn test_c_fixed_array_arguments_reject_incompatible_pointees() {
	errors := c_fixed_array_argument_errors('wrong_pointers', 'module main
fn C.matrix(values [16]f32)
fn main() {
	words := []i16{len: 16}
	values := []f32{len: 16}
	C.matrix(&words[0])
	ptr := &values[0]
	C.matrix(&ptr)
}
')
	assert errors.any(it.msg.contains('cannot use `&i16` as `[16]f32`')), errors.str()
	assert errors.any(it.msg.contains('cannot use `&&f32` as `[16]f32`')), errors.str()
}

fn test_v_fixed_array_arguments_keep_value_type_checks() {
	errors := c_fixed_array_argument_errors('v_values', 'module main
fn matrix(values [16]f32) {}
fn C.get_data() voidptr
fn main() {
	values := []f32{len: 16}
	matrix(C.get_data())
	matrix(&values[0])
}
')
	assert errors.any(it.msg.contains('cannot use `voidptr` as `[16]f32`')), errors.str()
	assert errors.any(it.msg.contains('cannot use `&f32` as `[16]f32`')), errors.str()
}
