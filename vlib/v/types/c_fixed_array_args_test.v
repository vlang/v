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

fn test_c_fixed_array_arguments_require_c_integer_storage() {
	previous_bits := platform_int_bits()
	set_platform_int_bits(64)
	defer { set_platform_int_bits(previous_bits) }
	errors := c_fixed_array_argument_errors('integer_storage', 'module main
type CInt = int
type IntStorage = i32
fn C.sum(values [2]int) int
fn C.alias_sum(values [2]CInt) int
fn C.wide_sum(values [2]i64) i64
fn C.get_data() voidptr
fn main() {
	values := [int(10), 20]
	wide := [i64(10), 20]
	storage := [i32(10), 20]
	aliases := [IntStorage(10), 20]
	C.sum(&values[0])
	C.sum(&wide[0])
	C.alias_sum(&values[0])
	C.sum(&storage[0])
	C.sum(&aliases[0])
	C.alias_sum(&storage[0])
	C.wide_sum(&wide[0])
	C.sum(C.get_data())
	C.sum(unsafe { nil })
}
')
	assert errors.len == 3, errors.str()
	assert errors.any(it.msg.contains('cannot use `&int` as `[2]int`')), errors.str()
	assert errors.any(it.msg.contains('cannot use `&i64` as `[2]int`')), errors.str()
	assert errors.any(it.msg.contains('cannot use `&int` as `[2]CInt`')), errors.str()
}

fn test_c_fixed_array_pointer_elements_require_c_integer_storage() {
	previous_bits := platform_int_bits()
	set_platform_int_bits(64)
	defer { set_platform_int_bits(previous_bits) }
	errors := c_fixed_array_argument_errors('integer_pointer_storage', 'module main
fn C.rows(values [2]&int)
fn main() {
	value := int(10)
	storage := i32(10)
	values := [&value, &value]
	rows := [&storage, &storage]
	C.rows(&values[0])
	C.rows(&rows[0])
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].msg.contains('cannot use `&&int` as `[2]&int`'), errors.str()
}

fn test_c_fixed_array_arguments_accept_platform_integer_storage_on_32_bit_targets() {
	previous_bits := platform_int_bits()
	set_platform_int_bits(32)
	defer { set_platform_int_bits(previous_bits) }
	errors := c_fixed_array_argument_errors('integer_storage_32', 'module main
fn C.sum(values [2]int) int
fn main() {
	values := [int(10), 20]
	C.sum(&values[0])
}
')
	assert errors.len == 0, errors.str()
}

fn test_c_fixed_array_row_pointers_require_c_integer_storage() {
	previous_bits := platform_int_bits()
	set_platform_int_bits(64)
	defer { set_platform_int_bits(previous_bits) }
	errors := c_fixed_array_argument_errors('integer_row_storage', 'module main
fn C.rows(values [2][2]int)
fn main() {
	storage := [2][2]i32{}
	wide := [2][2]int{}
	C.rows(&storage[0])
	C.rows(&wide[0])
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].msg.contains('cannot use `&[2]int` as `[2][2]int`'), errors.str()
}

fn test_c_fixed_array_callback_pointers_require_c_integer_storage() {
	previous_bits := platform_int_bits()
	set_platform_int_bits(64)
	defer { set_platform_int_bits(previous_bits) }
	errors := c_fixed_array_argument_errors('integer_callback_storage', 'module main
fn C.callbacks(values [2]fn (int) int)
fn narrow(value i32) i32 { return value }
fn wide(value int) int { return value }
fn main() {
	storage := [narrow, narrow]
	values := [wide, wide]
	C.callbacks(&storage[0])
	C.callbacks(&values[0])
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].msg.contains('cannot use `&fn (int) int` as `[2]fn (int) int`'), errors.str()
}
