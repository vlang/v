// vtest build: !linux && !windows
module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_array_append_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_array_append_codegen_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	return gen.gen()
}

fn test_generate_c_uses_push_many_for_array_slice_append() {
	csrc := generate_array_append_c_for_test('
fn main() {
	mut dst := []u8{}
	haystack := [u8(1), 2, 3]
	dst << haystack[1..3]
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){array__slice(haystack, 1, 3)}')
}

fn test_generate_c_uses_push_many_for_array_literal_append() {
	csrc := generate_array_append_c_for_test('
fn main() {
	mut dst := []u8{}
	dst << [u8(`$`)]
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){new_array_from_c_array(')
}

fn test_generate_c_uses_push_many_for_local_array_append() {
	csrc := generate_array_append_c_for_test('
fn source() []u8 {
	return [u8(1), 2]
}

fn main() {
	mut dst := []u8{}
	src := source()
	dst << src
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){src}')
}

fn test_generate_c_uses_push_many_for_fn_pointer_array_return_append() {
	csrc := generate_array_append_c_for_test('
fn source(data []u8) []u8 {
	return data
}

fn consume(hash_func fn ([]u8) []u8) {
	mut dst := []u8{}
	src := hash_func(dst)
	dst << src
}

fn main() {
	consume(source)
}
')
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){src}')
}

fn test_generate_c_keeps_bitwise_or_inside_array_append_value() {
	csrc := generate_array_append_c_for_test('
fn main() {
	mut bytes := []u8{}
	n1 := u8(1)
	n0 := u8(2)
	bytes << (n1 << 4) | n0
}
')
	assert csrc.contains('array__push(')
	assert csrc.contains('| n0')
	assert !csrc.contains('}) | n0')
}

fn test_generate_c_indexes_local_array_that_shadows_function_name() {
	csrc := generate_array_append_c_for_test('
fn bytes() []u8 {
	return []u8{}
}

fn main() {
	mut bytes := [u8(1)]
	bytes[0] &= u8(3)
}
')
	assert csrc.contains('((u8*)bytes.data)[((int)(0))] &= ((u8)(3));')
	assert !csrc.contains('bytes &= ((u8)(3));')
}

fn test_generate_c_uses_typed_zero_for_array_tuple_if_declarations() {
	csrc := generate_array_append_c_for_test('
fn pick(operand_a []u64, operand_b []u64) {
	mut a, mut b := if operand_a.len >= operand_b.len {
		operand_a, operand_b
	} else {
		operand_b, operand_a
	}
	_ = a
	_ = b
}
')
	assert csrc.contains('Array_u64 a = ((Array_u64){0});')
	assert csrc.contains('Array_u64 b = ((Array_u64){0});')
	assert !csrc.contains('Array_u64 a = 0;')
	assert !csrc.contains('Array_u64 b = 0;')
}
