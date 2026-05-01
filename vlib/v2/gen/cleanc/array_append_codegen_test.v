module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_array_append_c_for_test(code string) string {
	tmp_file := '/tmp/v2_array_append_codegen_test_${os.getpid()}.v'
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

fn test_generate_c_uses_push_many_for_bytes_method_append_in_fn_literal() {
	csrc := generate_array_append_c_for_test("
fn main() {
	caps := ['foo']
	cb := fn [caps] (i usize, mut dst []u8) {
		if i < caps.len {
			dst << caps[i].bytes()
		}
	}
	mut dst := []u8{}
	cb(0, mut dst)
}
")
	assert csrc.contains('array__push_many(')
	assert !csrc.contains('&(u8[1]){string__bytes(')
}
