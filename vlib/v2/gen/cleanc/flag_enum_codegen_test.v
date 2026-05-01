module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_c_for_test(code string) string {
	tmp_file := '/tmp/v2_flag_enum_codegen_test_${os.getpid()}.v'
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

fn test_generate_c_rewrites_flag_enum_zero_static_call() {
	csrc := generate_c_for_test('
@[flag]
enum Bits {
	a
	b
}

fn main() {
	mut bits := Bits.zero()
	bits.set(.a)
	_ = bits.has(.a)
}
')
	assert csrc.contains('(Bits)(0)')
	assert !csrc.contains('Bits__zero')
}

fn test_generate_c_escapes_keyword_struct_fields_in_map_eq() {
	csrc := generate_c_for_test("
struct Item {
	short string
}

fn main() {
	a := {
		'x': Item{short: 'a'}
	}
	b := {
		'x': Item{short: 'b'}
	}
	_ = a == b
}
")
	assert csrc.contains('string__eq(va._short, vb._short)')
	assert !csrc.contains('string__eq(va.short, vb.short)')
}

fn test_generate_c_uses_concrete_map_method_name_in_generic_comptime_body() {
	code := [
		'fn (m map[string]int) query_item(name string) ?int {',
		'	return m[name]',
		'}',
		'',
		'struct Holder {',
		'	items map[string]int',
		'}',
		'',
		'fn find_item[T](value T) ?int {',
		'	@DLR@if T is Holder {',
		'		return value.items.query_item(@SQ@x@SQ@)',
		'	}',
		'	return none',
		'}',
		'',
		'fn main() {',
		'	_ = find_item[Holder](Holder{})',
		'}',
	].join('\n').replace('@DLR@', '$').replace('@SQ@', "'")
	csrc := generate_c_for_test(code)
	assert csrc.contains('Map_string_int__query_item(')
	assert !csrc.contains('map__query_item(')
}

