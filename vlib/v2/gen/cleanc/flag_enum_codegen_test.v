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

fn test_generate_c_lowers_map_ops_in_comptime_field_attrs_loop() {
	code := [
		'struct Schema {',
		'	field string @[long: @SQ@value@SQ@; short: x]',
		'}',
		'',
		'fn get_info[T]() {',
		'	@DLR@if T is @DLR@struct {',
		'		@DLR@for field in T.fields {',
		'			mut attrs := map[string]string{}',
		'			for attr in field.attrs {',
		'				if attr.contains(@SQ@:@SQ@) {',
		'					split := attr.split(@SQ@:@SQ@)',
		'					attrs[split[0].trim_space()] = split[1].trim_space()',
		'				} else {',
		'					attrs[attr.trim_space()] = @SQ@true@SQ@',
		'				}',
		'			}',
		'			if long_alias := attrs[@SQ@long@SQ@] {',
		'				_ = long_alias.replace(@SQ@_@SQ@, @SQ@-@SQ@)',
		'			}',
		'		}',
		'	}',
		'}',
		'',
		'fn main() {',
		'	get_info[Schema]()',
		'}',
	].join('\n').replace('@SQ@', "'").replace('@DLR@', '$')
	csrc := generate_c_for_test(code)
	assert csrc.contains('map__set(')
	assert !csrc.contains('cannot resolve map type for index expr')
	assert !csrc.contains('bool__replace')
}

fn test_generate_c_rewrites_typeof_name_in_comptime_field_loop() {
	code := [
		'struct Schema {',
		'	field string @[long: @SQ@value@SQ@]',
		'}',
		'',
		'fn get_info[T]() {',
		'	@DLR@if T is @DLR@struct {',
		'		@DLR@for field in T.fields {',
		'			_ = typeof(field).name',
		'		}',
		'	}',
		'}',
		'',
		'fn main() {',
		'	get_info[Schema]()',
		'}',
	].join('\n').replace('@SQ@', "'").replace('@DLR@', '$')
	csrc := generate_c_for_test(code)
	assert csrc.contains('(string){.str = "field", .len = sizeof("field") - 1, .is_lit = 1}')
	assert !csrc.contains('(string){.str = "field", .len = sizeof("field") - 1, .is_lit = 1}.name')
}

fn test_generate_c_lowers_flag_enum_has_in_generic_comptime_body() {
	code := [
		'@[flag]',
		'enum Bits {',
		'	a',
		'	b',
		'}',
		'',
		'struct Holder {',
		'	bits Bits',
		'}',
		'',
		'fn use_bits[T](value T) {',
		'	@DLR@if T is Holder {',
		'		_ = value.bits.has(.a)',
		'	}',
		'}',
		'',
		'fn main() {',
		'	use_bits[Holder](Holder{})',
		'}',
	].join('\n').replace('@DLR@', '$')
	csrc := generate_c_for_test(code)
	assert csrc.contains('__v2_flag_has_int')
	assert csrc.contains('Bits__a')
	assert !csrc.contains('Bits__a.err')
	assert !csrc.contains('((int)())')
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

fn test_generate_c_rewrites_continue_in_generic_comptime_field_loop() {
	code := [
		'struct Schema {',
		'	field string @[long: @SQ@value@SQ@]',
		'	skip  string @[ignore]',
		'}',
		'',
		'fn get_info[T]() {',
		'	@DLR@if T is @DLR@struct {',
		'		@DLR@for field in T.fields {',
		'			mut attrs := map[string]string{}',
		'			for attr in field.attrs {',
		'				if attr.contains(@SQ@:@SQ@) {',
		'					parts := attr.split_nth(@SQ@:@SQ@, 2)',
		'					attrs[parts[0].trim_space()] = parts[1].trim_space()',
		'				} else {',
		'					attrs[attr.trim_space()] = @SQ@true@SQ@',
		'				}',
		'			}',
		'			if @SQ@ignore@SQ@ in attrs {',
		'				continue',
		'			}',
		'			_ = field.name',
		'		}',
		'	}',
		'}',
		'',
		'fn main() {',
		'	get_info[Schema]()',
		'}',
	].join('\n').replace('@SQ@', "'").replace('@DLR@', '$')
	csrc := generate_c_for_test(code)
	assert csrc.contains('goto __v_ctf_continue_')
	assert csrc.contains('__v_ctf_continue_')
}

fn test_generate_c_keeps_nameless_comptime_attrs_without_colon_prefix() {
	code := [
		'struct Schema {',
		'	field int @[repeats]',
		'}',
		'',
		'fn get_info[T]() {',
		'	@DLR@if T is @DLR@struct {',
		'		@DLR@for field in T.fields {',
		'			for attr in field.attrs {',
		'				_ = attr',
		'			}',
		'		}',
		'	}',
		'}',
		'',
		'fn main() {',
		'	get_info[Schema]()',
		'}',
	].join('\n').replace('@DLR@', '$')
	csrc := generate_c_for_test(code)
	assert csrc.contains('(string){.str = "repeats", .len = sizeof("repeats") - 1, .is_lit = 1}')
	assert !csrc.contains('(string){.str = ": repeats", .len = sizeof(": repeats") - 1, .is_lit = 1}')
}

fn test_generate_c_recurses_struct_equality_through_nested_string_fields() {
	csrc := generate_c_for_test("
struct Inner {
	label string
}

struct Outer {
	inner Inner
}

fn main() {
	a := Outer{
		inner: Inner{label: 'x'}
	}
	b := Outer{
		inner: Inner{label: 'x'}
	}
	_ = a == b
}
")
	assert csrc.contains('string__eq(_cmp_l_')
	assert !csrc.contains('memcmp(&_cmp_l_')
}
