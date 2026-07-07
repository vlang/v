import os

const round4_vexe = @VEXE
const round4_tests_dir = os.dir(@FILE)
const round4_v3_dir = os.dir(round4_tests_dir)
const round4_vlib_dir = os.dir(round4_v3_dir)
const round4_v3_src = os.join_path(round4_v3_dir, 'v3.v')

fn round4_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_comptime_review_round4_test_${os.getpid()}')
}

fn testsuite_begin() {
	v3_bin := round4_v3_bin_path()
	if os.exists(v3_bin) {
		os.rm(v3_bin) or {}
	}
}

fn round4_build_v3() string {
	v3_bin := round4_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${round4_vexe} -gc none -path "${round4_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${round4_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn round4_tmp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_round4_${name}_${os.getpid()}')
}

fn round4_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn round4_run_good(v3_bin string, name string, src string) string {
	src_path := '${round4_tmp_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	bin_path := round4_tmp_path(name)
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
	run := os.execute(bin_path)
	assert run.exit_code == 0, '${name}: ${run.output}'
	return run.output.trim_space()
}

fn round4_run_bad(v3_bin string, name string, src string, expected string) {
	src_path := '${round4_tmp_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	bin_path := round4_tmp_path(name)
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code != 0, '${name}: expected failure, got success\n${compile.output}'
	assert compile.output.contains(expected), '${name}: expected `${expected}` in\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
}

fn round4_run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	root := '${round4_tmp_path(name)}_project'
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		round4_write_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	bin_path := round4_tmp_path(name)
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
	run := os.execute(bin_path)
	assert run.exit_code == 0, '${name}: ${run.output}'
	return run.output.trim_space()
}

fn test_comptime_field_metadata_records_modifiers_and_attrs() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'field_metadata', "struct S {
	private int
pub:
	public string
mut:
	mutable bool
pub mut:
	pub_mut f64
	@[json: 'wire']
	attr int
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		rows << field.name + ':' + field.is_mut.str() + ':' + field.is_pub.str() + ':' + field.attrs.join(',')
	}
	println(rows.join('|'))
	}
	")
	assert out == "private:false:false:|public:false:true:|mutable:true:false:|pub_mut:true:true:|attr:true:true:json: 'wire'"
}

fn test_bare_comptime_field_materializes_fielddata() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'bare_fielddata', "type WireName = string

struct S {
pub:
	id int
mut:
	@[json: 'wire']
	name WireName
}

fn describe(field FieldData) string {
	return field.name + ':' + (field.typ > 0).str() + ':' + (field.unaliased_typ > 0).str() + ':' + (field.typ == field.unaliased_typ).str() + ':' + field.is_pub.str() + ':' + field.is_mut.str() + ':' + field.attrs.join(',')
}

fn main() {
	mut items := []FieldData{}
	mut rows := []string{}
	$for field in S.fields {
		items << field
		rows << describe(field)
	}
	rows << 'count:' + int_str(items.len)
	println(rows.join('|'))
}
")
	assert out == "id:true:true:true:true:false:|name:true:true:false:false:true:json: 'wire'|count:2"
}

fn test_unknown_comptime_field_member_is_rejected() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_field_member', 'struct S {
	name int
}

fn main() {
	$for field in S.fields {
		println(field.nmae)
	}
}
	',
		'unknown FieldData member `nmae`')
}

fn test_unknown_comptime_enum_value_member_is_rejected() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_enum_value_member', 'enum Color {
	red
}

fn main() {
	$for item in Color.values {
		println(item.nmae)
	}
}
	',
		'unknown EnumData member `nmae`')
}

fn test_unsupported_comptime_field_members_are_rejected() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_field_is_interface', 'struct S {
		name int
	}

fn main() {
	$for field in S.fields {
		println(field.is_interface)
	}
}
',
		'unknown FieldData member `is_interface`')
	round4_run_bad(v3_bin, 'bad_field_is_function', 'struct S {
		name int
	}

fn main() {
	$for field in S.fields {
		println(field.is_function)
	}
}
',
		'unknown FieldData member `is_function`')
}

fn test_enum_values_use_const_from_declaring_module() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'enum_const_decl_module', {
		'v.mod':           "Module { name: 'enum_const_decl_module' }\n"
		'colors/colors.v': 'module colors

const base = 4

pub enum Shade {
	dark = base + 2
	light
}

pub fn values() string {
	mut rows := []string{}
	$for item in Shade.values {
		rows << item.name + ":" + int_str(item.value)
	}
	return rows.join("|")
}
'
		'main.v':          'module main

import colors

fn main() {
	println(colors.values())
}
'
	}, 'main.v')
	assert out == 'dark:6|light:7'
}

fn test_enum_values_preserve_flag_values_and_attrs() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_flag_attrs', "@[flag]
enum Perm {
	read  @[json: 'r'; primary]
	write
	execute = 3
}

fn main() {
	mut rows := []string{}
	mut bare := []EnumData{}
	$for item in Perm.values {
		rows << item.name + ':' + int_str(item.value) + ':' + item.attrs.join(',')
		bare << item
	}
	rows << bare[0].name + ':' + int_str(bare[0].value) + ':' + bare[0].attrs.join(',')
	println(rows.join('|'))
}
")
	assert out == "read:1:json: 'r',primary|write:2:|execute:8:|read:1:json: 'r',primary"
}

fn test_selective_imported_enum_alias_resolves_underlying_enum() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'selective_enum_alias', {
		'v.mod':           "Module { name: 'selective_enum_alias' }\n"
		'colors/colors.v': 'module colors

pub enum Color {
	red = 3
	blue
}

pub type Shade = Color
pub type Tint = Shade
'
		'main.v':          'module main

import colors { Tint }

fn main() {
	println(Tint.red.str())
}
'
	}, 'main.v')
	assert out == 'red'
}

fn test_comptime_shared_type_group_is_checked_like_transformer() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'shared_type_group', "struct Counter {}

fn main() {
	$if shared Counter is $shared {
		println('shared')
	} $else {
		println(unknown_symbol)
	}
}
")
	assert out == 'shared'
}

fn test_comptime_voidptr_type_group_is_checked_like_transformer() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'voidptr_type_group', "fn main() {
	$if voidptr is $voidptr {
		println('voidptr')
	} $else {
		println(unknown_symbol)
	}
}
")
	assert out == 'voidptr'
}
