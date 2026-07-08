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

fn test_comptime_for_value_field_sources_use_value_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'value_field_source', "struct Inner {
	count int
}

struct S {
	id int
	name string
	nested Inner
}

fn names_from_param(param S) string {
	mut rows := []string{}
	$for field in param.fields {
		rows << field.name
	}
	return rows.join(',')
}

fn nested_names(param S) string {
	mut rows := []string{}
	$for field in param.nested.fields {
		rows << field.name
	}
	return rows.join(',')
}

fn main() {
	value := S{}
	mut rows := []string{}
	$for field in value.fields {
		rows << field.name
	}
	rows << names_from_param(value)
	rows << nested_names(value)
	println(rows.join('|'))
}
")
	assert out == 'id|name|nested|id,name,nested|count'
}

fn test_comptime_for_import_alias_source_values() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'import_alias_values', {
		'v.mod':           "Module { name: 'import_alias_values' }\n"
		'colors/colors.v': 'module colors

pub enum Shade {
	red
	blue
}
'
		'main.v':          'module main

import colors as palette

fn main() {
	mut rows := []string{}
	$for item in palette.Shade.values {
		rows << item.name
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'red|blue'
}

fn test_comptime_for_nested_value_source_uses_resolved_field_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'nested_value_resolved_field_type', {
		'v.mod':     "Module { name: 'nested_value_resolved_field_type' }\n"
		'pkg/pkg.v': 'module pkg

pub struct Inner {
pub:
	count int
}

pub struct Outer {
pub:
	nested Inner
}

pub fn make_outer() Outer {
	return Outer{}
}
'
		'main.v':    'module main

import pkg

fn nested_names(value pkg.Outer) string {
	mut rows := []string{}
	$for field in value.nested.fields {
		rows << field.name
	}
	return rows.join("|")
}

fn main() {
	println(nested_names(pkg.make_outer()))
}
'
	}, 'main.v')
	assert out == 'count'
}

fn test_comptime_field_typ_guards_use_declaring_module_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'field_typ_guard_decl_module', {
		'v.mod':     "Module { name: 'field_typ_guard_decl_module' }\n"
		'pkg/pkg.v': 'module pkg

pub struct Inner {
pub:
	id int
}

pub struct Outer {
pub:
	nested Inner
}
'
		'main.v':    'module main

import pkg

fn main() {
	mut rows := []string{}
	$for field in pkg.Outer.fields {
		$if field.typ is pkg.Inner {
			rows << "qualified:" + field.name
		}
		$if field.typ is $struct {
			rows << "struct:" + field.name
		}
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'qualified:nested|struct:nested'
}

fn test_comptime_field_unaliased_typ_preserves_option_wrapper() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'field_unaliased_option', "type Alias = string

struct S {
	opt_alias ?Alias
	opt_string ?string
}

fn same_type_ids(field FieldData) string {
	return (field.typ == field.unaliased_typ).str()
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field.unaliased_typ is ?string {
			rows << field.name + ':option:' + same_type_ids(field)
		} $else $if field.unaliased_typ is string {
			rows << field.name + ':payload:' + same_type_ids(field)
		} $else {
			rows << field.name + ':other:' + same_type_ids(field)
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'opt_alias:option:false|opt_string:option:true'
}

fn test_comptime_for_source_alias_chains_unroll() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'comptime_for_alias_chain', "enum Color {
	red
	blue
}

type Shade = Color
type Tint = Shade

struct S {
	id int
}

type StructAlias = S
type StructTint = StructAlias

fn main() {
	mut rows := []string{}
	$for item in Tint.values {
		rows << 'value:' + item.name
	}
	$for field in StructTint.fields {
		rows << 'field:' + field.name
	}
	println(rows.join('|'))
}
")
	assert out == 'value:red|value:blue|field:id'
}

fn test_comptime_for_selective_import_alias_source_unrolls() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'selective_comptime_for_alias_chain', {
		'v.mod':           "Module { name: 'selective_comptime_for_alias_chain' }\n"
		'colors/colors.v': 'module colors

pub enum Color {
	red
	blue
}

pub type Shade = Color
pub type Tint = Shade

pub struct S {
pub:
	id int
}

pub type StructAlias = S
pub type StructTint = StructAlias
'
		'main.v':          'module main

import colors { StructTint, Tint }

fn main() {
	mut rows := []string{}
	$for item in Tint.values {
		rows << "value:" + item.name
	}
	$for field in StructTint.fields {
		rows << "field:" + field.name
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'value:red|value:blue|field:id'
}

fn test_selective_import_comptime_for_static_pruning_resolves_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'selective_import_comptime_for_static_pruning', {
		'v.mod':     "Module { name: 'selective_import_comptime_for_static_pruning' }\n"
		'pkg/pkg.v': 'module pkg

pub struct S {
pub:
	id int
}
'
		'main.v':    'module main

import pkg { S }

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field.name == "id" {
			rows << field.name
		} $else {
			missing_fn()
		}
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'id'
}

fn test_non_generic_call_matching_generic_short_name_does_not_skip_comptime_for() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'generic_short_name_false_positive', {
		'v.mod':             "Module { name: 'generic_short_name_false_positive' }\n"
		'helpers/helpers.v': "module helpers

pub fn encode[T](value T) string {
	return 'generic'
}
"
		'main.v':            "module main

import helpers

struct S {
	id int
}

fn encode() string {
	return 'ok'
}

fn main() {
	warm := encode()
	generic_warm := helpers.encode(1)
	mut rows := []string{}
	if warm.len == 0 {
		rows << warm
	}
	if generic_warm.len == 0 {
		rows << generic_warm
	}
	$for field in S.fields {
		rows << field.name + ':' + encode()
	}
	println(rows.join('|'))
}
"
	}, 'main.v')
	assert out == 'id:ok'
}

fn test_unreachable_generic_call_in_metadata_if_does_not_skip_comptime_for() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'unreachable_generic_call_in_comptime_for', "struct S {
	id int
}

enum Color {
	red
}

fn generic_fn[T]() string {
	return 'bad'
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field.name == 'missing' {
			rows << generic_fn[int]()
		}
		rows << field.name
	}
	$for item in Color.values {
		$if item.name == 'missing' {
			rows << generic_fn[int]()
		}
		rows << item.name
	}
	println(rows.join('|'))
}
")
	assert out == 'id|red'
}

fn test_value_source_comptime_for_static_pruning_resolves_field_path() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'value_source_comptime_for_static_pruning', "struct Inner {
	id int
}

struct Outer {
	nested Inner
}

fn collect(param Outer) []string {
	mut rows := []string{}
	$for field in param.nested.fields {
		$if field is string {
			missing_fn()
		} $else {
			rows << field.name
		}
	}
	return rows
}

fn main() {
	println(collect(Outer{}).join('|'))
}
")
	assert out == 'id'
}

fn test_shadowed_comptime_for_substitutes_nested_source_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'shadowed_comptime_for_nested_source', "struct Inner {
	name string
}

struct Outer {
	nested Inner
}

fn main() {
	mut rows := []string{}
	$for field in Outer.fields {
		$if field is $struct {
			$for field in field.typ.fields {
				rows << field.name
			}
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'name'
}

fn test_metadata_call_arguments_are_checked_against_callee_types() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_metadata_call_arg_type', 'struct S {
	id int
}

fn takes_int(x int) {}

fn main() {
	$for field in S.fields {
		takes_int(field.name)
	}
}
',
		'cannot use `string` as argument 1 to `takes_int`; expected `int`')
}

fn test_cross_module_composite_field_typ_guard_uses_qualified_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'cross_module_composite_field_typ_guard', {
		'v.mod':     "Module { name: 'cross_module_composite_field_typ_guard' }\n"
		'pkg/pkg.v': 'module pkg

pub struct Inner {
pub:
	id int
}

pub struct Outer {
pub:
	items map[string]Inner
}
'
		'main.v':    'module main

import pkg

fn main() {
	mut rows := []string{}
	$for field in pkg.Outer.fields {
		$if field.typ is map[string]pkg.Inner {
			rows << field.name
		} $else {
			missing_fn()
		}
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'items'
}

fn test_enum_value_metadata_call_arguments_use_enumdata_types() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_value_metadata_call_args', "enum Color {
	red = 7
}

fn use_enum(item EnumData) string {
	return item.name
}

fn use_i64(value i64) string {
	return value.str()
}

fn main() {
	mut rows := []string{}
	$for item in Color.values {
		rows << use_enum(item) + ':' + use_i64(item.value)
	}
	println(rows.join('|'))
}
")
	assert out == 'red:7'
}

fn test_enum_value_metadata_value_arg_is_checked() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_enum_value_metadata_arg_type', 'enum Color {
	red
}

fn takes_string(value string) {}

fn main() {
	$for item in Color.values {
		takes_string(item.value)
	}
}
',
		'cannot use `i64` as argument 1 to `takes_string`; expected `string`')
}

fn test_enum_value_metadata_if_prunes_untaken_branches() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_value_metadata_if_prunes_else', "enum Color {
	only
}

fn main() {
	mut rows := []string{}
	$for item in Color.values {
		$if item.name == 'only' {
			rows << item.name
		} $else {
			missing_fn()
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'only'
}

fn test_enum_value_metadata_handles_unsigned_shift() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_value_metadata_unsigned_shift', "enum Shifted {
	a = 8 >>> 1
}

fn main() {
	mut rows := []string{}
	$for item in Shifted.values {
		rows << item.value.str()
	}
	println(rows.join('|'))
}
")
	assert out == '4'
}

fn test_enum_value_static_pruning_resolves_forward_refs() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_value_static_pruning_forward_refs', "@[_allow_multiple_values]
enum E {
	a = .c
	c = 2
}

fn main() {
	mut rows := []string{}
	$for item in E.values {
		$if item.value == 2 {
			rows << item.name
		} $else {
			missing_fn()
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'a|c'
}

fn test_cross_module_dynamic_field_selector_uses_qualified_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'cross_module_dynamic_field_selector_type', {
		'v.mod':     "Module { name: 'cross_module_dynamic_field_selector_type' }\n"
		'pkg/pkg.v': 'module pkg

pub struct Child {
pub:
	value int
}

pub fn (c Child) str() string {
	return "child:" + c.value.str()
}

pub struct Outer {
pub:
	child Child
}
'
		'main.v':    'module main

import pkg

fn main() {
	outer := pkg.Outer{
		child: pkg.Child{
			value: 9
		}
	}
	mut rows := []string{}
	$for field in pkg.Outer.fields {
		rows << outer.$(field.name).str()
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'child:9'
}

fn test_comptime_for_typeof_dynamic_field_selector_is_preserved() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'typeof_dynamic_field_selector_in_comptime_for', "struct Foo {
	id int
	name string
}

fn main() {
	foo := Foo{
		id: 1
		name: 'Ada'
	}
	mut rows := []string{}
	$for field in Foo.fields {
		rows << typeof(foo.$(field.name)).name
	}
	println(rows.join('|'))
}
")
	assert out == 'int|string'
}

fn test_comptime_for_body_checks_static_code() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_static_code_in_comptime_for', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		missing_fn()
	}
}
',
		'unknown function `missing_fn`')
}

fn test_comptime_for_body_checks_static_code_in_regular_if_branch() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_static_code_in_comptime_for_if_branch', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		if field.name == "id" {
			missing_branch_fn()
		}
	}
}
',
		'unknown function `missing_branch_fn`')
}

fn test_comptime_for_body_checks_static_call_in_mixed_expression() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_static_call_in_comptime_for_mixed_expr', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		missing_fn(field.name)
	}
}
',
		'unknown function `missing_fn`')
}

fn test_comptime_field_type_selectors_are_type_ids() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'field_type_selectors_are_type_ids', 'type MyInt = int

struct S {
	id int
	alias MyInt
}

fn main() {
	mut rows := []string{}
	mut int_typ := 0
	mut alias_typ := 0
	mut alias_unaliased := 0
	$for field in S.fields {
		if field.name == "id" {
			int_typ = field.typ
			assert field.typ == field.unaliased_typ
			assert field.typ != 0
		}
		if field.name == "alias" {
			alias_typ = field.typ
			alias_unaliased = field.unaliased_typ
			assert field.typ != 0
			assert field.unaliased_typ != 0
		}
		$if field.typ is int {
			if field.name == "id" {
				rows << "typ-if-id"
			}
		}
		$if field.unaliased_typ is int {
			if field.name == "alias" {
				rows << "unaliased-if-alias"
			}
		}
	}
	assert alias_unaliased == int_typ
	assert alias_typ != alias_unaliased
	println(rows.join("|"))
}
')
	assert out == 'typ-if-id|unaliased-if-alias'
}

fn test_comptime_bare_field_type_guards_are_folded() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'bare_field_type_guards', 'struct S {
	id int
	name string
	maybe ?string
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field is int {
			rows << "int:" + field.name
		}
		$if field is string {
			rows << "string:" + field.name
		}
		$if field is $option {
			rows << "option:" + field.name
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'int:id|string:name|option:maybe'
}

fn test_comptime_for_body_checks_static_code_in_metadata_if_branch() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_static_call_in_comptime_for_metadata_if', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		$if field.name == "id" {
			missing_metadata_guard_fn()
		}
	}
}
',
		'unknown function `missing_metadata_guard_fn`')
	round4_run_bad(v3_bin, 'bad_static_call_in_comptime_for_type_if', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		$if field is int {
			missing_type_guard_fn()
		}
	}
}
',
		'unknown function `missing_type_guard_fn`')
}

fn test_comptime_for_static_check_skips_untaken_metadata_if_branch() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'untaken_metadata_if_static_code', 'struct S {
	id int
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field is string {
			missing_type_branch_fn()
		} $else {
			rows << field.name
		}
		$if field.name == "missing" {
			missing_name_branch_fn()
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'id'
}

fn test_comptime_for_static_check_folds_fielddata_flags() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'fielddata_flag_static_guards', 'struct S {
pub mut:
	ptr &int
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field.is_pub && field.is_mut && field.indirections == 1 {
			rows << field.name
		} $else {
			missing_fielddata_flag_branch_fn()
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'ptr'
}

fn test_comptime_field_is_struct_for_generic_struct_instance() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'generic_struct_field_is_struct', 'struct Box[T] {
	value T
}

struct S {
	box Box[int]
	name string
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field.is_struct {
			rows << field.name + ":" + field.is_struct.str()
		} $else $if field.name == "box" {
			missing_fn()
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'box:true'
}

fn test_nested_comptime_for_uses_substituted_field_type_source() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'nested_comptime_for_field_type_source', 'struct Inner {
	count int
	label string
}

struct Outer {
	id int
	nested Inner
}

fn main() {
	mut rows := []string{}
	$for field in Outer.fields {
		$if field is $struct {
			$for sub in field.typ.fields {
				rows << field.name + "." + sub.name
			}
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'nested.count|nested.label'
}

fn test_nested_comptime_for_shadowed_loop_variables() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'shadowed_comptime_for', "struct A {
	a int
}

struct B {
	b int
	c string
}

enum Outer {
	one
}

enum Inner {
	red
	blue
}

fn main() {
	mut rows := []string{}
	$for field in A.fields {
		rows << 'outer-field:' + field.name
		$for field in B.fields {
			rows << 'inner-field:' + field.name
		}
		$for field in Inner.values {
			rows << 'mixed-value:' + int_str(field.value)
		}
	}
	$for value in Outer.values {
		rows << 'outer-value:' + value.name
		$for value in Inner.values {
			rows << 'inner-value:' + value.name
		}
	}
	println(rows.join('|'))
}
	")
	assert out == 'outer-field:a|inner-field:b|inner-field:c|mixed-value:0|mixed-value:1|outer-value:one|inner-value:red|inner-value:blue'
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

fn test_nested_comptime_for_validates_inner_members() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_nested_enum_value_member', 'struct S {
	id int
}

enum Color {
	red
}

fn main() {
	$for field in S.fields {
		$for item in Color.values {
			println(item.nmae)
		}
	}
}
	',
		'unknown EnumData member `nmae`')
}

fn test_unknown_comptime_member_in_if_guard_is_rejected() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_field_guard_member', "struct S {
	id int
}

fn main() {
	$for field in S.fields {
		$if field.nmae == 'id' {
			println(field.name)
		}
	}
}
	",
		'unknown FieldData member `nmae`')
	round4_run_bad(v3_bin, 'bad_enum_guard_member', "enum Color {
	red
}

fn main() {
	$for item in Color.values {
		$if item.nmae == 'red' {
			println(item.name)
		}
	}
}
	",
		'unknown EnumData member `nmae`')
}

fn test_comptime_member_lookalikes_in_guard_strings_are_ignored() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'guard_member_text_literal', "struct S {
	id int
}

enum Color {
	red
}

fn main() {
	mut rows := []string{}
	$for field in S.fields {
		$if field.name == 'field.nmae' {
			rows << 'bad'
		} $else {
			rows << field.name
		}
	}
	$for item in Color.values {
		$if item.name == 'item.nmae' {
			rows << 'bad'
		} $else {
			rows << item.name
		}
	}
	println(rows.join('|'))
}
	")
	assert out == 'id|red'
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

fn test_enum_values_resolve_enum_member_references() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_member_ref_values', '@[_allow_multiple_values]
enum Multi {
	base = 100
	same = .base
	plus = .base + 2
	next
}

fn main() {
	mut rows := []string{}
	$for item in Multi.values {
		rows << item.name + ":" + int_str(item.value)
	}
	println(rows.join("|"))
}
')
	assert out == 'base:100|same:100|plus:102|next:103'
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

fn test_operator_overload_struct_pseudovar() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'operator_overload_struct_pseudovar', 'struct S {
	name string
}

fn (a S) + (b S) S {
	return S{
		name: @STRUCT
	}
}

fn main() {
	res := S{} + S{}
	println(res.name)
}
')
	assert out == 'S'
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
