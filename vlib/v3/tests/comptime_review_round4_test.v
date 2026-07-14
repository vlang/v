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

fn test_comptime_match_accepts_type_group_patterns() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'comptime_match_type_group_patterns', 'struct S {}

fn classify[T](value T) string {
	_ = value
	$match T {
		$int { return "int" }
		$struct { return "struct" }
		$else { return "other" }
	}
	return "unreachable"
}

fn main() {
	println(classify(1))
	println(classify(S{}))
}
')
	assert out == 'int\nstruct'
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

fn test_comptime_variant_metadata_only_exposes_typ() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'variant_metadata_typ', 'struct A {}
struct B {}

type Value = A | B

fn variant_type(item VariantData) int {
	return item.typ
}

fn type_id(typ int) int {
	return typ
}

fn main() {
	mut count := 0
	mut type_ids_match := []string{}
	$for variant in Value.variants {
		_ = variant_type(variant)
		_ = type_id(variant.typ)
		type_ids_match << (typeof(variant.typ).idx == variant.typ).str()
		count++
	}
	println(type_ids_match.join(",") + "|" + int_str(count))
}
')
	assert out == 'true,true|2'

	round4_run_bad(v3_bin, 'bad_variant_name_member', 'struct A {}
struct B {}

type Value = A | B

fn use_name(name string) {}

fn main() {
	$for variant in Value.variants {
		use_name(variant.name)
	}
}
',
		'unknown VariantData member `name`')
	round4_run_bad(v3_bin, 'bad_variant_attrs_member', 'struct A {}
struct B {}

type Value = A | B

fn use_attrs(attrs []string) {}

fn main() {
	$for variant in Value.variants {
		use_attrs(variant.attrs)
	}
}
',
		'unknown VariantData member `attrs`')
}

fn test_comptime_variants_prefer_local_sum_over_imported_short_name() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'variant_local_sum_precedence', {
		'v.mod':         "Module { name: 'variant_local_sum_precedence' }\n"
		'other/other.v': 'module other

pub struct RemoteA {}
pub struct RemoteB {}

pub type Sum = RemoteA | RemoteB

pub const marker = 1
'
		'main.v':        'module main

import other

struct LocalA {}
struct LocalB {}

type Sum = LocalA | LocalB

fn main() {
	mut names := []string{}
	$for variant in Sum.variants {
		names << typeof(variant.typ).name
	}
	mut remote_names := []string{}
	$for variant in other.Sum.variants {
		remote_names << typeof(variant.typ).name
	}
	println(names.join(","))
	println(remote_names.join(","))
	println(int_str(other.marker))
}
'
	}, 'main.v')
	assert out == 'LocalA,LocalB\nother.RemoteA,other.RemoteB\n1'
}

fn test_comptime_field_generic_helper_infers_matching_argument() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'field_generic_helper_matching_arg', 'struct S {
	id int
	name string
}

fn add[T](label string, value T) string {
	return label + ":" + "\${value}"
}

fn main() {
	value := S{
		id: 7
		name: "v"
	}
	mut rows := []string{}
	$for field in S.fields {
		rows << add(field.name, value.$(field.name))
	}
	println(rows.join("|"))
}
')
	assert out == 'id:7|name:v'
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

fn test_enum_value_metadata_interpolation_stays_numeric() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_value_metadata_numeric_interpolation', "enum Color {
	red = 7
	blue = 42
}

fn main() {
	mut rows := []string{}
	$for item in Color.values {
		rows << '\${item.value}:\${item.value:4}'
	}
	println(rows.join('|'))
}
")
	assert out == '7:   7|42:  42'
}

fn test_enum_value_metadata_preserves_wide_backed_values() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'enum_value_metadata_wide_backed', "enum Wide as u64 {
	big = 1 << 40
	next
}

fn main() {
	mut rows := []string{}
	$for item in Wide.values {
		rows << '\${item.value}'
	}
	println(rows.join('|'))
}
")
	assert out == '1099511627776|1099511627777'
}

fn test_field_loop_specializes_multi_parameter_generic_helper() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'field_loop_multi_generic_helper', "struct Data {
	n int
	label string
}

fn pair[A, B](a A, b B) string {
	_ = a
	_ = b
	return 'ok'
}

fn main() {
	data := Data{
		n: 7
		label: 'x'
	}
	mut rows := []string{}
	$for field in Data.fields {
		rows << pair(data.$(field.name), field.name)
	}
	println(rows.join('|'))
}
")
	assert out == 'ok|ok'
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
	round4_run_bad(v3_bin, 'bad_outer_field_member_in_nested_loop', 'struct Foo {
	id int
}

struct Bar {
	name string
}

fn main() {
	$for field in Foo.fields {
		$for other in Bar.fields {
			println(field.nmae + other.name)
		}
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

fn test_non_variant_comptime_loop_variables_are_rejected_as_is_patterns() {
	v3_bin := round4_build_v3()
	round4_run_bad(v3_bin, 'bad_sum_is_field_loop_variable', 'struct A {}
struct B {}
struct Holder {
	n int
}
type Value = A | B

fn check(value Value) {
	$for field in Holder.fields {
		if value is field {}
	}
}

fn main() {}
',
		'`field` is not a variant of sum type `Value`')
	round4_run_bad(v3_bin, 'bad_sum_is_enum_value_loop_variable', 'enum E {
	one
}
struct A {}
struct B {}
type Value = A | B

fn check(value Value) {
	$for item in E.values {
		if value is item {}
	}
}

fn main() {}
',
		'`item` is not a variant of sum type `Value`')
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

fn test_cached_values_do_not_replace_reflection_guard_identifiers() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'cached_reflection_guard_identifiers', "struct Foo {
	id    int
	title string
}

fn selector_member_collision() string {
	name := 'x'
	mut found := ''
	$for field in Foo.fields {
		$if field.name == 'id' {
			found = field.name
		}
	}
	return found + ':' + name
}

fn loop_var_collision() string {
	field := 'x'
	mut found := ''
	$for field in Foo.fields {
		$if field.name == 'id' {
			found = field.name
		}
	}
	return found + ':' + field
}

fn main() {
	println(selector_member_collision())
	println(loop_var_collision())
}
")
	assert out == 'id:x\nid:x'
}

fn test_method_metadata_arrays_attrs_and_type_guards_are_materialized() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'method_metadata_materialized', "struct App {}

@[get]
@[host: 'example.com']
fn (app App) handle(value string, count int) string {
	return value + int_str(count)
}

fn (app App) empty() {}

fn attrs_text(attrs []string) string {
	return attrs.join(',')
}

fn attribute_names(attrs []VAttribute) string {
	mut names := []string{}
	for attr in attrs {
		names << attr.name
	}
	return names.join(',')
}

fn main() {
	mut rows := []string{}
	$for method in App.methods {
		if method.name == 'handle' {
			rows << method.args.len.str()
			rows << method.params.len.str()
			rows << attrs_text(method.attrs)
			rows << attribute_names(method.attributes)
		}
		$if method.typ is fn (string, int) string {
			rows << 'typed:' + method.name
		}
	}
	println(rows.join('|'))
}
")
	assert out == "2|2|get,host: 'example.com'|get,host|typed:handle"
}

fn test_field_type_membership_uses_the_selected_receiver_type() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'field_type_membership_receiver', 'struct ReflectedFields {
	id int
}

struct GuardConfig {
	id string
}

fn main() {
	cfg := GuardConfig{
		id: "text"
	}
	value := ReflectedFields{
		id: 7
	}
	mut rows := []string{}
	$for field in ReflectedFields.fields {
		$if cfg.id in [string] {
			rows << "config:string"
		} $else {
			rows << "config:wrong"
		}
		$if value.$(field.name) in [int] {
			rows << "reflected:int"
		} $else {
			rows << "reflected:wrong"
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'config:string|reflected:int'
}

fn test_optional_reflected_for_iterable_with_fallback_is_not_unwrapped_twice() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'optional_reflected_for_fallback', 'struct OptionalRows {
	values ?[]int
}

fn main() {
	rows := OptionalRows{}
	mut total := 0
	$for field in OptionalRows.fields {
		for value in rows.$(field.name) or { [4, 5] } {
			total += value
		}
	}
	println(total)
}
')
	assert out == '9'
}

fn test_quoted_attribute_preserves_structured_kind() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'quoted_attribute_kind', "@['route'; plain]
struct Tagged {}

fn describe_attribute(attr VAttribute) string {
	return attr.name + ':' + attr.kind.str() + ':' + attr.has_arg.str()
}

fn main() {
	mut rows := []string{}
	$for attr in Tagged.attributes {
		rows << describe_attribute(attr)
	}
	println(rows.join('|'))
}
")
	assert out == 'route:string:false|plain:plain:false'
}

fn test_qualified_single_letter_type_is_a_concrete_generic_argument() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'qualified_single_letter_generic_arg', {
		'v.mod':           "Module { name: 'qualified_single_letter_generic_arg' }\n"
		'letter/letter.v': 'module letter

pub struct T {
pub:
	n int
}
'
		'main.v':          'module main

import letter

struct Box[X] {
	value X
}

fn decode[X](value X) Box[X] {
	return Box[X]{
		value: value
	}
}

fn read(box Box[letter.T]) int {
	return box.value.n
}

fn main() {
	value := letter.T{
		n: 7
	}
	direct := Box[letter.T]{
		value: value
	}
	decoded := decode[letter.T](value)
	println(int_str(read(direct) + read(decoded)))
}
'
	}, 'main.v')
	assert out == '14'
}

fn test_attribute_reflection_resolves_import_alias_sources() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'attribute_reflection_import_alias', {
		'v.mod':           "Module { name: 'attribute_reflection_import_alias' }\n"
		'routes/routes.v': 'module routes

@[endpoint]
pub fn handler() {}
'
		'main.v':          'module main

import routes as r

fn attribute_name(attr VAttribute) string {
	return attr.name
}

fn main() {
	mut rows := []string{}
	$for attr in r.handler.attributes {
		rows << "direct:" + attribute_name(attr)
	}
	h := r.handler
	$for attr in h.attributes {
		rows << "local:" + attribute_name(attr)
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'direct:endpoint|local:endpoint'
}

fn test_method_type_value_and_indexed_param_type_guards_are_materialized() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'method_type_and_indexed_param_guards', 'struct MethodTypes {}

fn (m MethodTypes) empty() {
	_ = m
}

fn (m MethodTypes) text(value string, count int) {
	_ = m
	_ = value
	_ = count
}

fn main() {
	mut rows := []string{}
	$for method in MethodTypes.methods {
		if method.name == "empty" {
			rows << "empty-type:" + (method.typ == typeof[fn ()]().idx).str()
		}
		if method.name == "text" {
			rows << "text-type:" + (method.typ == typeof[fn (string, int)]().idx).str()
		}
		$if method.args[0].typ is string {
			rows << "args:" + method.name
		}
		$if method.params[1].typ is int {
			rows << "params:" + method.name
		}
		$if method.args[0].name == "value" {
			rows << "args-name:" + method.name
		}
		$if method.params[1].name == "count" {
			rows << "params-name:" + method.name
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'empty-type:true|text-type:true|args:text|params:text|args-name:text|params-name:text'
}

fn test_comptime_pseudo_value_is_not_resolved_as_cached_local() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'pseudo_value_cached_local', "fn main() {
	c := 'js'
	_ = c
	\$if @BACKEND == 'c' {
		println('ok')
	} \$else {
		println('wrong')
	}
}
")
	assert out == 'ok'
}

fn test_comptime_define_builtin_is_not_resolved_as_cached_local() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'define_builtin_cached_local', "fn main() {
	d := false
	_ = d
	\$if \$d('v3_parser_cached_d_regression', true) {
		println('ok')
	} \$else {
		println('wrong')
	}
}
")
	assert out == 'ok'
}

fn test_fn_literal_comptime_values_do_not_leak_to_outer_scope() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'fn_literal_comptime_value_scope', "fn main() {
	f := fn () {
		x := 'inside'
		_ = x
	}
	_ = f
	\$if x == 'inside' {
		println('wrong')
	} \$else {
		println('ok')
	}
}
")
	assert out == 'ok'
}

fn test_cached_char_literals_match_scanner_values() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'cached_char_literals', "const cached_char = `a`

fn main() {
	local_char := `b`
	mut rows := []string{}
	\$if cached_char == `a` {
		rows << 'const-if'
	} \$else {
		rows << 'wrong-const'
	}
	\$match local_char {
		`b` { rows << 'local-match' }
		\$else { rows << 'wrong-local' }
	}
	println(rows.join('|'))
}
")
	assert out == 'const-if|local-match'
}

fn test_bare_method_data_and_imported_return_type_are_materialized() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'bare_method_data_imported_return', {
		'v.mod':     "Module { name: 'bare_method_data_imported_return' }\n"
		'pkg/pkg.v': 'module pkg

pub struct Foo {}

pub struct Ret {}

pub fn (f Foo) make(value Ret) Ret {
	_ = f
	return value
}
'
		'main.v':    'module main

import pkg

fn main() {
	mut items := []FunctionData{}
	mut rows := []string{}
	$for method in pkg.Foo.methods {
		items << method
		rows << typeof(method).name
		rows << method.name
		rows << (method.return_type == typeof[pkg.Ret]().idx).str()
		rows << (method.typ == typeof[fn (pkg.Ret) pkg.Ret]().idx).str()
	}
	rows << items[0].name
	rows << (items[0].return_type == typeof[pkg.Ret]().idx).str()
	rows << (items[0].args[0].typ == typeof[pkg.Ret]().idx).str()
	rows << (items[0].typ == typeof[fn (pkg.Ret) pkg.Ret]().idx).str()
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'FunctionData|make|true|true|make|true|true|true'
}

fn test_imported_function_param_types_use_declaring_module() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'imported_function_param_module', {
		'v.mod':     "Module { name: 'imported_function_param_module' }\n"
		'pkg/pkg.v': 'module pkg

pub struct Item {}

pub fn consume(item Item) {
	_ = item
}
'
		'main.v':    'module main

import pkg

fn main() {
	mut params := []FunctionParam{}
	mut rows := []string{}
	$for param in pkg.consume.params {
		params << param
		rows << param.name
		rows << (param.typ == typeof[pkg.Item]().idx).str()
	}
	rows << (params[0].typ == typeof[pkg.Item]().idx).str()
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'item|true|true'
}

fn test_nested_param_and_attribute_reflection_respects_shadowing() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'nested_param_attribute_shadowing', "@[outer]
struct OuterAttrs {}

@[inner]
struct InnerAttrs {}

fn outer(first string) {
	_ = first
}

fn inner(second int) {
	_ = second
}

fn main() {
	mut rows := []string{}
	\$for param in outer.params {
		rows << 'outer-param:' + param.name
		\$for param in inner.params {
			rows << 'inner-param:' + param.name
		}
	}
	\$for attr in OuterAttrs.attributes {
		rows << 'outer-attr:' + attr.name
		\$for attr in InnerAttrs.attributes {
			rows << 'inner-attr:' + attr.name
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'outer-param:first|inner-param:second|outer-attr:outer|inner-attr:inner'
}

fn test_nested_method_attribute_guard_preserves_inner_selector() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'nested_method_attribute_guard', "struct App {}

@[get]
fn (a App) index() {
	_ = a
}

@[post]
fn (a App) submit() {
	_ = a
}

fn main() {
	mut rows := []string{}
	\$for method in App.methods {
		\$for attr in method.attributes {
			\$if method.name == 'index' && attr.name == 'get' {
				rows << method.name + ':' + attr.name
			}
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'index:get'
}

fn test_nested_attribute_method_guard_preserves_inner_selector() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'nested_attribute_method_guard', "@[route]
struct Routes {}

struct App {}

fn (a App) index() {
	_ = a
}

fn (a App) submit() {
	_ = a
}

fn main() {
	mut rows := []string{}
	\$for attr in Routes.attributes {
		\$for method in App.methods {
			\$if attr.name == 'route' && method.name == 'index' {
				rows << attr.name + ':' + method.name
			}
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'route:index'
}

fn test_nested_param_field_guard_preserves_inner_selector() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'nested_param_field_guard', "struct Row {
	id int
	name string
}

fn consume(x string, y int) {
	_ = x
	_ = y
}

fn main() {
	mut rows := []string{}
	\$for param in consume.params {
		\$for field in Row.fields {
			\$if param.name == 'x' && field.name == 'id' {
				rows << param.name + ':' + field.name
			}
		}
	}
	println(rows.join('|'))
}
")
	assert out == 'x:id'
}

fn test_comptime_string_literal_text_is_not_resolved_as_cached_local() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'comptime_string_literal_cached_local', "fn main() {
	x := 'y'
	_ = x
	\$if 'x' == 'y' {
		println('wrong')
	} \$else {
		println('ok')
	}
}
")
	assert out == 'ok'
}

fn test_params_reflection_resolves_import_alias_source() {
	v3_bin := round4_build_v3()
	out := round4_run_good_project(v3_bin, 'params_reflection_import_alias', {
		'v.mod':     "Module { name: 'params_reflection_import_alias' }\n"
		'pkg/pkg.v': 'module pkg

pub fn consume(item string) {
	_ = item
}
'
		'main.v':    'module main

import pkg as p

fn main() {
	mut rows := []string{}
	$for param in p.consume.params {
		rows << param.name
	}
	println(rows.join("|"))
}
'
	}, 'main.v')
	assert out == 'item'
}

fn test_nested_method_reflection_respects_shadowed_loop_variable() {
	v3_bin := round4_build_v3()
	out := round4_run_good(v3_bin, 'nested_method_shadowing', 'struct OuterMethods {}

fn (o OuterMethods) outer(value string) string {
	_ = o
	return "outer:" + value
}

struct InnerMethods {}

fn (i InnerMethods) alpha(value string) string {
	_ = i
	return "alpha:" + value
}

fn (i InnerMethods) beta(value string) string {
	_ = i
	return "beta:" + value
}

fn main() {
	inner := InnerMethods{}
	mut rows := []string{}
	$for method in OuterMethods.methods {
		rows << "outer:" + method.name
		$for method in InnerMethods.methods {
			$if method.args[0].typ is string {
				rows << method.name + "=" + inner.$method(method.name)
			}
		}
	}
	println(rows.join("|"))
}
')
	assert out == 'outer:outer|alpha=alpha:alpha|beta=beta:beta'
}
