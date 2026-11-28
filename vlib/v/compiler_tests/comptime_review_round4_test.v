import json2
import os
import v.cmdexec
import v.tempname

const v3_bin = @VEXE
const round4_fixture_dir = os.join_path(os.vtmp_dir(), 'v3_round4_${tempname.unique_token()}')

fn testsuite_begin() {
	os.mkdir(round4_fixture_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(round4_fixture_dir) or {}
}

fn run_round4_compiler(compiler string, args []string) os.Result {
	return cmdexec.run_in_merged_with_env(compiler, args, '', {
		'VFLAGS': ''
	})
}

fn round4_tmp_path(name string) string {
	return os.join_path(round4_fixture_dir, 'v3_round4_${name}')
}

fn round4_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn round4_run_good(compiler string, name string, src string) string {
	src_path := '${round4_tmp_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	bin_path := round4_tmp_path(name)
	compile := run_round4_compiler(compiler, [src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
	return round4_run_binary(name, bin_path)
}

fn round4_run_binary(name string, bin_path string, args ...string) string {
	run := os.exec([bin_path, ...args])
	assert run.exit_code == 0, '${name}: ${run.output}'
	return run.output.trim_space()
}

fn round4_run_bad(compiler string, name string, src string, expected string) {
	src_path := '${round4_tmp_path(name)}.v'
	os.write_file(src_path, src) or { panic(err) }
	bin_path := round4_tmp_path(name)
	compile := run_round4_compiler(compiler, [src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code != 0, '${name}: expected failure, got success\n${compile.output}'
	assert compile.output.contains(expected), '${name}: expected `${expected}` in\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
}

struct Round4Rejection {
	name     string
	source   string
	expected string
}

struct Round4Diagnostic {
	file     string
	severity string
	message  string
}

fn round4_check_rejections(name string, cases []Round4Rejection) {
	root := round4_tmp_path(name)
	mut imports := []string{cap: cases.len}
	for case in cases {
		imports << 'import ${case.name} as _'
		round4_write_file(root, os.join_path(case.name, '${case.name}.v'), 'module ${case.name}\n\n' +
			case.source)
	}
	round4_write_file(root, 'main.v', 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n')
	result := run_round4_compiler(v3_bin, [root, '-json-errors', '-b', 'c', '-o',
		os.join_path(root, 'program')])
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	mut errors := map[string][]string{}
	for line in result.output.split_into_lines() {
		if !line.starts_with('{') {
			continue
		}
		diagnostic := json2.decode[Round4Diagnostic](line) or { panic(err) }
		if diagnostic.severity == 'error' {
			errors[os.file_name(diagnostic.file)] << diagnostic.message
		}
	}
	for case in cases {
		assert errors['${case.name}.v'].any(it.contains(case.expected)), '${case.name}: expected `${case.expected}` in\n${result.output}'
	}
}

fn round4_run_good_project(compiler string, name string, files map[string]string, input string) string {
	bin_path := round4_compile_good_project(compiler, name, files, input)
	return round4_run_binary(name, bin_path)
}

fn round4_compile_good_project(compiler string, name string, files map[string]string, input string) string {
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
	compile := run_round4_compiler(compiler, [input_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: ${compile.output}'
	return bin_path
}

struct Round4RuntimeCase {
	name     string
	source   string
	expected string
}

fn round4_check_runtime(name string, cases []Round4RuntimeCase) {
	mut files := map[string]string{}
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		source := 'module ${module_name}\n\n' + case.source
		files['${module_name}/${case.name}.v'] = source.replace_once('\nfn main()', '\npub fn exercise()')
	}
	files['main.v'] = "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n"
	round4_check_runtime_outputs(name, cases, files)
}

fn round4_check_main_runtime(name string, cases []Round4RuntimeCase, extra_files map[string]string) {
	// Keep each fixture in its own file so that its import aliases stay local.
	mut files := extra_files.clone()
	mut calls := []string{cap: cases.len}
	for case in cases {
		files['${case.name}.v'] = case.source.replace_once('\nfn main()', '\nfn ${case.name}()')
		calls << "\t\t'${case.name}' { ${case.name}() }"
	}
	files['main.v'] = "module main\n\nimport os\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n"
	round4_check_runtime_outputs(name, cases, files)
}

fn round4_check_runtime_outputs(name string, cases []Round4RuntimeCase, files map[string]string) {
	bin_path := round4_compile_good_project(v3_bin, name, files, '')
	for case in cases {
		out := round4_run_binary(case.name, bin_path, case.name)
		assert out == case.expected, '${case.name}: expected ${case.expected}\n${out}'
	}
}

fn test_field_metadata_and_type_shapes() {
	round4_check_runtime('field_metadata_and_type_shapes', [
		Round4RuntimeCase{
			name:     'field_metadata'
			source:   "struct S {
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
	"
			expected: "private:false:false:|public:false:true:|mutable:true:false:|pub_mut:true:true:|attr:true:true:json: 'wire'"
		},
		Round4RuntimeCase{
			name:     'bare_fielddata'
			source:   "type WireName = string

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
"
			expected: "id:true:true:true:true:false:|name:true:true:false:false:true:json: 'wire'|count:2"
		},
		Round4RuntimeCase{
			name:     'field_unaliased_option'
			source:   "type Alias = string

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
"
			expected: 'opt_alias:option:false|opt_string:option:true'
		},
		Round4RuntimeCase{
			name:     'generic_struct_field_is_struct'
			source:   'struct Box[T] {
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
'
			expected: 'box:true'
		},
		Round4RuntimeCase{
			name:     'field_type_membership_receiver'
			source:   'struct ReflectedFields {
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
'
			expected: 'config:string|reflected:int'
		},
		Round4RuntimeCase{'variant_metadata_typ', 'struct A {}
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
', 'true,true|2'},
	])
}

fn test_comptime_variant_metadata_only_exposes_typ() {
	round4_check_rejections('variant_and_selected_method_metadata_rejections', [
		Round4Rejection{'bad_variant_name_member', 'struct A {}
struct B {}

type Value = A | B

fn use_name(name string) {}

fn main() {
	$for variant in Value.variants {
		use_name(variant.name)
	}
}
', 'unknown VariantData member `name`'},
		Round4Rejection{'bad_variant_attrs_member', 'struct A {}
struct B {}

type Value = A | B

fn use_attrs(attrs []string) {}

fn main() {
	$for variant in Value.variants {
		use_attrs(variant.attrs)
	}
}
', 'unknown VariantData member `attrs`'},
		Round4Rejection{'bad_sum_is_field_loop_variable', 'struct A {}
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
', '`bad_sum_is_field_loop_variable.Value` has no variant `bad_sum_is_field_loop_variable.field`'},
		Round4Rejection{'bad_sum_is_enum_value_loop_variable', 'enum EnumKind {
	one
}
struct A {}
struct B {}
type Value = A | B

fn check(value Value) {
	$for item in EnumKind.values {
		if value is item {}
	}
}

fn main() {}
', '`bad_sum_is_enum_value_loop_variable.Value` has no variant `bad_sum_is_enum_value_loop_variable.item`'},
		Round4Rejection{'method_metadata_location_selected_guard', 'struct Located {}

fn (item Located) run() {
	_ = item
}

fn main() {
	$for method in Located.methods {
		$if method.location != "" {
			missing_selected_method_location_fn()
		}
	}
}
', 'unknown function: missing_selected_method_location_fn'},
		Round4Rejection{'generic_receiver_selected_name_branch', 'struct Box[T] {}

fn (b Box[T]) get(value T) T {
	_ = b
	return value
}

fn main() {
	box := Box[int]{}
	$for method in box.methods {
		$if method.name == "get" {
			missing_generic_method_name_branch()
		}
	}
}
', 'unknown function: missing_generic_method_name_branch'},
		Round4Rejection{'generic_receiver_selected_param_branch', 'struct Box[T] {}

fn (b Box[T]) get(value T) T {
	_ = b
	return value
}

fn main() {
	box := Box[int]{}
	$for method in box.methods {
		$if method.args[0].typ is int {
			missing_generic_method_param_branch()
		}
	}
}
', 'unknown function: missing_generic_method_param_branch'},
	])
}

fn test_comptime_variants_prefer_local_sum_over_imported_short_name() {
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

fn test_reflected_field_helpers_and_type_guards() {
	round4_check_runtime('reflected_field_helpers_and_type_guards', [
		Round4RuntimeCase{
			name:     'field_generic_helper_matching_arg'
			source:   'struct S {
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
'
			expected: 'id:7|name:v'
		},
		Round4RuntimeCase{
			name:     'field_loop_multi_generic_helper'
			source:   "struct Data {
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
"
			expected: 'ok|ok'
		},
		Round4RuntimeCase{
			name:     'field_type_selectors_are_type_ids'
			source:   'type MyInt = int

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
'
			expected: 'typ-if-id|unaliased-if-alias'
		},
		Round4RuntimeCase{
			name:     'bare_field_type_guards'
			source:   'struct S {
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
'
			expected: 'int:id|string:name|option:maybe'
		},
		Round4RuntimeCase{'typeof_dynamic_field_selector_in_comptime_for', "struct Foo {
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
", 'int|string'},
	])
}

fn test_comptime_loop_sources() {
	round4_check_runtime('comptime_loop_sources', [
		Round4RuntimeCase{
			name:     'value_field_source'
			source:   "struct Inner {
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
"
			expected: 'id|name|nested|id,name,nested|count'
		},
		Round4RuntimeCase{
			name:     'comptime_for_alias_chain'
			source:   "enum Color {
	red
	blue
}

type Shade = Color
type Tint = Shade

struct Sample {
	id int
}

type StructAlias = Sample
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
"
			expected: 'value:red|value:blue|field:id'
		},
		Round4RuntimeCase{
			name:     'nested_comptime_for_field_type_source'
			source:   'struct Inner {
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
'
			expected: 'nested.count|nested.label'
		},
		Round4RuntimeCase{
			name:     'shadowed_comptime_for'
			source:   "struct A {
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
	"
			expected: 'outer-field:a|inner-field:b|inner-field:c|mixed-value:0|mixed-value:1|outer-value:one|inner-value:red|inner-value:blue'
		},
		Round4RuntimeCase{'optional_reflected_for_fallback', 'struct OptionalRows {
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
', '9'},
	])
}

fn test_comptime_import_alias_lookups() {
	round4_check_main_runtime('comptime_import_alias_lookups', [
		Round4RuntimeCase{'import_alias_values', 'module main

import colors as palette

fn main() {
	mut rows := []string{}
	$for item in palette.Shade.values {
		rows << item.name
	}
	println(rows.join("|"))
}
', 'red|blue'},
		Round4RuntimeCase{'nested_value_resolved_field_type', 'module main

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
', 'count'},
		Round4RuntimeCase{'selective_import_comptime_for_static_pruning', 'module main

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
', 'id'},
		Round4RuntimeCase{'attribute_reflection_import_alias', 'module main

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
', 'direct:endpoint|local:endpoint'},
		Round4RuntimeCase{'attribute_reflection_selective_import', 'module main

import routes { Controller, handler }

fn main() {
	mut rows := []string{}
	$for attr in handler.attributes {
		rows << "direct:" + attr.name
	}
	h := handler
	$for attr in h.attributes {
		rows << "local:" + attr.name
	}
	$for attr in Controller.attributes {
		rows << "type:" + attr.name
	}
	println(rows.join("|"))
}
', 'direct:endpoint|local:endpoint|type:group'},
		Round4RuntimeCase{'params_reflection_import_alias', 'module main

import pkg as p

fn main() {
	mut rows := []string{}
	$for param in p.consume.params {
		rows << param.name
	}
	println(rows.join("|"))
}
', 'item'},
		Round4RuntimeCase{'params_reflection_selective_import', 'module main

import pkg { consume }

fn main() {
	mut rows := []string{}
	$for param in consume.params {
		rows << "direct:" + param.name
	}
	h := consume
	$for param in h.params {
		rows << "local:" + param.name
	}
	println(rows.join("|"))
}
', 'direct:item|local:item'},
	], {
		'v.mod':           "Module { name: 'comptime_import_alias_lookups' }\n"
		'colors/colors.v': 'module colors

pub enum Shade {
	red
	blue
}
'
		'pkg/pkg.v':       'module pkg

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

pub struct S {
pub:
	id int
}

pub fn consume(item string) {
	_ = item
}
'
		'routes/routes.v': 'module routes

@[endpoint]
pub fn handler() {}

@[group]
pub struct Controller {}
'
	})
}

fn test_comptime_field_typ_guards_use_declaring_module_type() {
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

fn test_comptime_for_selective_import_alias_source_unrolls() {
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

fn test_non_generic_call_matching_generic_short_name_does_not_skip_comptime_for() {
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

fn test_field_source_and_metadata_pruning() {
	round4_check_runtime('field_source_and_metadata_pruning', [
		Round4RuntimeCase{
			name:     'value_source_comptime_for_static_pruning'
			source:   "struct Inner {
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
"
			expected: 'id'
		},
		Round4RuntimeCase{
			name:     'shadowed_comptime_for_nested_source'
			source:   "struct Inner {
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
"
			expected: 'name'
		},
		Round4RuntimeCase{
			name:     'untaken_metadata_if_static_code'
			source:   'struct S {
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
'
			expected: 'id'
		},
		Round4RuntimeCase{
			name:     'fielddata_flag_static_guards'
			source:   'struct S {
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
'
			expected: 'ptr'
		},
		Round4RuntimeCase{'unreachable_generic_call_in_comptime_for', "struct S {
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
", 'id|red'},
		Round4RuntimeCase{'cached_reflection_guard_identifiers', "struct Foo {
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
", 'id:x\nid:x'},
	])
}

fn test_field_and_enum_metadata_argument_and_member_rejections() {
	round4_check_rejections('field_and_enum_metadata_argument_and_member_rejections', [
		Round4Rejection{'bad_metadata_call_arg_type', 'struct S {
	id int
}

fn takes_int(x int) {}

fn main() {
	$for field in S.fields {
		takes_int(field.name)
	}
}
', 'cannot use `string` as argument 1 to `takes_int`; expected `int`'},
		Round4Rejection{'bad_enum_value_metadata_arg_type', 'enum Color {
	red
}

fn takes_string(value string) {}

fn main() {
	$for item in Color.values {
		takes_string(item.value)
	}
}
', 'cannot use `i64` as argument 1 to `takes_string`; expected `string`'},
		Round4Rejection{'bad_field_member', 'struct S {
	name int
}

fn main() {
	$for field in S.fields {
		println(field.nmae)
	}
}
	', 'unknown FieldData member `nmae`'},
		Round4Rejection{'bad_outer_field_member_in_nested_loop', 'struct Foo {
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
	', 'unknown FieldData member `nmae`'},
		Round4Rejection{'bad_field_is_interface', 'struct S {
		name int
	}

fn main() {
	$for field in S.fields {
		println(field.is_interface)
	}
}
', 'unknown FieldData member `is_interface`'},
		Round4Rejection{'bad_field_is_function', 'struct S {
		name int
	}

fn main() {
	$for field in S.fields {
		println(field.is_function)
	}
}
', 'unknown FieldData member `is_function`'},
		Round4Rejection{'bad_enum_value_member', 'enum Color {
	red
}

fn main() {
	$for item in Color.values {
		println(item.nmae)
	}
}
	', 'unknown EnumData member `nmae`'},
		Round4Rejection{'bad_nested_enum_value_member', 'struct S {
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
	', 'unknown EnumData member `nmae`'},
	])
}

fn test_deferred_reflection_body_and_selected_branch_rejections() {
	round4_check_rejections('deferred_reflection_body_and_selected_branch_rejections', [
		Round4Rejection{'bad_method_metadata_call_arg_type', 'struct App {}

fn (app App) run() {
	_ = app
}

fn takes_int(value int) {}

fn main() {
	$for method in App.methods {
		takes_int(method.name)
	}
}
', 'cannot use `string` as argument 1 to `takes_int`; expected `int`'},
		Round4Rejection{'bad_param_metadata_call_arg_type', 'fn consume(value int) {
	_ = value
}

fn takes_string(value string) {}

fn main() {
	$for param in consume.params {
		takes_string(param.typ)
	}
}
', 'cannot use `int` as argument 1 to `takes_string`; expected `string`'},
		Round4Rejection{'bad_attribute_metadata_call_arg_type', "@[route: '/']
struct App {}

fn takes_bool(value bool) {}

fn main() {
	\$for attr in App.attributes {
		takes_bool(attr.arg)
	}
}
", 'cannot use `string` as argument 1 to `takes_bool`; expected `bool`'},
		Round4Rejection{'method_loop_static_body_error', 'struct App {}

fn (app App) run(value int) {
	_ = app
	_ = value
}

fn main() {
	$for method in App.methods {
		missing_method_loop_fn()
	}
}
', 'unknown function: missing_method_loop_fn'},
		Round4Rejection{'param_loop_static_body_error', 'fn consume(value int) {
	_ = value
}

fn main() {
	$for param in consume.params {
		missing_param_loop_fn()
	}
}
', 'unknown function: missing_param_loop_fn'},
		Round4Rejection{'attribute_loop_static_body_error', '@[route]
struct App {}

fn main() {
	$for attr in App.attributes {
		missing_attribute_loop_fn()
	}
}
', 'unknown function: missing_attribute_loop_fn'},
		Round4Rejection{'method_loop_selected_branch_error', 'struct App {}

fn (app App) run() {
	_ = app
}

fn main() {
	$for method in App.methods {
		$if method.name == "run" {
			missing_selected_method_fn()
		}
	}
}
', 'unknown function: missing_selected_method_fn'},
		Round4Rejection{'method_loop_selected_missing_param_branch_error', 'struct App {}

fn (app App) run() {
	_ = app
}

fn main() {
	$for method in App.methods {
		$if method.args[1].name == "" {
			missing_selected_method_param_slot_fn()
		}
	}
}
', 'unknown function: missing_selected_method_param_slot_fn'},
		Round4Rejection{'param_loop_selected_branch_error', 'fn consume(value int) {
	_ = value
}

fn main() {
	$for param in consume.params {
		$if param.name == "value" {
			missing_selected_param_fn()
		}
	}
}
', 'unknown function: missing_selected_param_fn'},
		Round4Rejection{'attribute_loop_selected_branch_error', '@[route]
struct App {}

fn main() {
	$for attr in App.attributes {
		$if attr.name == "route" {
			missing_selected_attribute_fn()
		}
	}
}
', 'unknown function: missing_selected_attribute_fn'},
		Round4Rejection{'method_loop_selected_is_pub_branch_error', 'struct App {}

pub fn (app App) run() {
	_ = app
}

fn main() {
	$for method in App.methods {
		$if method.is_pub {
			missing_selected_public_method_fn()
		}
	}
}
', 'unknown function: missing_selected_public_method_fn'},
		Round4Rejection{'param_loop_selected_typ_branch_error', 'fn consume(value string) {
	_ = value
}

fn main() {
	$for param in consume.params {
		$if param.typ is string {
			missing_selected_string_param_fn()
		}
	}
}
', 'unknown function: missing_selected_string_param_fn'},
		Round4Rejection{'attribute_loop_selected_arg_branch_error', "@[route: '/path']
struct App {}

fn main() {
	\$for attr in App.attributes {
		\$if attr.arg == '/path' {
			missing_selected_attribute_arg_fn()
		}
	}
}
", 'unknown function: missing_selected_attribute_arg_fn'},
	])
}

fn test_comptime_declaring_module_lookups() {
	round4_check_main_runtime('comptime_declaring_module_lookups', [
		Round4RuntimeCase{'cross_module_composite_field_typ_guard', 'module main

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
', 'items'},
		Round4RuntimeCase{'enum_const_decl_module', 'module main

import colors

fn main() {
	println(colors.values())
}
', 'dark:6|light:7'},
		Round4RuntimeCase{'bare_method_data_imported_return', 'module main

import pkg

fn main() {
	mut items := []FunctionData{}
	mut rows := []string{}
	$for method in pkg.Foo.methods {
		items << method
		rows << typeof(method).name
		rows << method.name
		$if method.return_type is pkg.Ret {
			rows << "return-guard"
		} $else {
			missing_imported_method_return_guard()
		}
		$if method.params[0].typ is pkg.Ret {
			rows << "param-guard"
		} $else {
			missing_imported_method_param_guard()
		}
		rows << (method.return_type == typeof[pkg.Ret]().idx).str()
		rows << (method.typ == typeof[fn (pkg.Ret) pkg.Ret]().idx).str()
	}
	rows << items[0].name
	rows << (items[0].return_type == typeof[pkg.Ret]().idx).str()
	rows << (items[0].args[0].typ == typeof[pkg.Ret]().idx).str()
	rows << (items[0].typ == typeof[fn (pkg.Ret) pkg.Ret]().idx).str()
	println(rows.join("|"))
}
', 'FunctionData|make|return-guard|param-guard|true|true|make|true|true|true'},
		Round4RuntimeCase{'imported_function_param_module', 'module main

import pkg

fn main() {
	mut params := []FunctionParam{}
	mut rows := []string{}
	h := pkg.consume
	$for param in h.params {
		params << param
		rows << param.name
		$if param.typ is pkg.Item {
			rows << "guard"
		} $else {
			missing_imported_function_param_guard()
		}
		rows << (param.typ == typeof[pkg.Item]().idx).str()
	}
	rows << (params[0].typ == typeof[pkg.Item]().idx).str()
	println(rows.join("|"))
}
', 'item|guard|true|true'},
		Round4RuntimeCase{'local_reflection_source_module_qualification', 'module main

import localmod

@[main_attr]
fn target(foreign int) {
	_ = foreign
}

fn main() {
	println(localmod.inspect())
}
', 'param:local|attr:module_attr'},
	], {
		'v.mod':               "Module { name: 'comptime_declaring_module_lookups' }\n"
		'pkg/pkg.v':           'module pkg

pub struct Inner {
pub:
	id int
}

pub struct Outer {
pub:
	items map[string]Inner
}

pub struct Foo {}

pub struct Ret {}

pub fn (f Foo) make(value Ret) Ret {
	_ = f
	return value
}

pub struct Item {}

pub fn consume(item Item) {
	_ = item
}
'
		'colors/colors.v':     'module colors

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
		'localmod/localmod.v': 'module localmod

@[module_attr]
pub fn target(local string) {
	_ = local
}

pub fn inspect() string {
	mut rows := []string{}
	$for param in target.params {
		rows << "param:" + param.name
	}
	$for attr in target.attributes {
		rows << "attr:" + attr.name
	}
	return rows.join("|")
}
'
	})
}

fn test_enum_metadata_calls_and_power_pruning() {
	round4_check_runtime('enum_metadata_calls_and_power_pruning', [
		Round4RuntimeCase{'enum_value_metadata_call_args', "enum Color {
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
", 'red:7'},
		Round4RuntimeCase{'enum_power_metadata_static_checker', 'enum Powered {
	a = 2 ** 3
}

fn main() {
	$for item in Powered.values {
		$if item.value == 8 {
			println(item.name)
		} $else {
			missing_from_inactive_enum_power_branch()
		}
	}
}
', 'a'},
	])
}

fn test_enum_metadata_folding_and_numeric_values() {
	round4_check_runtime('enum_metadata_folding_and_numeric_values', [
		Round4RuntimeCase{
			name:     'enum_value_metadata_if_prunes_else'
			source:   "enum Color {
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
"
			expected: 'only'
		},
		Round4RuntimeCase{
			name:     'enum_value_metadata_unsigned_shift'
			source:   "enum Shifted {
	a = 8 >>> 1
}

fn main() {
	mut rows := []string{}
	$for item in Shifted.values {
		rows << i64(item.value).str()
	}
	println(rows.join('|'))
}
"
			expected: '4'
		},
		Round4RuntimeCase{
			name:     'enum_value_metadata_power'
			source:   "enum Powered {
	a = 2 ** 3
	b
}

fn main() {
	mut rows := []string{}
	$for item in Powered.values {
		rows << item.name + ':' + i64(item.value).str()
	}
	println(rows.join('|'))
}
"
			expected: 'a:8|b:9'
		},
		// `item.value` is the enum value itself (as in V1), so it interpolates as the
		// field name; its numeric form, including width formatting, needs a cast.
		Round4RuntimeCase{
			name:     'enum_value_metadata_numeric_interpolation'
			source:   "enum Color {
	red = 7
	blue = 42
}

fn main() {
	mut rows := []string{}
	$for item in Color.values {
		rows << '\${item.value}=\${i64(item.value)}:\${i64(item.value):4}'
	}
	println(rows.join('|'))
}
"
			expected: 'red=7:   7|blue=42:  42'
		},
		Round4RuntimeCase{
			name:     'enum_value_metadata_wide_backed'
			source:   "enum Wide as u64 {
	big = u64(1) << 40
	next
}

fn main() {
	mut rows := []string{}
	$for item in Wide.values {
		rows << '\${u64(item.value)}'
	}
	println(rows.join('|'))
}
"
			expected: '1099511627776|1099511627777'
		},
	])
}

fn test_cross_module_dynamic_field_selector_uses_qualified_type() {
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

fn test_comptime_for_static_body_and_metadata_guard_rejections() {
	round4_check_rejections('comptime_for_static_body_and_metadata_guard_rejections', [
		Round4Rejection{'bad_static_code_in_comptime_for', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		missing_fn()
	}
}
', 'unknown function: missing_fn'},
		Round4Rejection{'bad_static_code_in_comptime_for_if_branch', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		if field.name == "id" {
			missing_branch_fn()
		}
	}
}
', 'unknown function: missing_branch_fn'},
		Round4Rejection{'bad_static_call_in_comptime_for_mixed_expr', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		missing_fn(field.name)
	}
}
', 'unknown function: missing_fn'},
		Round4Rejection{'bad_static_call_in_comptime_for_metadata_if', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		$if field.name == "id" {
			missing_metadata_guard_fn()
		}
	}
}
', 'unknown function: missing_metadata_guard_fn'},
		Round4Rejection{'bad_static_call_in_comptime_for_type_if', 'struct S {
	id int
}

fn main() {
	$for field in S.fields {
		$if field is int {
			missing_type_guard_fn()
		}
	}
}
', 'unknown function: missing_type_guard_fn'},
		Round4Rejection{'bad_field_guard_member', "struct S {
	id int
}

fn main() {
	$for field in S.fields {
		$if field.nmae == 'id' {
			println(field.name)
		}
	}
}
	", 'unknown FieldData member `nmae`'},
		Round4Rejection{'bad_enum_guard_member', "enum Color {
	red
}

fn main() {
	$for item in Color.values {
		$if item.nmae == 'red' {
			println(item.name)
		}
	}
}
	", 'unknown EnumData member `nmae`'},
	])
}

fn test_reflected_enum_members_and_flags() {
	round4_check_runtime('reflected_enum_members_and_flags', [
		Round4RuntimeCase{
			name:     'enum_member_ref_values'
			source:   '@[_allow_multiple_values]
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
'
			expected: 'base:100|same:100|plus:102|next:103'
		},
		Round4RuntimeCase{
			name:     'enum_flag_attrs'
			source:   "@[flag]
enum Perm {
	read  @[json: 'r'; primary]
	write
	execute = 3
}

fn main() {
	mut rows := []string{}
	mut bare := []EnumData{}
	$for item in Perm.values {
		rows << item.name + ':' + i64(item.value).str() + ':' + item.attrs.join(',')
		bare << item
	}
	rows << bare[0].name + ':' + bare[0].value.str() + ':' + bare[0].attrs.join(',')
	println(rows.join('|'))
}
"
			expected: "read:1:json: 'r',primary|write:2:|execute:8:|read:1:json: 'r',primary"
		},
	])
}

fn test_selective_imported_enum_alias_resolves_underlying_enum() {
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

fn test_shared_and_voidptr_type_groups() {
	round4_check_runtime('shared_and_voidptr_type_groups', [
		Round4RuntimeCase{'shared_type_group', "struct Counter {}

fn main() {
	$if shared Counter is $shared {
		println('shared')
	} $else {
		println(unknown_symbol)
	}
}
", 'shared'},
		Round4RuntimeCase{'voidptr_type_group', "fn main() {
	$if voidptr is $voidptr {
		println('voidptr')
	} $else {
		println(unknown_symbol)
	}
}
", 'voidptr'},
		Round4RuntimeCase{'comptime_match_type_group_patterns', 'struct S {}

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
', 'int\nstruct'},
	])
}

fn test_reflected_method_signatures_and_arity() {
	round4_check_runtime('reflected_method_signatures_and_arity', [
		Round4RuntimeCase{
			name:     'method_metadata_materialized'
			source:   "struct App {}

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
"
			expected: "2|2|get,host: 'example.com'|get,host|typed:handle"
		},
		Round4RuntimeCase{
			name:     'method_type_and_indexed_param_guards'
			source:   'struct MethodTypes {}

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
'
			expected: 'empty-type:true|text-type:true|args:text|params:text|args-name:text|params-name:text'
		},
		Round4RuntimeCase{
			name:     'method_parameter_count_guards'
			source:   'struct Counted {}

fn (item Counted) empty() {
	_ = item
}

fn (item Counted) one(value int) {
	_ = item
	_ = value
}

fn main() {
	mut rows := []string{}
	$for method in Counted.methods {
		$if method.params.len == 0 {
			rows << "params:" + method.name
		}
		$if method.args.len == 1 {
			rows << "args:" + method.name
		}
	}
	println(rows.join("|"))
}
'
			expected: 'params:empty|args:one'
		},
		Round4RuntimeCase{
			name:     'method_call_spread_and_variadic_arity'
			source:   'struct Calls {}

fn (calls Calls) fixed(value string, count int) string {
	_ = calls
	return value.repeat(count)
}

fn (calls Calls) variadic(values ...string) string {
	_ = calls
	return values.join("")
}

fn main() {
	calls := Calls{}
	mut rows := []string{}
	$for method in Calls.methods {
		$if method.name == "fixed" {
			args := ["x", "3"]
			rows << calls.$method(...args)
		}
		$if method.name == "variadic" {
			rows << calls.$method("a", "b")
		}
	}
	println(rows.join("|"))
}
'
			expected: 'xxx|ab'
		},
		Round4RuntimeCase{'generic_receiver_method_metadata', 'struct Box[T] {
	value T
}

fn (b Box[T]) get(value T) T {
	_ = b
	return value
}

fn reflected_method_metadata[T]() string {
	mut rows := []string{}
	$for method in T.methods {
		$if method.return_type is int {
			rows << "return"
		}
		$if method.params[0].typ is int {
			rows << "param"
		}
		if method.typ == typeof[fn (int) int]().idx {
			rows << "signature"
		}
	}
	return rows.join("|")
}

fn main() {
	println(reflected_method_metadata[Box[int]]())
}
', 'return|param|signature'},
	])
}

fn test_method_metadata_locations_are_materialized() {
	out := round4_run_good(v3_bin, 'method_metadata_location', 'struct Located {}

fn (item Located) run() {
	_ = item
}

fn main() {
	mut items := []FunctionData{}
	mut locations := []string{}
	mut guarded := []string{}
	$for method in Located.methods {
		locations << method.location
		items << method
		$if method.location != "" {
			guarded << method.name
		}
	}
	println(locations[0].ends_with(":3:18"))
	println(items[0].location == locations[0])
	println(guarded.join("|"))
}
')
	assert out == 'true\ntrue\nrun'
}

fn test_quoted_attribute_names_and_arguments() {
	round4_check_runtime('quoted_attribute_names_and_arguments', [
		Round4RuntimeCase{
			name:     'quoted_attribute_kind'
			source:   "@['route'; plain]
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
"
			expected: 'route:string:false|plain:plain:false'
		},
		Round4RuntimeCase{
			name:     'string_attribute_names_with_colons'
			source:   "@['http://x'; 'a:b']
struct ColonNamedAttrs {}

fn describe_colon_named_attr(attr VAttribute) string {
	return attr.name + '#' + attr.arg + '#' + attr.has_arg.str() + '#' + attr.kind.str()
}

fn main() {
	mut rows := []string{}
	\$for attr in ColonNamedAttrs.attributes {
		\$if attr.has_arg {
			missing_string_attribute_argument_branch()
		}
		rows << describe_colon_named_attr(attr)
	}
	println(rows.join('|'))
}
"
			expected: 'http://x##false#string|a:b##false#string'
		},
		Round4RuntimeCase{
			name:     'quoted_param_attribute_guards'
			source:   "@[route]
@[timeout: 5]
struct GuardAttrs {}

fn inspect(target string, other int) {
	_ = target
	_ = other
}

fn main() {
	mut rows := []string{}
	\$for param in inspect.params {
		\$if param.name == 'target' || 'param.typ' == 'int' {
			rows << param.name
		}
	}
	\$for attr in GuardAttrs.attributes {
		\$if attr.name == 'route' || 'attr.has_arg' == 'true' {
			rows << attr.name
		}
	}
	println(rows.join('|'))
}
"
			expected: 'target|route'
		},
		Round4RuntimeCase{
			name:     'raw_string_attribute_argument'
			source:   "@[route: r'/path']
fn handler() {}

fn main() {
	mut rows := []string{}
	\$for attr in handler.attributes {
		rows << attr.name + ':' + attr.arg
	}
	println(rows.join('|'))
}
"
			expected: 'route:/path'
		},
		Round4RuntimeCase{
			name:     'attribute_guard_escaped_argument'
			source:   '@[label: "O\'Reilly"]
fn handler() {}

fn main() {
	mut matched := false
	\$for attr in handler.attributes {
		\$if attr.arg == "O\'Reilly" {
			matched = true
		}
	}
	println(matched)
}
'
			expected: 'true'
		},
	])
}

fn test_qualified_single_letter_type_is_a_concrete_generic_argument() {
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

fn test_comptime_pseudo_values_and_define_shadowing() {
	round4_check_runtime('comptime_pseudo_values_and_define_shadowing', [
		Round4RuntimeCase{'pseudo_value_cached_local', "fn main() {
	c := 'js'
	_ = c
	\$if @BACKEND == 'c' {
		println('ok')
	} \$else {
		println('wrong')
	}
}
", 'ok'},
		Round4RuntimeCase{'define_builtin_cached_local', "fn main() {
	d := false
	_ = d
	\$if \$d('v3_parser_cached_d_regression', true) {
		println('ok')
	} \$else {
		println('wrong')
	}
}
", 'ok'},
		Round4RuntimeCase{'fn_literal_comptime_value_scope', "fn main() {
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
", 'ok'},
	])
}

fn test_build_pseudo_values_expand_in_comptime_conditions() {
	previous_epoch := os.getenv_opt('SOURCE_DATE_EPOCH')
	os.setenv('SOURCE_DATE_EPOCH', '0', true)
	defer {
		if epoch := previous_epoch {
			os.setenv('SOURCE_DATE_EPOCH', epoch, true)
		} else {
			os.unsetenv('SOURCE_DATE_EPOCH')
		}
	}
	out := round4_run_good(v3_bin, 'build_pseudo_values_in_comptime_conditions', "fn main() {
	mut rows := []string{}
	\$if @BUILD_TIMESTAMP == '0' {
		rows << 'timestamp'
	} \$else {
		rows << 'wrong-timestamp'
	}
	\$if @BUILD_DATE == '1970-01-01' {
		rows << 'date'
	} \$else {
		rows << 'wrong-date'
	}
	\$match @BUILD_TIME {
		'00:00:00' { rows << 'time' }
		\$else { rows << 'wrong-time' }
	}
	println(rows.join('|'))
}
")
	assert out == 'timestamp|date|time'
}

fn test_path_pseudo_values_expand_in_comptime_conditions() {
	out := round4_run_good(v3_bin, 'path_pseudo_values_in_comptime_conditions', "fn main() {
	mut rows := []string{}
	\$if @FILE != '@FILE' {
		rows << 'file'
	}
	\$if @DIR != '@DIR' {
		rows << 'dir'
	}
	\$if @LINE != '@LINE' {
		rows << 'line'
	}
	\$if @FILE_LINE != '@FILE_LINE' {
		rows << 'file-line'
	}
	\$if @VEXE != '@VEXE' {
		rows << 'vexe'
	}
	\$if @VEXEROOT != '@VEXEROOT' {
		rows << 'vexeroot'
	}
	\$if @VMODROOT != '@VMODROOT' {
		rows << 'vmodroot'
	}
	\$if @LOCATION != '@LOCATION' {
		rows << 'location'
	}
	\$if @VHASH == '' && @VCURRENTHASH == '' {
		rows << 'hashes'
	}
	\$match @FILE {
		'@FILE' { rows << 'wrong-match' }
		\$else { rows << 'match' }
	}
	println(rows.join('|'))
}
")
	assert out == 'file|dir|line|file-line|vexe|vexeroot|vmodroot|location|match'
}

fn test_comptime_line_pseudo_values_use_their_token_positions() {
	name := 'comptime_line_pseudo_token_positions'
	src_path := '${round4_tmp_path(name)}.v'
	src_file_name := os.file_name(src_path)
	mut lines := ['fn main() {', '\tmut rows := []string{}']
	if_line := lines.len + 1
	lines << "\t\$if @LINE == '${if_line}'"
	lines << '\t{'
	lines << "\t\trows << 'if-line'"
	lines << '\t}'
	file_line := lines.len + 1
	lines << "\t\$if @FILE_LINE == '${src_file_name}:${file_line}'"
	lines << '\t{'
	lines << "\t\trows << 'if-file-line'"
	lines << '\t}'
	match_line := lines.len + 1
	lines << '\t\$match @LINE'
	lines << '\t{'
	lines << "\t\t'${match_line}' { rows << 'match-line' }"
	lines << "\t\t\$else { rows << 'wrong-match-line' }"
	lines << '\t}'
	match_file_line := lines.len + 1
	lines << '\t\$match @FILE_LINE'
	lines << '\t{'
	lines << "\t\t'${src_file_name}:${match_file_line}' { rows << 'match-file-line' }"
	lines << "\t\t\$else { rows << 'wrong-match-file-line' }"
	lines << '\t}'
	lines << "\tprintln(rows.join('|'))"
	lines << '}'
	out := round4_run_good(v3_bin, name, lines.join('\n') + '\n')
	assert out == 'if-line|if-file-line|match-line|match-file-line'
}

fn test_attribute_line_pseudo_values_use_their_token_positions() {
	name := 'attribute_line_pseudo_token_positions'
	src_path := '${round4_tmp_path(name)}.v'
	src_file_name := os.file_name(src_path)
	mut lines := []string{}
	line := lines.len + 1
	lines << "@[if @LINE == '${line}'"
	lines << ']'
	lines << "const selected_line = 'line'"
	file_line := lines.len + 1
	lines << "@[if @FILE_LINE == '${src_file_name}:${file_line}'"
	lines << ']'
	lines << "const selected_file_line = 'file-line'"
	lines << ''
	lines << 'fn main() {'
	lines << "\tprintln(selected_line + '|' + selected_file_line)"
	lines << '}'
	out := round4_run_good(v3_bin, name, lines.join('\n') + '\n')
	assert out == 'line|file-line'
}

fn test_cached_and_shadowed_char_literals() {
	round4_check_runtime('cached_and_shadowed_char_literals', [
		Round4RuntimeCase{'cached_char_literals', "const cached_char = `a`

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
", 'const-if|local-match'},
		Round4RuntimeCase{'char_literal_cached_local_collision', "fn main() {
	a := `b`
	ch := `a`
	_ = a
	\$if ch == `a` {
		println('ok')
	} \$else {
		println('wrong')
	}
}
", 'ok'},
	])
}

fn test_dynamic_selector_outside_reflection_loop_is_rejected() {
	round4_check_rejections('reflection_selector_rejections', [
		Round4Rejection{'dynamic_selector_outside_reflection_loop', 'struct Item {
	name string
}

fn main() {
	item := Item{
		name: "Ada"
	}
	name := "name"
	println(item.$(name))
}
', 'expected selector expression e.g. `$(field.name)`'},
		Round4Rejection{'shorthand_method_selector_outside_loop', 'struct Item {}

fn (item Item) run() {
	_ = item
}

fn main() {
	item := Item{}
	item.$method()
}
', 'unknown identifier `method`'},
		Round4Rejection{'misspelled_shorthand_method_selector', 'struct Item {}

fn (item Item) run() {
	_ = item
}

fn main() {
	item := Item{}
	$for method in Item.methods {
		item.$methd()
	}
}
', 'unknown identifier `methd`'},
	])
}

fn test_raw_and_quoted_string_literal_shadowing() {
	round4_check_runtime('raw_and_quoted_string_literal_shadowing', [
		Round4RuntimeCase{'raw_string_comptime_local_collision', "fn main() {
	r := 'wrong'
	_ = r
	\$if r'abc' == 'abc' {
		println('ok')
	} \$else {
		println('wrong')
	}
}
", 'ok'},
		Round4RuntimeCase{'comptime_string_literal_cached_local', "fn main() {
	x := 'y'
	_ = x
	\$if 'x' == 'y' {
		println('wrong')
	} \$else {
		println('ok')
	}
}
", 'ok'},
		Round4RuntimeCase{'guard_member_text_literal', "struct S {
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
	", 'id|red'},
	])
}

fn test_raw_string_comptime_match_literals_are_normalized() {
	out := round4_run_good(v3_bin, 'raw_string_comptime_match_literals', "fn main() {
	\$match r'abc' {
		'abc' { println('subject') }
		\$else { println('wrong-subject') }
	}
	\$match 'def' {
		r'def' { println('pattern') }
		\$else { println('wrong-pattern') }
	}
}
")
	assert out == 'subject\npattern'
}

fn test_quoted_and_nested_reflection_selectors() {
	round4_check_runtime('quoted_and_nested_reflection_selectors', [
		Round4RuntimeCase{
			name:     'field_condition_string_literal'
			source:   "struct QuotedFields {
pub:
	id int
}

fn main() {
	mut rows := []string{}
	\$for field in QuotedFields.fields {
		\$if 'field.is_pub' == 'true' {
			rows << 'wrong'
		} \$else {
			rows << field.name
		}
		\$if field.name == 'id' {
			rows << 'real-selector'
		}
	}
	println(rows.join('|'))
}
"
			expected: 'id|real-selector'
		},
		Round4RuntimeCase{
			name:     'method_condition_string_literal'
			source:   "struct QuotedMethods {}

pub fn (q QuotedMethods) target() {
	_ = q
}

pub fn (q QuotedMethods) other() {
	_ = q
}

fn main() {
	mut rows := []string{}
	keep_serial := fn () {}
	keep_serial()
	value := QuotedMethods{}
	value.target()
	value.other()
	\$for method in QuotedMethods.methods {
		\$if method.name == 'target' || 'method.is_pub' == 'true' {
			rows << method.name
		}
	}
	println(rows.join('|'))
}
"
			expected: 'target'
		},
		Round4RuntimeCase{
			name:     'nested_field_method_guard'
			source:   "struct Row {
	id int
	name string
}

struct App {}

fn (a App) index() {
	_ = a
}

fn (a App) submit() {
	_ = a
}

fn main() {
	mut rows := []string{}
	\$for field in Row.fields {
		\$for method in App.methods {
			\$if field.name == 'id' && method.name == 'index' {
				rows << field.name + ':' + method.name
			}
		}
	}
	println(rows.join('|'))
}
"
			expected: 'id:index'
		},
		Round4RuntimeCase{
			name:     'nested_method_shadowing'
			source:   'struct OuterMethods {}

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
'
			expected: 'outer:outer|alpha=alpha:alpha|beta=beta:beta'
		},
	])
}

fn test_vmod_file_expands_in_comptime_conditions() {
	out := round4_run_good_project(v3_bin, 'vmod_file_comptime_conditions', {
		'v.mod':  "Module { name: 'vmod_file_comptime_conditions' }\n"
		'main.v': "module main

fn main() {
	mut rows := []string{}
	\$if @VMOD_FILE != '@VMOD_FILE' {
		rows << 'if'
	} \$else {
		rows << 'wrong-if'
	}
	\$match @VMOD_FILE {
		'@VMOD_FILE' { rows << 'wrong-match' }
		\$else { rows << 'match' }
	}
	println(rows.join('|'))
}
"
	}, 'main.v')
	assert out == 'if|match'
}

fn test_vmod_file_comptime_condition_without_project_is_compile_error() {
	round4_run_bad(v3_bin, 'vmod_file_comptime_condition_without_project', "fn main() {
	\$if @VMOD_FILE == '' {}
}
",
		'@VMOD_FILE can only be used in projects that have a v.mod file')
}

fn test_comptime_const_values_persist_across_module_files_without_leaking() {
	out := round4_run_good_project(v3_bin, 'comptime_const_values_across_files', {
		'v.mod':      "Module { name: 'comptime_const_values_across_files' }\n"
		'a_consts.v': "module main

const enabled = true
const flavor = 'vanilla'
"
		'b_main.v':   "module main

import pkg

fn main() {
	mut rows := []string{}
	\$if enabled == true {
		rows << 'if'
	} \$else {
		rows << 'wrong-if'
	}
	\$match flavor {
		'vanilla' { rows << 'match' }
		\$else { rows << 'wrong-match' }
	}
	rows << pkg.value()
	println(rows.join('|'))
}
"
		'pkg/pkg.v':  "module pkg

pub fn value() string {
	\$if enabled ? {
		return 'wrong-leak'
	}
	return 'pkg'
}
"
	}, '')
	assert out == 'if|match|pkg'
}

fn test_soa_attribute_and_static_method_reflection_distinction() {
	out := round4_run_good(v3_bin, 'soa_and_static_method_distinction', "@[soa]
struct SoaItem {
	value int
}

@[typedef]
struct C.SoaItem_SOA {
	len   int
	cap   int
	value &int
}

fn C.SoaItem_SOA_new(int, int) C.SoaItem_SOA
fn C.SoaItem_SOA_push(&C.SoaItem_SOA, SoaItem)
fn C.SoaItem_SOA_get(C.SoaItem_SOA, int) SoaItem
fn C.SoaItem_SOA_free(&C.SoaItem_SOA)

struct Foo {}

fn Foo.make(value Foo) {
	_ = value
}

fn (foo Foo) run(value int) {
	_ = foo
	_ = value
}

fn main() {
	mut rows := []string{}
	mut soa := C.SoaItem_SOA_new(0, 3)
	C.SoaItem_SOA_push(&soa, SoaItem{value: 3})
	item := C.SoaItem_SOA_get(soa, 0)
	rows << soa.cap.str() + ':' + soa.len.str() + ':' + item.value.str()
	C.SoaItem_SOA_free(&soa)
	\$for method in Foo.methods {
		rows << method.name + ':' + method.params.len.str()
	}
	println(rows.join('|'))
}
")
	assert out == '3:1:3|run:1'
}

fn test_v_owned_matching_soa_companion_declaration_is_preserved() {
	out := round4_run_good(v3_bin, 'v_owned_matching_soa_companion', "@[soa]
struct VOwnedSoaItem {
	value int
}

struct C.VOwnedSoaItem_SOA {
	len   int
	cap   int
	value &int
}

fn C.VOwnedSoaItem_SOA_new(int, int) C.VOwnedSoaItem_SOA
fn C.VOwnedSoaItem_SOA_push(&C.VOwnedSoaItem_SOA, VOwnedSoaItem)
fn C.VOwnedSoaItem_SOA_get(C.VOwnedSoaItem_SOA, int) VOwnedSoaItem
fn C.VOwnedSoaItem_SOA_free(&C.VOwnedSoaItem_SOA)

fn main() {
	mut soa := C.VOwnedSoaItem_SOA_new(0, 2)
	C.VOwnedSoaItem_SOA_push(&soa, VOwnedSoaItem{value: 5})
	item := C.VOwnedSoaItem_SOA_get(soa, 0)
	println(soa.cap.str() + ':' + soa.len.str() + ':' + item.value.str())
	C.VOwnedSoaItem_SOA_free(&soa)
}
")
	assert out == '2:1:5'
}

fn test_header_backed_soa_companion_uses_native_typedef() {
	out := round4_run_good_project(v3_bin, 'header_backed_soa_companion', {
		'v.mod':           "Module { name: 'header_backed_soa_companion' }\n"
		'soa.h':           '#ifndef HEADER_BACKED_SOA_H\n#define HEADER_BACKED_SOA_H\n#include <stdint.h>\ntypedef struct HeaderSoaItem_SOA {\n\tint len;\n\tint cap;\n\tint64_t* value;\n} HeaderSoaItem_SOA;\n#endif\n'
		'soa_binding.c.v': 'module main

#include "@VMODROOT/soa.h"

@[typedef]
struct C.HeaderSoaItem_SOA {
	len   int
	cap   int
	value &int
}

fn C.HeaderSoaItem_SOA_new(int, int) C.HeaderSoaItem_SOA
fn C.HeaderSoaItem_SOA_push(&C.HeaderSoaItem_SOA, HeaderSoaItem)
fn C.HeaderSoaItem_SOA_get(C.HeaderSoaItem_SOA, int) HeaderSoaItem
fn C.HeaderSoaItem_SOA_free(&C.HeaderSoaItem_SOA)
'
		'main.v':          'module main

@[soa]
struct HeaderSoaItem {
	value int
}

fn main() {
	mut soa := C.HeaderSoaItem_SOA_new(0, 2)
	C.HeaderSoaItem_SOA_push(&soa, HeaderSoaItem{value: 7})
	item := C.HeaderSoaItem_SOA_get(soa, 0)
	println(soa.cap.str() + ":" + soa.len.str() + ":" + item.value.str())
	C.HeaderSoaItem_SOA_free(&soa)
}
'
	}, '')
	assert out == '2:1:7'
}

fn test_nested_parameter_attribute_and_field_selectors() {
	round4_check_runtime('nested_parameter_attribute_and_field_selectors', [
		Round4RuntimeCase{
			name:     'nested_param_attribute_shadowing'
			source:   "@[outer]
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
"
			expected: 'outer-param:first|inner-param:second|outer-attr:outer|inner-attr:inner'
		},
		Round4RuntimeCase{
			name:     'nested_param_guard_identifier_boundaries'
			source:   "fn outer(y string, z int) {
	_ = y
	_ = z
}

fn inner(x string, q int) {
	_ = x
	_ = q
}

fn main() {
	mut rows := []string{}
	\$for outer_param in outer.params {
		\$for param in inner.params {
			\$if param.name == 'x' || outer_param.name == 'y' {
				rows << outer_param.name + ':' + param.name
			}
		}
	}
	println(rows.join('|'))
}
"
			expected: 'y:x|y:q|z:x'
		},
		Round4RuntimeCase{
			name:     'nested_method_attribute_guard'
			source:   "struct App {}

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
"
			expected: 'index:get'
		},
		Round4RuntimeCase{
			name:     'nested_attribute_method_guard'
			source:   "@[route]
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
"
			expected: 'route:index'
		},
		Round4RuntimeCase{
			name:     'nested_param_field_guard'
			source:   "struct Row {
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
"
			expected: 'x:id'
		},
	])
}

fn test_normal_string_attribute_arguments_are_unescaped() {
	src := [
		"@[quote: 'O\\'Reilly']",
		"@[line: 'a\\n']",
		"@[hex: '\\x41']",
		"@[unicode: '\\u03bb']",
		"@[raw: r'a\\n']",
		'fn handler() {}',
		'',
		'fn main() {',
		'\tmut rows := []string{}',
		'\t\$for attr in handler.attributes {',
		"\t\trows << attr.name + ':' + attr.arg.replace('\\n', '<nl>')",
		'\t}',
		'\tprintln(rows.join("|"))',
		'}',
	].join('\n')
	out := round4_run_good(v3_bin, 'normal_string_attribute_arguments', src)
	assert out == "quote:O'Reilly|line:a<nl>|hex:A|unicode:λ|raw:a\\n"
	round4_run_bad(v3_bin, 'escaped_attribute_selected_guard', [
		"@[label: 'O\\'Reilly']",
		'fn handler() {}',
		'',
		'fn main() {',
		'\t\$for attr in handler.attributes {',
		'\t\t\$if attr.arg == "O\'Reilly" {',
		'\t\t\tmissing_selected_escaped_attribute_fn()',
		'\t\t}',
		'\t}',
		'}',
	].join('\n'), 'unknown function: missing_selected_escaped_attribute_fn')
}

fn test_generic_reflection_compile_error_waits_for_selected_branch() {
	round4_run_bad(v3_bin, 'generic_reflection_selected_compile_error', "struct App {}

fn (app App) present() {
	_ = app
}

fn validate[T]() {
	\$for method in T.methods {
		\$if method.name == 'present' {
			\$compile_error('present method selected')
		}
	}
}

fn main() {
	validate[App]()
}
",
		'compile-time error: present method selected')
}

fn test_generic_method_reflection_preserves_main_specialization_origin() {
	out := round4_run_good_project(v3_bin, 'generic_method_reflection_main_origin', {
		'v.mod':     "Module { name: 'generic_method_reflection_main_origin' }\n"
		'pkg/pkg.v': 'module pkg

pub struct App {}

fn (app App) package_method() {
	_ = app
}

pub fn method_names[T]() string {
	mut names := []string{}
	$for method in T.methods {
		names << method.name
	}
	return names.join(",")
}
'
		'main.v':    'module main

import pkg

struct App {}

fn (app App) main_method() {
	_ = app
}

fn main() {
	println(pkg.method_names[App]())
}
'
	}, 'main.v')
	assert out == 'main_method'
}

fn test_selected_comptime_if_expression_keeps_setup_statements() {
	out := round4_run_good(v3_bin, 'comptime_if_expr_setup_statements', "fn main() {
	x := \$if true {
		println('then setup')
		tmp := 1
		tmp + 1
	} \$else {
		0
	}
	y := \$if false {
		0
	} \$else {
		println('else setup')
		tmp := 3
		tmp
	}
	println(int_str(x) + ':' + int_str(y))
}
")
	assert out == 'then setup\nelse setup\n2:3'
}

fn test_comptime_string_comparisons_normalize_escapes() {
	src := [
		'const quote = "\'"',
		"const slash = '\\\\'",
		'fn main() {',
		'\t\$if "\'" == \'\\\'\' { println(\'if quote\') } \$else { println(\'bad if quote\') }',
		"\t\$if quote == '\\'' { println('const quote') } \$else { println('bad const quote') }",
		"\t\$if '\\\\' == '\\x5c' { println('if slash') } \$else { println('bad if slash') }",
		"\t\$if slash == '\\x5c' { println('const slash') } \$else { println('bad const slash') }",
		'\t\$match "\'" { \'\\\'\' { println(\'match quote\') } \$else { println(\'bad match quote\') } }',
		"\t\$match '\\\\' { '\\x5c' { println('match slash') } \$else { println('bad match slash') } }",
		'}',
	].join('\n')
	out := round4_run_good(v3_bin, 'comptime_escaped_string_comparisons', src)
	assert out == 'if quote\nconst quote\nif slash\nconst slash\nmatch quote\nmatch slash'
}

fn test_deferred_reflection_branch_pruning() {
	round4_check_runtime('deferred_reflection_branch_pruning', [
		Round4RuntimeCase{
			name:     'deferred_loop_unselected_metadata_branches'
			source:   '@[route]
struct App {}

fn (app App) run() {
	_ = app
}

fn consume(value int) {
	_ = value
}

fn main() {
	mut rows := []string{}
	$for method in App.methods {
		$if method.name == "missing" {
			missing_unselected_method_fn()
		} $else {
			rows << method.name
		}
	}
	$for param in consume.params {
		$if param.name == "missing" {
			missing_unselected_param_fn()
		} $else {
			rows << param.name
		}
	}
	$for attr in App.attributes {
		$if attr.name == "missing" {
			missing_unselected_attribute_fn()
		} $else {
			rows << attr.name
		}
	}
	println(rows.join("|"))
}
'
			expected: 'run|value|route'
		},
		Round4RuntimeCase{
			name:     'deferred_loop_unselected_non_name_metadata_branches'
			source:   '@[route]
struct App {}

fn (app App) run() {
	_ = app
}

fn consume(value int) {
	_ = value
}

fn main() {
	mut rows := []string{}
	$for method in App.methods {
		$if method.is_pub {
			missing_unselected_public_method_fn()
		} $else {
			rows << method.name
		}
	}
	$for param in consume.params {
		$if param.typ is string {
			missing_unselected_string_param_fn()
		} $else {
			rows << param.name
		}
	}
	$for attr in App.attributes {
		$if attr.has_arg {
			missing_unselected_attribute_arg_fn()
		} $else {
			rows << attr.name
		}
	}
	println(rows.join("|"))
}
'
			expected: 'run|value|route'
		},
		Round4RuntimeCase{'generic_reflection_unselected_compile_error', "struct App {}

fn (app App) present() {
	_ = app
}

fn validate[T]() {
	\$for method in T.methods {
		\$if method.name == 'missing' {
			\$compile_error('missing method selected')
		}
	}
}

fn main() {
	validate[App]()
	println('ok')
}
", 'ok'},
	])
}

fn test_empty_deferred_reflection_loops_skip_static_body_checks() {
	round4_check_runtime('empty_deferred_and_interface_attribute_loops', [
		Round4RuntimeCase{'empty_deferred_reflection_loops', 'struct Empty {}

fn no_params() {}

fn main() {
	$for method in Empty.methods {
		missing_empty_method_fn()
	}
	$for param in no_params.params {
		missing_empty_param_fn()
	}
	$for attr in Empty.attributes {
		missing_empty_attribute_fn()
	}
	println("ok")
}
', 'ok'},
		Round4RuntimeCase{'empty_interface_attribute_loop', 'interface EmptyInterface {}

fn main() {
	$for attr in EmptyInterface.attributes {
		missing_empty_interface_attribute_fn()
		$compile_error("empty interface attribute loop should not run")
	}
	println("ok")
}
', 'ok'},
	])
	imported_out := round4_run_good_project(v3_bin, 'empty_imported_deferred_reflection_loops', {
		'v.mod':            "Module { name: 'empty_imported_deferred_reflection_loops' }\n"
		'pkg/pkg.v':        'module pkg

pub struct Empty {}

pub fn no_params() {}
'
		'selected/types.v': 'module selected

pub struct EmptySelected {}

pub fn no_params_selected() {}
'
		'main.v':           'module main

import pkg as p
import selected { EmptySelected, no_params_selected }

fn main() {
	$for method in p.Empty.methods {
		missing_empty_imported_method_fn()
	}
	$for param in p.no_params.params {
		missing_empty_imported_param_fn()
	}
	$for attr in p.Empty.attributes {
		missing_empty_imported_attribute_fn()
	}
	$for method in EmptySelected.methods {
		missing_empty_selected_method_fn()
	}
	$for param in no_params_selected.params {
		missing_empty_selected_param_fn()
	}
	$for attr in EmptySelected.attributes {
		missing_empty_selected_attribute_fn()
	}
	println("imported ok")
}
'
	}, '')
	assert imported_out == 'imported ok'
}
