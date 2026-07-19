module markused

import os
import v3.flat
import v3.gen.c as cgen
import v3.parser
import v3.pref
import v3.transform
import v3.types

const custom_str_tests_dir = os.dir(@FILE)
const custom_str_v3_dir = os.dir(custom_str_tests_dir)
const custom_str_v3_src = os.join_path(custom_str_v3_dir, 'v3.v')
const custom_str_vexe = @VEXE

// parse_checked_two_file_source reads parse checked two file source input for v3 tests.
fn parse_checked_two_file_source(name string, main_source string, module_rel string, module_source string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.temp_dir(), 'v3_markused_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	main_src := os.join_path(root, 'main.v')
	module_src := os.join_path(root, module_rel)
	os.mkdir_all(os.dir(module_src)) or { panic(err) }
	os.write_file(main_src, main_source) or { panic(err) }
	os.write_file(module_src, module_source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files([main_src, module_src])
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[main_src] = true
	tc.diagnostic_files[module_src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn parse_checked_prelude_user_source(name string, prelude_rel string, prelude_source string, main_source string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.temp_dir(), 'v3_markused_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	prelude_src := os.join_path(root, prelude_rel)
	main_src := os.join_path(root, 'main.v')
	os.mkdir_all(os.dir(prelude_src)) or { panic(err) }
	os.write_file(prelude_src, prelude_source) or { panic(err) }
	os.write_file(main_src, main_source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	p.parse_file(prelude_src)
	p.a.user_code_start = p.a.nodes.len
	mut a := p.parse_files([main_src])
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[prelude_src] = true
	tc.diagnostic_files[main_src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

// build_v3_bin builds v3 bin data for v3 tests.
fn build_v3_bin(name string) string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	build := os.execute('${custom_str_vexe} -o ${v3_bin} ${custom_str_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// imported_enum_main_source supports imported enum main source handling for v3 tests.
fn imported_enum_main_source(expr string) string {
	return '
module main

import colors

fn main() {
	c := colors.Color.red
	${expr}
}
'
}

// imported_optional_enum_main_source
// supports helper handling in v3 tests.
fn imported_optional_enum_main_source(expr string) string {
	return '
module main

import colors

fn maybe_color() ?colors.Color {
	return colors.Color.red
}

fn main() {
	${expr}
}
'
}

// imported_enum_module_source supports imported enum module source handling for v3 tests.
fn imported_enum_module_source() string {
	return '
module colors

pub enum Color {
	red
	blue
}

pub fn (c Color) str() string {
	return "red"
}
'
}

// imported_operator_main_source supports imported operator main source handling for v3 tests.
fn imported_operator_main_source(expr string) string {
	return '
module main

import vectors

fn main() {
	a := vectors.new_vec(1)
	b := vectors.new_vec(2)
	${expr}
}
'
}

// imported_operator_module_source supports imported operator module source handling for v3 tests.
fn imported_operator_module_source() string {
	return '
module vectors

pub struct Vec {
	x int
}

pub fn new_vec(x int) Vec {
	return Vec{
		x: x
	}
}

pub fn (a Vec) + (b Vec) Vec {
	return Vec{
		x: a.x + b.x
	}
}

pub fn (a Vec) < (b Vec) bool {
	return a.x < b.x
}
'
}

// imported_operator_usage_source supports imported operator usage source handling for v3 tests.
fn imported_operator_usage_source() string {
	return 'sum := a + b
	gt := b > a
	if gt {
		_ := sum
	}'
}

// imported_struct_default_main_source supports imported_struct_default_main_source handling.
fn imported_struct_default_main_source(expr string) string {
	return '
module main

import defaults

fn main() {
	${expr}
}
'
}

// imported_struct_default_module_source supports imported_struct_default_module_source handling.
fn imported_struct_default_module_source() string {
	return '
module defaults

pub struct Box {
	value int = default_value()
}

pub fn default_value() int {
	return 7
}

pub fn maybe_box() ?Box {
	return none
}
'
}

// test_string_interpolation_seeds_imported_enum_str_method validates this v3 regression case.
fn test_string_interpolation_seeds_imported_enum_str_method() {
	a, tc := parse_checked_two_file_source('imported_enum_interp_str',
		imported_enum_main_source('_ := "\${c}"'), 'colors/colors.v', imported_enum_module_source())
	mut used := mark_used(a, tc)
	assert used['colors.Color.str']
}

// test_optional_string_interpolation_seeds_imported_enum_str_method
// validates this v3 regression case.
fn test_optional_string_interpolation_seeds_imported_enum_str_method() {
	a, tc := parse_checked_two_file_source('imported_optional_enum_interp_str',
		imported_optional_enum_main_source('_ := "\${maybe_color()}"'), 'colors/colors.v',
		imported_enum_module_source())
	mut used := mark_used(a, tc)
	assert used['colors.Color.str']
}

// test_imported_operator_infix_seeds_operator_methods validates this v3 regression case.
fn test_imported_operator_infix_seeds_operator_methods() {
	a, tc := parse_checked_two_file_source('imported_operator_infix',
		imported_operator_main_source(imported_operator_usage_source()), 'vectors/vectors.v',
		imported_operator_module_source())
	mut used := mark_used(a, tc)
	assert used['vectors.Vec.+']
	assert used['vectors.Vec.<']
}

// test_flag_enum_seeds_string_plus_helper validates this v3 regression case: a `[flag]`
// enum's synthesized `<Enum>__autostr` builds its string with `string__plus`, but that
// generated body is invisible to markused, so the helper must be seeded from the flag-enum
// declaration or it could be pruned and leave the autostr calling an undefined helper.
fn test_flag_enum_seeds_string_plus_helper() {
	main_src := '
module main

import colors

@[flag]
enum Perm {
	read
	write
}

fn main() {
	p := Perm.read | Perm.write
	_ := p
	_ := colors.Color.red
}
'
	a, tc := parse_checked_two_file_source('flag_enum_string_plus', main_src, 'colors/colors.v',
		imported_enum_module_source())
	mut used := mark_used(a, tc)
	assert used['string__plus']
}

fn test_channel_auto_str_seeds_inline_helpers() {
	main_src := '
module main

import support

fn main() {
	ch := chan int{cap: support.capacity}
	println(ch)
}
'
	a, tc := parse_checked_two_file_source('channel_auto_str_helpers', main_src,
		'support/support.v', 'module support\n\npub const capacity = 1\n')
	mut used := mark_used(a, tc)
	assert used['string__plus']
	assert used['int__str']
}

// test_optional_struct_zero_seeds_imported_default_helper validates this v3 regression case.
fn test_optional_struct_zero_seeds_imported_default_helper() {
	a, tc := parse_checked_two_file_source('imported_struct_default_or',
		imported_struct_default_main_source('box := defaults.maybe_box() or { return }\n\t_ := box'),
		'defaults/defaults.v', imported_struct_default_module_source())
	mut used := mark_used(a, tc)
	assert used['defaults.default_value']
}

fn test_prelude_global_initializer_seeds_calls_and_c_externs() {
	mut a, mut tc := parse_checked_prelude_user_source('prelude_global_initializer',
		'hidden/hidden.c.v', 'module hidden

fn C.hidden_external() int

pub const hidden_value = helper() + C.hidden_external()

fn helper() int {
	return 7
}
', 'module main

fn main() {}
')
	mut used := mark_used(a, tc)
	assert used['hidden.helper']
	assert used['C.hidden_external'] || used['hidden_external']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('hidden__helper(')
	assert c_code.contains('hidden_external(void);')
}

// test_string_interpolation_lowers_to_imported_enum_str_after_used_filter_transform
// validates this v3 regression case.
fn test_string_interpolation_lowers_to_imported_enum_str_after_used_filter_transform() {
	mut a, mut tc := parse_checked_two_file_source('imported_enum_interp_str_cgen',
		imported_enum_main_source('_ := "\${c}"'), 'colors/colors.v', imported_enum_module_source())
	mut used := mark_used(a, tc)
	assert used['colors.Color.str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('colors__Color__str(')
}

// test_imported_operator_infix_lowers_after_used_filter_transform
// validates this v3 regression case.
fn test_imported_operator_infix_lowers_after_used_filter_transform() {
	mut a, mut tc := parse_checked_two_file_source('imported_operator_infix_cgen',
		imported_operator_main_source(imported_operator_usage_source()), 'vectors/vectors.v',
		imported_operator_module_source())
	mut used := mark_used(a, tc)
	assert used['vectors.Vec.+']
	assert used['vectors.Vec.<']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('vectors__Vec__plus(')
	assert c_code.contains('vectors__Vec__lt(')
}

// test_optional_struct_zero_lowers_to_imported_default_after_used_filter_transform
// validates this v3 regression case.
fn test_optional_struct_zero_lowers_to_imported_default_after_used_filter_transform() {
	mut a, mut tc := parse_checked_two_file_source('imported_struct_default_or_cgen',
		imported_struct_default_main_source('box := defaults.maybe_box() or { return }\n\t_ := box'),
		'defaults/defaults.v', imported_struct_default_module_source())
	mut used := mark_used(a, tc)
	assert used['defaults.default_value']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('defaults__default_value(')
}

// test_optional_string_interpolation_lowers_to_imported_enum_str_after_used_filter_transform
// validates this v3 regression case.
fn test_optional_string_interpolation_lowers_to_imported_enum_str_after_used_filter_transform() {
	mut a, mut tc := parse_checked_two_file_source('imported_optional_enum_interp_str_cgen',
		imported_optional_enum_main_source('_ := "\${maybe_color()}"'), 'colors/colors.v',
		imported_enum_module_source())
	mut used := mark_used(a, tc)
	assert used['colors.Color.str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('colors__Color__str(')
}

// test_imported_enum_print_compile_keeps_str_method validates this v3 regression case.
fn test_imported_enum_print_compile_keeps_str_method() {
	v3_bin := build_v3_bin('imported_enum_print_str_test')

	root := os.join_path(os.temp_dir(), 'v3_markused_imported_enum_print_input')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(os.join_path(root, 'colors')) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), imported_enum_main_source('println(c)')) or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'colors/colors.v'), imported_enum_module_source()) or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_markused_imported_enum_print_input_bin')
	compile := os.execute('${v3_bin} -o ${bin} ${root}')
	assert compile.exit_code == 0, compile.output
}

// test_imported_operator_compile_keeps_operator_methods validates this v3 regression case.
fn test_imported_operator_compile_keeps_operator_methods() {
	v3_bin := build_v3_bin('imported_operator_test')

	root := os.join_path(os.temp_dir(), 'v3_markused_imported_operator_input')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(os.join_path(root, 'vectors')) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'),
		imported_operator_main_source(imported_operator_usage_source())) or { panic(err) }
	os.write_file(os.join_path(root, 'vectors/vectors.v'), imported_operator_module_source()) or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_markused_imported_operator_input_bin')
	compile := os.execute('${v3_bin} -o ${bin} ${root}')
	assert compile.exit_code == 0, compile.output
}

// test_optional_struct_zero_compile_keeps_imported_default_helper
// validates this v3 regression case.
fn test_optional_struct_zero_compile_keeps_imported_default_helper() {
	v3_bin := build_v3_bin('imported_struct_default_or_test')

	root := os.join_path(os.temp_dir(), 'v3_markused_imported_struct_default_or_input')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(os.join_path(root, 'defaults')) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'),
		imported_struct_default_main_source('box := defaults.maybe_box() or { return }\n\t_ := box')) or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'defaults/defaults.v'),
		imported_struct_default_module_source()) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_markused_imported_struct_default_or_input_bin')
	compile := os.execute('${v3_bin} -o ${bin} ${root}')
	assert compile.exit_code == 0, compile.output
}

// test_imported_optional_enum_interpolation_compile_keeps_str_method
// validates this v3 regression case.
fn test_imported_optional_enum_interpolation_compile_keeps_str_method() {
	v3_bin := build_v3_bin('imported_optional_enum_interp_str_test')

	root := os.join_path(os.temp_dir(), 'v3_markused_imported_optional_enum_interp_input')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(os.join_path(root, 'colors')) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'),
		imported_optional_enum_main_source('_ := "\${maybe_color()}"')) or { panic(err) }
	os.write_file(os.join_path(root, 'colors/colors.v'), imported_enum_module_source()) or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_markused_imported_optional_enum_interp_input_bin')
	compile := os.execute('${v3_bin} -o ${bin} ${root}')
	assert compile.exit_code == 0, compile.output
}
