import os
import v3.flat
import v3.gen.c as cgen
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// parse_checked_source reads parse checked source input for v3 tests.
fn parse_checked_source(name string, source string) (&flat.FlatAst, &types.TypeChecker) {
	src := os.join_path(os.temp_dir(), 'v3_markused_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn parse_checked_project(name string, files map[string]string, main_file string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	os.rmdir_all(root) or {}
	for rel, source in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}
	mut paths := []string{}
	if main_file.len > 0 {
		paths << os.join_path(root, main_file)
	}
	mut rel_paths := []string{}
	for rel, _ in files {
		if rel != main_file {
			rel_paths << rel
		}
	}
	rel_paths.sort()
	for rel in rel_paths {
		paths << os.join_path(root, rel)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

// mark_used_source updates mark used source state for v3 tests.
fn mark_used_source(name string, source string) map[string]bool {
	a, tc := parse_checked_source(name, source)
	return markused.mark_used(a, tc)
}

// build_v3_bin builds v3 bin data for v3 tests.
fn build_v3_bin(name string) string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// test_map_literals_seed_new_map_runtime_helper validates this v3 regression case.
fn test_map_literals_seed_new_map_runtime_helper() {
	used := mark_used_source('map_literal_new_map', '
fn make_map() map[string]int {
	return map[string]int{}
}

fn main() {
	_ := make_map()
}
')
	assert used['new_map']
}

// test_optional_map_or_seeds_new_map_runtime_helper validates this v3 regression case.
fn test_optional_map_or_seeds_new_map_runtime_helper() {
	used := mark_used_source('option_map_or_new_map', '
fn maybe_map() ?map[string]int {
	return none
}

fn main() {
	m := maybe_map() or { return }
	_ := m
}
')
	assert used['new_map']
}

// test_string_membership_seeds_contains_runtime_helpers validates this v3 regression case.
fn test_string_membership_seeds_contains_runtime_helpers() {
	used := mark_used_source('string_membership_contains', '
fn has_needle() bool {
	return "bc" in "abcd"
}

fn main() {
	_ := has_needle()
}
')
	assert used['string__contains']
	assert used['string__contains_u8']
}

// test_string_interpolation_seeds_string_plus_and_formatter_helpers
// validates this v3 regression case.
fn test_string_interpolation_seeds_string_plus_and_formatter_helpers() {
	used := mark_used_source('string_interp_plus_formatter', '
fn message(name string) string {
	return "hello \${name} \${true}"
}

fn main() {
	_ := message("v")
}
')
	assert used['string__plus']
	assert used['bool.str']
}

// test_print_bool_seeds_formatter_runtime_helper validates this v3 regression case.
fn test_print_bool_seeds_formatter_runtime_helper() {
	used := mark_used_source('print_bool_formatter', '
fn println(s string) {}

fn main() {
	println(true)
}
')
	assert used['bool.str']
}

// test_string_compound_assign_seeds_string_plus_runtime_helper validates this v3 regression case.
fn test_string_compound_assign_seeds_string_plus_runtime_helper() {
	used := mark_used_source('string_plus_assign', '
fn main() {
	mut s := "a"
	s += "b"
	_ := s
}
')
	assert used['string__plus']
}

fn test_receiver_method_call_in_selector_assign_rhs_is_used() {
	used := mark_used_source('receiver_method_selector_assign_rhs', '
struct Builder {
mut:
	name string
}

fn (b &Builder) main_module_name() string {
	return "main"
}

fn build_with_options() string {
	mut b := Builder{}
	b.name = b.main_module_name()
	return b.name
}

fn main() {
	_ := build_with_options()
}
')
	assert used['Builder.main_module_name']
}

fn test_unreachable_interface_implementer_method_is_not_rooted() {
	used := mark_used_source('unreachable_interface_implementer', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

fn main() {}
')
	assert !used['File.read']
}

fn test_reachable_interface_dispatch_keeps_implementer_method() {
	used := mark_used_source('reachable_interface_dispatch', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

fn call_reader(r Reader) int {
	return r.read()
}

fn main() {
	_ := call_reader(File{})
}
')
	assert used['File.read']
}

fn test_global_interface_dispatch_keeps_implementer_method() {
	used := mark_used_source('global_interface_dispatch', '
interface Reader {
	read() int
}

struct File {}

__global default_reader &Reader

fn (f File) read() int {
	return 1
}

fn call_default_reader() int {
	return default_reader.read()
}

fn main() {
	_ := call_default_reader()
}
')
	assert used['Reader.read']
	assert used['File.read']
}

fn test_unreachable_interface_dispatch_stub_is_not_emitted_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('unreachable_interface_dispatch_cgen', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

fn main() {}
')
	mut used := markused.mark_used(a, tc)
	assert !used['Reader.read']
	assert !used['File.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('Reader__read(')
}

fn test_short_interface_dispatch_does_not_emit_imported_name_collision_stub() {
	mut a, mut tc := parse_checked_project('interface_dispatch_name_collision', {
		'main.v':      'module main

import moda

interface Reader {
	read() int
}

struct Local {}

fn (l Local) read() int {
	return 1
}

fn call_reader(r Reader) int {
	return r.read()
}

fn main() {
	_ := call_reader(Local{})
}
'
		'moda/moda.v': 'module moda

interface Reader {
	read(path string) string
}

struct Remote {}

fn (r Remote) read(path string) string {
	return path
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['Reader.read']
	assert !used['moda.Reader.read']
	assert !used['moda.Remote.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('Reader__read(')
	assert !c_code.contains('moda__Reader__read(')
}

fn test_local_receiver_method_does_not_root_imported_interface_by_short_name() {
	mut a, mut tc := parse_checked_project('local_receiver_imported_interface_short_name', {
		'main.v':      'module main

import moda

struct Reader {}

fn (r Reader) read() int {
	return 1
}

fn main() {
	_ := Reader{}.read()
}
'
		'moda/moda.v': 'module moda

pub interface Reader {
	read() int
}

pub struct Remote {}

pub fn (r Remote) read() int {
	return 2
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['Reader.read']
	assert !used['moda.Reader.read']
	assert !used['moda.Remote.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('Reader__read(')
	assert !c_code.contains('moda__Reader__read(')
	assert !c_code.contains('moda__Remote__read(')
}

fn test_imported_interface_dispatch_is_emitted_when_exactly_used() {
	mut a, mut tc := parse_checked_project('imported_interface_dispatch_used', {
		'main.v':      'module main

import moda

fn call_reader(r moda.Reader) string {
	return r.read("ok")
}

fn main() {
	_ := call_reader(moda.Remote{})
}
'
		'moda/moda.v': 'module moda

pub interface Reader {
	read(path string) string
}

pub struct Remote {}

pub fn (r Remote) read(path string) string {
	return path
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['moda.Reader.read']
	assert used['moda.Remote.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('moda__Reader__read(')
	assert c_code.contains('moda__Remote__read(')
}

fn test_interface_dispatch_target_does_not_use_bare_method_key_for_imported_homonym() {
	mut a, mut tc := parse_checked_project('interface_dispatch_target_homonym', {
		'main.v':      'module main

import moda

interface Reader {
	read() int
}

struct Local {}

fn (l Local) read() int {
	return 1
}

fn read() int {
	return 9
}

fn call_reader(r Reader) int {
	return r.read()
}

fn main() {
	_ := read()
	_ := call_reader(Local{})
}
'
		'moda/moda.v': 'module moda

pub struct Remote {}

pub fn (r Remote) read() int {
	return 2
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['read']
	assert used['Reader.read']
	used.delete('moda.Remote.read')
	used.delete('Remote.read')
	used.delete('moda__Remote__read')
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('Reader__read(')
	assert !c_code.contains('moda__Remote__read(')
}

fn test_unused_main_method_with_interface_dispatch_is_pruned_with_stub() {
	mut a, mut tc := parse_checked_source('unused_main_method_interface_dispatch_cgen', '
interface Reader {
	read() int
}

struct File {}

fn (f File) read() int {
	return 1
}

struct X {}

fn (x X) unused(r Reader) int {
	return r.read()
}

fn main() {}
')
	mut used := markused.mark_used(a, tc)
	assert !used['X.unused']
	assert !used['Reader.read']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('X__unused(')
	assert !c_code.contains('Reader__read(')
}

fn test_unused_main_helper_with_method_call_is_pruned_with_method() {
	mut a, mut tc := parse_checked_source('unused_main_helper_method_call_cgen',
		'module main\n\nstruct X {}\n\nfn (x X) m() int {\n\treturn 1\n}\n\nfn helper() int {\n\treturn X{}.m()\n}\n\nfn main() {}\n')
	mut used := markused.mark_used(a, tc)
	assert !used['helper']
	assert !used['X.m']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('helper(')
	assert !c_code.contains('X__m(')
}

fn test_reachable_main_fn_literal_is_emitted_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('reachable_main_fn_literal_cgen',
		'module main\n\nfn callback_value(cb fn () int) int {\n\treturn cb()\n}\n\nfn main() {\n\t_ := callback_value(fn () int {\n\t\treturn 7\n\t})\n}\n')
	mut used := markused.mark_used(a, tc)
	assert used['callback_value']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('int __anon_fn_')
	assert c_code.contains('callback_value(__anon_fn_')
}

fn test_flag_default_value_lowering_keeps_escape_helper() {
	mut a, mut tc := parse_checked_source('flag_default_value_escape_helper_cgen',
		'module main\n\nfn escape_default_string(value string) string {\n\treturn value\n}\n\nfn flag_default_value(value string) string {\n\treturn value\n}\n\nfn main() {\n\t_ := flag_default_value("abc")\n}\n')
	mut used := markused.mark_used(a, tc)
	assert used['escape_default_string']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('escape_default_string(')
}

// test_return_local_address_seeds_memdup_runtime_helper validates this v3 regression case.
fn test_return_local_address_seeds_memdup_runtime_helper() {
	used := mark_used_source('return_local_address_memdup', '
struct Box {
	x int
}

fn make_box() &Box {
	b := Box{
		x: 1
	}
	return &b
}

fn main() {
	_ := make_box()
}
')
	assert used['memdup']
}

// test_map_literals_lower_to_new_map_after_used_filter_transform validates this v3 regression case.
fn test_map_literals_lower_to_new_map_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('map_literal_new_map_cgen', '
fn make_map() map[string]int {
	return map[string]int{}
}

fn main() {
	_ := make_map()
}
')
	mut used := markused.mark_used(a, tc)
	assert used['new_map']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('new_map(sizeof(string), sizeof(int)')
}

// test_optional_map_or_lowers_to_new_map_after_used_filter_transform
// validates this v3 regression case.
fn test_optional_map_or_lowers_to_new_map_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('option_map_or_new_map_cgen', '
fn maybe_map() ?map[string]int {
	return none
}

fn main() {
	m := maybe_map() or { return }
	_ := m
}
')
	mut used := markused.mark_used(a, tc)
	assert used['new_map']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('new_map(sizeof(string), sizeof(int)')
}

// test_string_membership_lowers_to_contains_after_used_filter_transform
// validates this v3 regression case.
fn test_string_membership_lowers_to_contains_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('string_membership_contains_cgen', '
fn has_needle() bool {
	return "bc" in "abcd"
}

fn main() {
	_ := has_needle()
}
')
	mut used := markused.mark_used(a, tc)
	assert used['string__contains']
	assert used['string__contains_u8']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('string__contains(')
}

// test_string_compound_assign_lowers_to_plus_after_used_filter_transform
// validates this v3 regression case.
fn test_string_compound_assign_lowers_to_plus_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('string_plus_assign_cgen', '
fn main() {
	mut s := "a"
	s += "b"
	_ := s
}
')
	mut used := markused.mark_used(a, tc)
	assert used['string__plus']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('string__plus(')
}

// test_string_interpolation_lowers_to_formatter_after_used_filter_transform
// validates this v3 regression case.
fn test_string_interpolation_lowers_to_formatter_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('string_interp_formatter_cgen', '
fn main() {
	_ := "\${true}"
}
')
	mut used := markused.mark_used(a, tc)
	assert used['bool.str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('bool__str(')
}

// test_print_bool_lowers_to_formatter_after_used_filter_transform
// validates this v3 regression case.
fn test_print_bool_lowers_to_formatter_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('print_bool_formatter_cgen', '
fn println(s string) {}

fn main() {
	println(true)
}
')
	mut used := markused.mark_used(a, tc)
	assert used['bool.str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('bool__str(')
}

// test_return_local_address_lowers_to_memdup_after_used_filter_transform
// validates this v3 regression case.
fn test_return_local_address_lowers_to_memdup_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('return_local_address_memdup_cgen', '
struct Box {
	x int
}

fn make_box() &Box {
	b := Box{
		x: 1
	}
	return &b
}

fn main() {
	_ := make_box()
}
')
	mut used := markused.mark_used(a, tc)
	assert used['memdup']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('memdup(')
}

// test_map_literal_compile_keeps_new_map_runtime_helper validates this v3 regression case.
fn test_map_literal_compile_keeps_new_map_runtime_helper() {
	v3_bin := build_v3_bin('map_literal_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_map_literal_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_map_literal_input')
	os.write_file(src, '
fn make_map() map[string]int {
	return map[string]int{}
}

fn main() {
	_ := make_map()
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_optional_map_or_compile_keeps_new_map_runtime_helper validates this v3 regression case.
fn test_optional_map_or_compile_keeps_new_map_runtime_helper() {
	v3_bin := build_v3_bin('option_map_or_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_option_map_or_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_option_map_or_input')
	os.write_file(src, '
fn maybe_map() ?map[string]int {
	return none
}

fn main() {
	m := maybe_map() or { return }
	_ := m
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_return_local_address_compile_keeps_memdup_runtime_helper validates this v3 regression case.
fn test_return_local_address_compile_keeps_memdup_runtime_helper() {
	v3_bin := build_v3_bin('return_local_address_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_return_local_address_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_return_local_address_input')
	os.write_file(src, '
struct Box {
	x int
}

fn make_box() &Box {
	b := Box{
		x: 1
	}
	return &b
}

fn main() {
	_ := make_box()
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_print_bool_compile_keeps_formatter_runtime_helper validates this v3 regression case.
fn test_print_bool_compile_keeps_formatter_runtime_helper() {
	v3_bin := build_v3_bin('print_bool_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_print_bool_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_print_bool_input')
	os.write_file(src, '
fn main() {
	println(true)
}
') or { panic(err) }
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_string_plus_compile_keeps_plus_runtime_helper validates this v3 regression case.
fn test_string_plus_compile_keeps_plus_runtime_helper() {
	v3_bin := build_v3_bin('string_plus_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_string_plus_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_string_plus_input')
	os.write_file(src, '
fn main() {
	flag := true
	mut s := "a"
	s += "\${flag}"
	_ := s
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}

// test_string_membership_compile_keeps_contains_runtime_helpers validates this v3 regression case.
fn test_string_membership_compile_keeps_contains_runtime_helpers() {
	v3_bin := build_v3_bin('string_membership_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_string_membership_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_string_membership_input')
	os.write_file(src, '
fn has_needle() bool {
	return "bc" in "abcd"
}

fn main() {
	_ := has_needle()
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
}
