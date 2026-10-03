import os
import strings
import v.flat
import v.gen.c as cgen
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')

// parse_checked_source reads parse checked source input for v3 tests.
fn parse_checked_source(name string, source string) (&flat.FlatAst, &types.TypeChecker) {
	return parse_checked_source_with_unknown_calls(name, source, true)
}

fn parse_checked_source_with_unknown_calls(name string, source string, diagnose_unknown_calls bool) (&flat.FlatAst, &types.TypeChecker) {
	src := os.join_path(os.temp_dir(), 'v3_markused_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	tc.enable_globals = true
	tc.diagnose_unknown_calls = diagnose_unknown_calls
	if diagnose_unknown_calls {
		tc.diagnostic_files[src] = true
	} else {
		tc.diagnostic_files['__external_import_fixture__'] = true
	}
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
	tc.enable_globals = true
	tc.collect(a)
	tc.enable_globals = true
	tc.diagnose_unknown_calls = true
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn parse_checked_project_in_order(name string, rels []string, sources []string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	os.rmdir_all(root) or {}
	mut paths := []string{}
	for i, rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, sources[i]) or { panic(err) }
		paths << path
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	tc.enable_globals = true
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

fn test_trivial_literal_output_prunes_conservative_runtime_helper_seeds() {
	used := mark_used_source('trivial_literal_output', "println('Hello, World!')")
	assert !used['__new_array']
	assert !used['array.get']
	assert !used['array.push']
	assert !used['array.delete_last']
	assert !used['i64.str']
	assert !used['map.clone']
	assert !used['strconv.format_uint']
	assert used['string.free']
}

fn test_nontrivial_output_keeps_conservative_runtime_helper_seeds() {
	used := mark_used_source('nontrivial_output', "
fn message() string {
	return 'Hello, World!'
}

println(message())
")
	assert used['map.clone']
	assert used['strconv.format_uint']
	assert used['array.delete_last']
	assert used['i64.str']
	assert used['v.pref.detect_vroot']
	assert used['v.pref.detect_vexe']
}

fn test_cached_trivial_output_keeps_cached_runtime_helper_seeds() {
	a, tc := parse_checked_source('cached_trivial_output', "println('Hello, World!')")
	used := markused.mark_used_for_cache(a, tc, []string{}, {
		'builtin': true
	})
	assert used['__new_array']
	assert used['array.push']
	assert used['byteptr.vstring_with_len']
	assert used['strconv.format_uint']
}

fn find_fn_node_id(a &flat.FlatAst, name string) int {
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value == name {
			return i
		}
	}
	return -1
}

// test_import_alias_context_is_file_local verifies that declarations retain
// the imports of their own file even when later files reuse or omit an alias.
fn test_import_alias_context_is_file_local() {
	a, tc := parse_checked_project_in_order('file_local_import_context', [
		'main/a.v',
		'main/b.v',
		'main/c.v',
		'left/left.v',
		'right/right.v',
	], [
		'module main

import left as dep

fn selected() dep.Box[int] {
	return dep.make()
}
',
		'module main

import right as dep

fn decoy() dep.Box {
	return dep.make()
}
',
		'module main

fn main() {
	_ := selected()
}
',
		'module left

pub struct Box[T] {
	value T
}

pub fn make() Box[int] {
	return Box[int]{
		value: 1
	}
}
',
		'module right

pub struct Box {
	value int
}

pub fn make() Box {
	return Box{
		value: 2
	}
}
',
	])
	selected_id := find_fn_node_id(a, 'selected')
	assert selected_id >= 0
	selected_dependencies := tc.direct_dependencies(selected_id)
	assert 'left.make' in selected_dependencies
	assert 'right.make' !in selected_dependencies
	used, uses_generics := markused.mark_used_with_generic_usage(a, tc)
	assert used['left.make']
	assert !used['right.make']
	assert uses_generics
}

fn test_module_function_owner_uses_qualified_declaration_key() {
	a, tc := parse_checked_project_in_order('qualified_function_owner', [
		'main/main.v',
		'builtin/compat.v',
		'support/support.v',
	], [
		'module main

import support

fn main() {
	_ := support.decode()
}
',
		'module builtin

fn decode() int {
	return 0
}
',
		'module support

pub fn decode() int {
	return helper()
}

fn helper() int {
	return 1
}
',
	])
	used := markused.mark_used_without_generic_detection(a, tc)
	assert used['support.decode']
	assert used['support.helper']
	assert !used['builtin.decode']
}

// test_eager_markused_import_alias_context_is_file_local covers the eager,
// parallel-capable body precollection path with the same per-file alias reuse.
fn test_eager_markused_import_alias_context_is_file_local() {
	mut first := strings.new_builder(100_000)
	first.writeln('module main')
	first.writeln('import left as dep')
	first.writeln('fn selected() dep.Box[int] { return dep.make() }')
	for i in 0 .. 2050 {
		first.writeln('fn first_pad_${i}() int { return ${i} }')
	}
	mut second := strings.new_builder(100_000)
	second.writeln('module main')
	second.writeln('import right as dep')
	second.writeln('fn decoy() dep.Box { return dep.make() }')
	for i in 0 .. 2050 {
		second.writeln('fn second_pad_${i}() int { return ${i} }')
	}
	a, tc := parse_checked_project_in_order('eager_file_local_import_context', [
		'main/a.v',
		'main/b.v',
		'main/c.v',
		'left/left.v',
		'right/right.v',
	], [first.str(), second.str(), 'module main

fn main() {
	_ := selected()
}
', 'module left

pub struct Box[T] {
	value T
}

pub fn make() Box[int] {
	return Box[int]{value: 1}
}
', 'module right

pub struct Box {
	value int
}

pub fn make() Box {
	return Box{value: 2}
}
'])
	used, uses_generics := markused.mark_used_with_generic_usage(a, tc)
	assert used['left.make']
	assert !used['right.make']
	assert uses_generics
}

fn test_top_level_initializer_keeps_file_import_context() {
	a, tc := parse_checked_project_in_order('top_level_initializer_import_context', [
		'main/main.v',
		'support/support.v',
	], [
		'module main

import support as dep

__global answer = dep.make()

fn main() {
	_ := answer
}
',
		'module support

pub struct Box[T] {
	value T
}

pub fn make() Box[int] {
	return Box[int]{value: 42}
}
',
	])
	used, uses_generics := markused.mark_used_with_generic_usage(a, tc)
	assert used['support.make']
	assert uses_generics
}

fn test_local_generic_struct_init_requires_monomorphization() {
	a, tc := parse_checked_source('local_generic_struct_init', '
fn main() {
	struct Inner {}
	struct Box[T] {}
	_ := Box[Inner]{}
}
')
	_, uses_generics := markused.mark_used_with_generic_usage(a, tc)
	assert uses_generics
}

// build_v3_bin builds v3 bin data for v3 tests.
fn build_v3_bin(name string) string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_markused_${name}')
	build :=
		os.exec([vexe, '-gc', 'none', '-path', '${vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin,
			'${v3_src}'])
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

// test_prepared_markused_scans_runtime_helpers_after_semantic_check validates
// that declaration preparation never captures incomplete semantic results.
fn test_prepared_markused_scans_runtime_helpers_after_semantic_check() {
	src := os.join_path(os.temp_dir(), 'v3_markused_prepared_runtime_helpers.v')
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
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	tc.diagnostic_files[src] = true
	prepared_thread := spawn markused.prepare_markused_declarations(a, &tc, true)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	mut prepared := prepared_thread.wait()
	used := markused.mark_used_without_generic_detection_prepared(a, &tc, mut prepared)
	prepared.release()
	assert used['new_map']
}

fn test_prepared_markused_combines_exact_and_lowered_dependencies() {
	src := os.join_path(os.temp_dir(), 'v3_markused_prepared_exact_edges.v')
	os.write_file(src, '
fn direct_target() {}
fn callback_target() {}
fn take_callback(callback fn ()) { callback() }
fn default_target() int { return 1 }

struct Config {
	value int = default_target()
}

fn main() {
	direct_target()
	take_callback(callback_target)
	_ := Config{}
}
') or {
		panic(err)
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	tc.diagnostic_files[src] = true
	prepared_thread := spawn markused.prepare_markused_declarations(a, &tc, true)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	mut prepared := prepared_thread.wait()
	used := markused.mark_used_without_generic_detection_prepared(a, &tc, mut prepared)
	prepared.release()
	assert used['direct_target']
	assert used['callback_target']
	assert used['default_target']
}

fn test_non_generic_repeated_calls_keep_each_modules_dependencies() {
	a, tc := parse_checked_project('repeated_module_edges_${os.getpid()}', {
		'main.v':    'module main\nimport one\nimport two\nfn main() { one.first() one.second() two.first() two.second() }'
		'one/one.v': 'module one\npub fn first() { leaf() }\npub fn second() { leaf() }\nfn leaf() { tail() }\nfn tail() {}\nfn unused() {}'
		'two/two.v': 'module two\npub fn first() { leaf() }\npub fn second() { leaf() }\nfn leaf() { tail() }\nfn tail() {}\nfn unused() {}'
	}, 'main.v')
	used := markused.mark_used_without_generic_detection(a, tc)
	for mod in ['one', 'two'] {
		for name in ['first', 'second', 'leaf', 'tail'] {
			assert used['${mod}.${name}']
		}
		assert !used['${mod}.unused']
	}
}

fn test_self_typed_default_collects_explicit_initializer_calls() {
	used := mark_used_source('self_typed_default_explicit_call', '
interface Value {}

struct End {}

struct S {
	inner Value = S{
		inner: make()
	}
}

fn make() Value {
	return End{}
}

fn main() {
	_ := S{}
}
')
	assert used['make']
}

fn test_self_typed_default_collects_nested_omitted_default_calls() {
	used := mark_used_source('self_typed_default_nested_omitted_call', '
interface Value {}

struct End {}

struct S {
	inner Value = S{
		inner: End{}
	}
	token int = make()
}

fn make() int {
	return 7
}

fn main() {
	_ := S{
		token: 1
	}
}
')
	assert used['make']
}

fn test_array_defaults_stop_at_nested_dynamic_elements() {
	cases := {
		'[][]Box{len: 1}':       false
		'[][][]Box{len: 1}':     false
		'[2][]Box{}':            false
		'[][2][]Box{len: 1}':    false
		'[][2][2][]Box{len: 1}': false
		'[]Box{len: 1}':         true
		'[2]Box{}':              true
		'[][2]Box{len: 1}':      true
		'[2][2]Box{}':           true
		'Box{}':                 true
	}
	for literal, needs_defaults in cases {
		a, tc := parse_checked_source('nested_array_default_${os.getpid()}', '
struct Box {
	value int = default_value()
}

fn default_value() int { return 7 }

fn main() {
	values := ${literal}
	_ := values
}
')
		used := markused.mark_used(a, tc)
		assert used['default_value'] == needs_defaults, literal
		without_generics := markused.mark_used_without_generic_detection(a, tc)
		assert without_generics['default_value'] == needs_defaults, literal
	}
}

fn test_nested_dynamic_array_drops_unresolved_default_call() {
	v3_bin := build_v3_bin('nested_dynamic_array_default_${os.getpid()}')
	source := os.join_path(os.temp_dir(), 'v3_markused_nested_dynamic_default_${os.getpid()}.c.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_nested_dynamic_default_${os.getpid()}')
	defer {
		os.rm(source) or {}
		os.rm(bin) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(source, '
fn C.v3_markused_missing_default_symbol() int

fn default_value() int {
	return C.v3_markused_missing_default_symbol()
}

struct Box {
	value int = default_value()
}

fn main() {
	values := [][]Box{len: 1}
	assert values.len == 1
	assert values[0].len == 0
	println("ok")
}
') or { panic(err) }
	compiled := os.exec([v3_bin, '-o', bin, source])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([bin])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'ok', ran.output
	os.write_file(source, '
fn default_value() int { return 7 }

struct Box {
	value int = default_value()
}

fn main() {
	direct := []Box{len: 1}
	fixed := [2]Box{}
	nested := [][2][2]Box{len: 1}
	assert direct[0].value == 7
	assert fixed[1].value == 7
	assert nested[0][1][1].value == 7
	println("ok")
}
') or { panic(err) }
	defaults_compiled := os.exec([v3_bin, '-o', bin, source])
	assert defaults_compiled.exit_code == 0, defaults_compiled.output
	defaults_ran := os.exec([bin])
	assert defaults_ran.exit_code == 0, defaults_ran.output
	assert defaults_ran.output.trim_space() == 'ok', defaults_ran.output
}

fn test_array_defaults_only_keep_implicitly_initialized_elements() {
	cases := {
		'Rows{}':                                 false
		'Rows{cap: 10}':                          false
		'Rows{len: 1}':                           true
		'Rows{len: 1, init: Box{value: 1}}':      false
		'[]Rows{len: 1}':                         false
		'FixedAlias{init: Box{value: 1}}':        false
		'FixedAlias{}':                           true
		'[Box{value: 1}]!':                       false
		'[Box{}]!':                               true
		'[]Box{}':                                false
		'[]Box{cap: 10}':                         false
		'[]Box{len: 1, init: Box{value: 1}}':     false
		'[]Box{len: 1, init: explicit_box()}':    false
		'[][2]Box{}':                             false
		'[][2]Box{cap: 10}':                      false
		'[][2]Box{len: 1, init: explicit_row()}': false
		'[2]Box{init: Box{value: 1}}':            false
		'[]Box{len: 1}':                          true
		'[]Box{len: 1, init: Box{}}':             true
		'[][2]Box{len: 1}':                       true
		'[2]Box{}':                               true
	}
	for literal, needs_defaults in cases {
		for top_level in [false, true] {
			initializer := if top_level {
				'__global values = ${literal}\nfn main() { _ := values }'
			} else {
				'fn main() { values := ${literal}; _ := values }'
			}
			a, tc := parse_checked_source('implicit_array_elements_${os.getpid()}', '
struct Box {
	value int = default_value()
}
type FixedAlias = [2]Box
type Rows = []Box
fn default_value() int { return 7 }
fn explicit_box() Box { return Box{value: 1} }
fn explicit_row() [2]Box { return [2]Box{init: Box{value: 1}} }
${initializer}
')
			used := markused.mark_used(a, tc)
			assert used['default_value'] == needs_defaults, '${literal}, global: ${top_level}'
			without_generics := markused.mark_used_without_generic_detection(a, tc)
			assert without_generics['default_value'] == needs_defaults, '${literal}, global: ${top_level}'
			if literal.contains('explicit_box()') {
				assert used['explicit_box']
				assert without_generics['explicit_box']
			}
		}
	}
}

fn test_alias_value_defaults_keep_underlying_struct_calls() {
	for literal in ['Alias{}', '[]Alias{len: 1}', '[2]Alias{}', '[][2]Alias{len: 1}', 'Wrapper{}',
		'[]Wrapper{len: 1}', 'FixedWrapper{}', '[]FixedAlias{len: 1}', 'GenericAlias{}',
		'[]GenericAlias{len: 1}'] {
		for top_level in [false, true] {
			initializer := if top_level {
				'__global values = ${literal}\nfn main() { _ := values }'
			} else {
				'fn main() { values := ${literal} _ := values }'
			}
			a, tc := parse_checked_source('alias_value_defaults_${os.getpid()}', '
struct Box { value int = default_value() }
type FirstAlias = Box
type Alias = FirstAlias
struct Wrapper { box Alias }
type FixedAlias = [2]Alias
struct FixedWrapper { boxes FixedAlias }
struct GenericBox[T] { value int = default_value() item T }
type GenericAlias = GenericBox[int]
fn default_value() int { return 7 }
${initializer}
')
			used := markused.mark_used(a, tc)
			assert used['default_value'], '${literal}, global: ${top_level}'
			without_generics := markused.mark_used_without_generic_detection(a, tc)
			assert without_generics['default_value'], '${literal}, global: ${top_level}'
		}
	}
}

fn test_generic_omitted_fields_keep_concrete_defaults() {
	cases := {
		'Outer[Inner]{}':                       true
		'Outer[Outer[Inner]]{}':                true
		'[]Outer[Inner]{len: 1}':               true
		'[2]Outer[Inner]{}':                    true
		'OuterAlias{}':                         true
		'[]OuterAlias{len: 1}':                 true
		'Outer[InnerAlias]{}':                  true
		'Outer[[2]Inner]{}':                    true
		'Outer[[]Inner]{}':                     false
		'Outer[?Inner]{}':                      false
		'Outer[map[string]Inner]{}':            false
		'Outer[Inner]{inner: Inner{value: 1}}': false
	}
	for literal, needs_defaults in cases {
		for top_level in [false, true] {
			initializer := if top_level {
				'__global value = ${literal}\nfn main() { _ := value }'
			} else {
				'fn main() { value := ${literal}; _ := value }'
			}
			a, tc := parse_checked_source('generic_omitted_defaults_${os.getpid()}', '
struct Inner { value int = default_value() }
type InnerAlias = Inner
struct Outer[T] { inner T }
type OuterAlias = Outer[Inner]
fn default_value() int { return 7 }
${initializer}
')
			used := markused.mark_used(a, tc)
			assert used['default_value'] == needs_defaults, '${literal}, global: ${top_level}'
			without_generics := markused.mark_used_without_generic_detection(a, tc)
			assert without_generics['default_value'] == needs_defaults, '${literal}, global: ${top_level}'
		}
	}
}

fn test_distinct_generic_nested_defaults_keep_each_specialization() {
	a, tc := parse_checked_source('distinct_generic_defaults_${os.getpid()}', '
struct First { value int = first_value() }
struct Second { value int = second_value() }
struct Outer[T] { inner T }
struct Wrapper[T] { outer Outer[T] }
fn first_value() int { return 7 }
fn second_value() int { return 9 }
fn main() {
	_ := Wrapper[First]{}
	_ := Wrapper[Second]{}
}
')
	used := markused.mark_used(a, tc)
	assert used['first_value']
	assert used['second_value']
	without_generics := markused.mark_used_without_generic_detection(a, tc)
	assert without_generics['first_value']
	assert without_generics['second_value']
}

fn test_explicit_generic_field_defaults_keep_concrete_defaults() {
	cases := {
		'Wrapper[Inner]{}':                                               true
		'Wrapper[Inner]{outer: Explicit[Inner]{inner: Inner{value: 1}}}': false
	}
	for literal, needs_defaults in cases {
		for top_level in [false, true] {
			initializer := if top_level {
				'__global value = ${literal}\nfn main() { _ := value }'
			} else {
				'fn main() { value := ${literal}; _ := value }'
			}
			a, tc := parse_checked_source('explicit_generic_defaults_${os.getpid()}', '
struct Inner { value int = default_value() }
struct Explicit[T] { inner T }
struct Wrapper[T] { outer Explicit[T] = Explicit[T]{} }
fn default_value() int { return 7 }
${initializer}
')
			used := markused.mark_used(a, tc)
			assert used['default_value'] == needs_defaults, '${literal}, global: ${top_level}'
			without_generics := markused.mark_used_without_generic_detection(a, tc)
			assert without_generics['default_value'] == needs_defaults, '${literal}, global: ${top_level}'
		}
	}
}

fn test_generic_default_fields_follow_declared_parameter_positions() {
	cases := {
		'Pair[int, First]{}':      [true, false]
		'Pair[Second, int]{}':     [false, true]
		'Pair[First, []Second]{}': [true, false]
		'Pair[[]First, Second]{}': [false, true]
	}
	for literal, expected in cases {
		a, tc := parse_checked_source('generic_parameter_defaults_${os.getpid()}', '
struct First { value int = first_value() }
struct Second { value int = second_value() }
struct Pair[L, R] { left L right R }
fn first_value() int { return 7 }
fn second_value() int { return 9 }
fn main() { _ := ${literal} }
')
		used := markused.mark_used(a, tc)
		assert used['first_value'] == expected[0], literal
		assert used['second_value'] == expected[1], literal
		without_generics := markused.mark_used_without_generic_detection(a, tc)
		assert without_generics['first_value'] == expected[0], literal
		assert without_generics['second_value'] == expected[1], literal
	}
}

fn test_imported_generic_default_fields_keep_concrete_owner() {
	cases := {
		'dep.Outer[Inner]{}':            [true, false]
		'dep.Outer[dep.Outer[Inner]]{}': [true, false]
		'dep.Outer[[2]Inner]{}':         [true, false]
		'[]dep.Outer[Inner]{len: 1}':    [true, false]
		'dep.Outer[dep.Inner]{}':        [false, true]
		'dep.Holder{}':                  [false, true]
		'dep.Mixed[int]{}':              [false, true]
		'dep.Mixed[Inner]{}':            [true, true]
	}
	for literal, expected in cases {
		a, tc := parse_checked_project_in_order('imported_generic_defaults_${os.getpid()}', [
			'main/main.v',
			'worker/worker.v',
			'leaf/leaf.v',
		], [
			'module main
import worker as dep
struct Inner { value int = main_value() }
fn main_value() int { return 7 }
fn main() { _ := ${literal} }
',
			'module worker
import leaf as main
pub struct Outer[T] { pub: inner T }
pub struct T { pub: value int }
pub struct Inner { pub: value int = main.make() }
pub struct Holder { pub: inner Inner }
pub struct Mixed[T] { pub: inner T local Inner }
',
			'module leaf
pub fn make() int { return 9 }
',
		])
		used := markused.mark_used(a, tc)
		assert used['main_value'] == expected[0], literal
		assert used['leaf.make'] == expected[1], literal
		without_generics := markused.mark_used_without_generic_detection(a, tc)
		assert without_generics['main_value'] == expected[0], literal
		assert without_generics['leaf.make'] == expected[1], literal
	}
}

fn test_generic_omitted_field_defaults_compile_and_run() {
	v3_bin := build_v3_bin('generic_omitted_defaults_${os.getpid()}')
	source := os.join_path(os.temp_dir(), 'v3_markused_generic_defaults_${os.getpid()}.c.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_generic_defaults_run_${os.getpid()}')
	defer {
		os.rm(source) or {}
		os.rm(bin) or {}
		os.rm(v3_bin) or {}
	}
	cases := {
		'Outer[Inner]{}':                   'value.inner.value'
		'Outer[Outer[Inner]]{}':            'value.inner.inner.value'
		'[]Outer[Inner]{len: 1}':           'value[0].inner.value'
		'ExplicitWrapper[Inner]{}':         'value.outer.inner.value'
		'[]ExplicitWrapper[Inner]{len: 1}': 'value[0].outer.inner.value'
		'OuterAlias{}':                     'value.inner.value'
		'[]OuterAlias{len: 1}':             'value[0].inner.value'
		'Outer[InnerAlias]{}':              'value.inner.value'
		'Outer[[2]Inner]{}':                'value.inner[1].value'
	}
	for literal, element in cases {
		os.write_file(source, '
fn default_value() int { return 7 }
struct Inner { value int = default_value() }
type InnerAlias = Inner
struct Outer[T] { inner T }
struct ExplicitWrapper[T] { outer Outer[T] = Outer[T]{} }
type OuterAlias = Outer[Inner]
fn main() {
	value := ${literal}
	assert ${element} == 7
	println("ok")
}
') or { panic(err) }
		compiled := os.exec([v3_bin, '-gc', 'none', '-o', bin, source])
		assert compiled.exit_code == 0, '${literal}: ${compiled.output}'
		ran := os.exec([bin])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'ok', ran.output
	}
	os.write_file(source, '
fn C.v3_generic_defaults_unused_symbol() int
fn default_value() int { return C.v3_generic_defaults_unused_symbol() }
struct Inner { value int = default_value() }
struct Outer[T] { inner T }
struct ExplicitWrapper[T] { outer Outer[T] = Outer[T]{} }
fn main() {
	array := Outer[[]Inner]{}
	option := Outer[?Inner]{}
	mapping := Outer[map[string]Inner]{}
	explicit := Outer[Inner]{inner: Inner{value: 1}}
	wrapper := ExplicitWrapper[Inner]{outer: Outer[Inner]{inner: Inner{value: 1}}}
	assert array.inner.len == 0
	assert option.inner == none
	assert mapping.inner.len == 0
	assert explicit.inner.value == 1
	assert wrapper.outer.inner.value == 1
	println("ok")
}
') or { panic(err) }
	compiled := os.exec([v3_bin, '-gc', 'none', '-o', bin, source])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([bin])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'ok', ran.output
}

fn test_imported_generic_omitted_defaults_compile_and_run() {
	v3_bin := build_v3_bin('imported_generic_omitted_defaults_${os.getpid()}')
	root := os.join_path(os.temp_dir(), 'v3_markused_imported_generic_defaults_${os.getpid()}')
	bin := root + '_run'
	defer {
		os.rmdir_all(root) or {}
		os.rm(bin) or {}
		os.rm(v3_bin) or {}
	}
	os.mkdir_all(os.join_path(root, 'worker')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'leaf')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'generic_defaults' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), '
module main
import worker as dep
struct Inner { value int = main_value() }
fn main_value() int { return 7 }
fn main() {
	local := dep.Outer[dep.Outer[Inner]]{}
	remote := dep.Outer[dep.Inner]{}
	fixed := dep.Outer[[2]Inner]{}
	local_holder := dep.Holder{}
	mixed := dep.Mixed[Inner]{}
	assert local.inner.inner.value == 7
	assert remote.inner.value == 9
	assert fixed.inner[1].value == 7
	assert local_holder.inner.value == 9
	assert mixed.inner.value == 7
	assert mixed.local.value == 9
	println("ok")
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'worker', 'worker.v'), '
module worker
import leaf as defaults
pub struct Outer[T] { pub: inner T }
pub struct Inner { pub: value int = defaults.make() }
pub struct Holder { pub: inner Inner }
pub struct Mixed[T] { pub: inner T local Inner }
') or { panic(err) }
	os.write_file(os.join_path(root, 'leaf', 'leaf.v'), '
module leaf
pub fn make() int { return 9 }
') or { panic(err) }
	compiled := os.exec([v3_bin, '-gc', 'none', '-o', bin, root])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([bin])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'ok', ran.output
}

fn test_alias_struct_function_defaults_keep_dependencies() {
	for literal in ['Alias{}', '[]Alias{len: 1}', '[2]Alias{}', 'Wrapper{}'] {
		a, tc := parse_checked_source('alias_function_defaults_${os.getpid()}', '
struct Box { reader fn () int = default_reader }
type Alias = Box
struct Wrapper { box Alias }
fn default_reader() int { return leaf() }
fn leaf() int { return 7 }
fn main() { values := ${literal} _ := values }
')
		used := markused.mark_used(a, tc)
		assert used['default_reader'], literal
		assert used['leaf'], literal
		without_generics := markused.mark_used_without_generic_detection(a, tc)
		assert without_generics['default_reader'], literal
		assert without_generics['leaf'], literal
	}
}

fn test_alias_wrappers_do_not_keep_underlying_struct_defaults() {
	for literal in ['[]ArrayAlias{len: 1}', '[]MapAlias{len: 1}', '[]OptionAlias{len: 1}',
		'[]PointerAlias{len: 1}', 'Wrapper{}'] {
		a, tc := parse_checked_source('alias_wrapper_defaults_${os.getpid()}', '
struct Box { value int = default_value() }
fn default_value() int { return 7 }
type ArrayAlias = []Box
type MapAlias = map[string]Box
type OptionAlias = ?Box
type PointerAlias = &Box
struct Wrapper { values ArrayAlias mapping MapAlias maybe OptionAlias }
fn main() { values := ${literal} _ := values }
')
		used := markused.mark_used(a, tc)
		assert !used['default_value'], literal
		without_generics := markused.mark_used_without_generic_detection(a, tc)
		assert !without_generics['default_value'], literal
	}
}

fn test_imported_alias_defaults_keep_declaration_import_context() {
	a, tc := parse_checked_project_in_order('imported_alias_defaults_${os.getpid()}', [
		'main/a.v',
		'main/b.v',
		'worker/worker.v',
		'leaf/leaf.v',
		'decoy/decoy.v',
	], [
		'module main
import worker as dep
struct Wrapper { box dep.Alias }
__global global_boxes = []dep.Alias{len: 1}
fn main() { _ := global_boxes _ := Wrapper{} _ := [2]dep.Alias{} }
',
		'module main
import decoy as dep
fn unused() { _ := dep.Box{} }
',
		'module worker
import leaf as defaults
pub struct Box[T] { pub: value int = defaults.make() item T }
pub type FirstAlias = Box[int]
pub type Alias = FirstAlias
',
		'module leaf
pub fn make() int { return 7 }
',
		'module decoy
pub struct Box { pub: value int = make() }
fn make() int { return 9 }
',
	])
	used := markused.mark_used(a, tc)
	assert used['leaf.make']
	assert !used['decoy.make']
	without_generics := markused.mark_used_without_generic_detection(a, tc)
	assert without_generics['leaf.make']
	assert !without_generics['decoy.make']
}

fn test_array_initializers_link_without_unused_defaults() {
	v3_bin := build_v3_bin('unused_array_defaults_${os.getpid()}')
	source := os.join_path(os.temp_dir(), 'v3_markused_unused_array_defaults_${os.getpid()}.c.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_unused_array_defaults_run_${os.getpid()}')
	defer {
		os.rm(source) or {}
		os.rm(bin) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(source, '
fn C.v3_markused_unused_array_default_symbol() int
fn default_value() int { return C.v3_markused_unused_array_default_symbol() }
struct Box { value int = default_value() }
type FixedAlias = [2]Box
type Rows = []Box
fn explicit_box() Box { return Box{value: 3} }
fn main() {
	empty := []Box{}
	capacity := []Box{cap: 10}
	explicit := []Box{len: 1, init: Box{value: 1}}
	called := []Box{len: 1, init: explicit_box()}
	nested_empty := [][2]Box{}
	nested_capacity := [][2]Box{cap: 10}
	fixed_explicit := [2]Box{init: Box{value: 2}}
	raw_explicit := [Box{value: 4}]!
	alias_empty := Rows{}
	alias_capacity := Rows{cap: 10}
	alias_explicit := Rows{len: 1, init: Box{value: 6}}
	assert empty.len == 0
	assert capacity.len == 0
	assert explicit[0].value == 1
	assert called[0].value == 3
	assert nested_empty.len == 0
	assert nested_capacity.len == 0
	assert fixed_explicit[1].value == 2
	assert raw_explicit[0].value == 4
	assert alias_empty.len == 0
	assert alias_capacity.len == 0
	assert alias_explicit[0].value == 6
	println("ok")
}
') or { panic(err) }
	compiled := os.exec([v3_bin, '-gc', 'none', '-o', bin, source])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([bin])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'ok', ran.output
}

fn test_alias_value_defaults_compile_and_run() {
	v3_bin := build_v3_bin('alias_array_defaults_${os.getpid()}')
	source := os.join_path(os.temp_dir(), 'v3_markused_alias_array_defaults_${os.getpid()}.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_alias_array_defaults_run_${os.getpid()}')
	defer {
		os.rm(source) or {}
		os.rm(bin) or {}
		os.rm(v3_bin) or {}
	}
	cases := {
		'FixedAlias{}':           'values[1].value'
		'Rows{len: 1}':           'values[0].value'
		'[]Alias{len: 1}':        'values[0].value'
		'[2]Alias{}':             'values[1].value'
		'[][2]Alias{len: 1}':     'values[0][1].value'
		'Wrapper{}':              'values.box.value'
		'FixedWrapper{}':         'values.boxes[1].value'
		'[]FixedAlias{len: 1}':   'values[0][1].value'
		'[]GenericAlias{len: 1}': 'values[0].value'
	}
	for literal, element in cases {
		os.write_file(source, '
fn default_value() int { return 7 }
struct Box { value int = default_value() }
type FirstAlias = Box
type Alias = FirstAlias
struct Wrapper { box Alias }
type FixedAlias = [2]Alias
type Rows = []Box
struct FixedWrapper { boxes FixedAlias }
struct GenericBox[T] { value int = default_value() item T }
type GenericAlias = GenericBox[int]
fn main() {
	values := ${literal}
	assert ${element} == 7
	println("ok")
}
') or { panic(err) }
		compiled := os.exec([v3_bin, '-gc', 'none', '-o', bin, source])
		assert compiled.exit_code == 0, '${literal}: ${compiled.output}'
		ran := os.exec([bin])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'ok', ran.output
	}
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

fn test_implicit_interface_str_dispatch_seeds_array_helpers() {
	source := '
interface Printable {
	str() string
}

struct Foo {
	xs []int
}

fn main() {
	println(Printable(Foo{
		xs: [1, 2]
	}).str())
}
'
	mut a, mut tc := parse_checked_source('implicit_interface_str_dispatch_array_helpers', source)
	mut used := markused.mark_used(a, tc)
	assert used['string__plus']
	assert used['array.get']
	assert used['int__str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('_iface_str_arr_'), c_code
}

fn test_implicit_interface_str_dispatch_seeds_optional_payload_helpers() {
	source := '
interface Printable {
	str() string
}

struct Foo {
	maybe ?u64
}

fn main() {
	println(Printable(Foo{
		maybe: ?u64(7)
	}).str())
}
'
	mut a, mut tc := parse_checked_source('implicit_interface_str_dispatch_optional_helpers',
		source)
	used := markused.mark_used(a, tc)
	assert used['string__plus']
	assert used['u64__str']
}

fn test_generic_struct_operator_roots_operator_dependencies() {
	used := mark_used_source('generic_struct_operator_dependencies', '
struct Time {
	seconds int
}

fn (t Time) unix() int {
	return t.seconds
}

fn (a Time) < (b Time) bool {
	return a.unix() < b.unix()
}

fn min[T](a T, b T) T {
	if a < b {
		return a
	}
	return b
}

fn main() {
	a := Time{
		seconds: 1
	}
	b := Time{
		seconds: 2
	}
	_ := min(a, b)
}
')
	assert used['Time.<']
	assert used['Time.unix']
}

fn test_for_in_const_array_roots_receiver_method() {
	used := mark_used_source('for_in_const_array_receiver_method', '
enum FlagId {
	after_context
}

struct Doc {}

const flags = [FlagId.after_context]

fn (id FlagId) doc_short() Doc {
	_ := id
	return Doc{}
}

fn (d Doc) replace(from string, to string) string {
	_ := d
	_ := from
	_ := to
	return "Show n lines after each match."
}

fn main() {
	for flag in flags {
		_ := flag.doc_short().replace("NUM", "n")
	}
}
')
	assert used['FlagId.doc_short']
	assert used['Doc.replace']
}

fn test_error_argument_roots_nested_receiver_and_static_methods() {
	used := mark_used_source('error_arg_nested_receiver_static_methods', '
struct ErrorKind {}

struct GlobError {
	kind ErrorKind
}

struct Parser {}

fn ErrorKind.unopened_alternates() ErrorKind {
	return ErrorKind{}
}

fn (e GlobError) msg() string {
	_ := e
	return "unopened alternates"
}

fn (p Parser) mk_error(kind ErrorKind) GlobError {
	return GlobError{
		kind: kind
	}
}

fn (mut p Parser) pop_alternate() ! {
	return error(p.mk_error(ErrorKind.unopened_alternates()).msg())
}

fn main() {
	mut p := Parser{}
	p.pop_alternate() or { return }
}
')
	assert used['Parser.mk_error']
	assert used['GlobError.msg']
	assert used['ErrorKind.unopened_alternates']
}

fn test_nested_fn_literal_roots_callback_helper_dependencies() {
	used := mark_used_source('nested_fn_literal_callback_helpers', '
struct Match {}

fn Match.new() Match {
	return Match{}
}

struct Caps {}

fn Caps.overall(m Match) Caps {
	_ := m
	return Caps{}
}

fn (c Caps) get(i int) ?Match {
	_ := c
	_ := i
	return Match{}
}

fn no_index(name string) ?int {
	_ := name
	return none
}

fn append_match(mut dst []u8, m Match) {
	_ := m
	dst << u8(1)
}

fn interpolate(append fn (int, mut []u8), name_to_index fn (string) ?int, mut dst []u8) {
	_ := name_to_index
	append(0, mut dst)
}

fn find_iter(matched fn (Match) bool) {
	_ := matched(Match.new())
}

fn replace(mut dst []u8) {
	find_iter(fn [mut dst] (m Match) bool {
		caps := Caps.overall(m)
		interpolate(fn [caps] (i int, mut out []u8) {
			cap_match := caps.get(i) or {
				return
			}
			append_match(mut out, cap_match)
		}, no_index, mut dst)
		return true
	})
}

fn main() {
	mut dst := []u8{}
	replace(mut dst)
}
')
	assert used['Caps.overall']
	assert used['Caps.get']
	assert used['append_match']
	assert used['interpolate']
	assert used['no_index']
	assert used['Match.new']
}

fn test_imported_nested_fn_literal_roots_private_callback_helpers() {
	a, tc := parse_checked_project('imported_nested_fn_literal_callback_helpers', {
		'main.v':     'module main

import worker

fn main() {
	mut dst := []u8{}
	worker.replace(mut dst)
}
'
		'worker/w.v': 'module worker

struct Match {}

fn Match.new() Match {
	return Match{}
}

struct Caps {}

fn Caps.overall(m Match) Caps {
	_ := m
	return Caps{}
}

fn (c Caps) get(i int) ?Match {
	_ := c
	_ := i
	return Match{}
}

fn no_index(name string) ?int {
	_ := name
	return none
}

fn append_match(mut dst []u8, m Match) {
	_ := m
	dst << u8(1)
}

fn interpolate(append fn (int, mut []u8), name_to_index fn (string) ?int, mut dst []u8) {
	_ := name_to_index
	append(0, mut dst)
}

fn find_iter(matched fn (Match) bool) {
	_ := matched(Match.new())
}

pub fn replace(mut dst []u8) {
	find_iter(fn [mut dst] (m Match) bool {
		caps := Caps.overall(m)
		interpolate(fn [caps] (i int, mut out []u8) {
			cap_match := caps.get(i) or {
				return
			}
			append_match(mut out, cap_match)
		}, no_index, mut dst)
		return true
	})
}
'
	}, 'main.v')
	used := markused.mark_used(a, tc)
	assert used['worker.Caps.overall']
	assert used['worker.Caps.get']
	assert used['worker.append_match']
	assert used['worker.interpolate']
	assert used['worker.no_index']
	assert used['worker.Match.new']
}

fn test_map_str_seeds_string_plus_runtime_helper() {
	used := mark_used_source('map_str_string_plus', '
fn render(m map[string]int) string {
	return m.str()
}

fn main() {
	_ := render(map[string]int{})
}
')
	assert used['string__plus']
}

fn test_print_map_seeds_string_plus_runtime_helper() {
	used := mark_used_source('print_map_string_plus', '
fn main() {
	m := {
		"a": 1
	}
	println(m)
}
')
	assert used['string__plus']
}

fn test_moduleless_export_after_module_file_is_rooted() {
	a, tc := parse_checked_project_in_order('moduleless_export_after_module', [
		'helper/helper.v',
		'lonely.v',
	], ['module helper

pub fn helper_marker() int {
	return 1
}
', "@[export: 'raw_lonely']
fn lonely() int {
	return 7
}
"])
	used := markused.mark_used(a, tc)
	assert used['lonely']
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

fn test_nested_field_receiver_method_does_not_root_same_suffix_methods() {
	used := mark_used_source('nested_field_receiver_method', '
struct Flags {}

fn (mut flags Flags) set() {}

struct Holder {
mut:
	flags Flags
}

struct Unrelated {}

fn (mut item Unrelated) set() {}

fn (mut holder Holder) update() {
	holder.flags.set()
}

fn main() {
	mut holder := Holder{}
	holder.update()
}
')
	assert used['Flags.set']
	assert !used['Unrelated.set']
}

fn test_nested_flag_enum_intrinsic_does_not_root_same_suffix_methods() {
	used := mark_used_source('nested_flag_enum_intrinsic', '
@[flag]
enum Flags {
	active
}

struct Holder {
mut:
	flags Flags
}

struct Unrelated {}

fn (mut item Unrelated) set(flag Flags) {}

fn (mut holder Holder) update() {
	holder.flags.set(.active)
}

fn main() {
	mut holder := Holder{}
	holder.update()
}
')
	assert !used['Unrelated.set']
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

fn test_module_global_interface_dispatch_keeps_dispatch_stub() {
	mut a, mut tc := parse_checked_project('module_global_interface_dispatch', {
		'main.v':               'module main

import m

fn main() {
	m.error("x")
}
'
		'm/default.c.v':        'module m

__global default_logger &Logger

pub fn error(s string) {
	default_logger.error(s)
}
'
		'm/logger_interface.v': 'module m

pub enum Level {
	info
}

pub interface Logger {
	get_level() Level
mut:
	fatal(s string)
	error(s string)
	warn(s string)
	info(s string)
	debug(s string)
	set_level(level Level)
	set_always_flush(should_flush bool)
	free()
}
'
		'm/safe_log.v':         'module m

pub struct BaseLog {}

pub struct ThreadSafeLog {
	BaseLog
}

pub fn (l BaseLog) get_level() Level {
	_ := l
	return .info
}

pub fn (mut l ThreadSafeLog) fatal(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) error(s string) {}

pub fn (mut l ThreadSafeLog) warn(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) info(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) debug(s string) {
	_ := l
	_ := s
}

pub fn (mut l ThreadSafeLog) set_level(level Level) {
	_ := l
	_ := level
}

pub fn (mut l ThreadSafeLog) set_always_flush(should_flush bool) {
	_ := l
	_ := should_flush
}

pub fn (mut l ThreadSafeLog) free() {
	_ := l
}
'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	assert used['m.Logger.error']
	assert used['m.ThreadSafeLog.error']
	assert used['m.BaseLog.get_level'] == false
	used = transform.transform_with_used(mut a, tc, used)
	assert used['m.Logger.error']
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('m__Logger__error(')
	assert c_code.contains(': m__ThreadSafeLog__error')
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
	assert c_code.contains('i64 __anon_fn_')
	assert c_code.contains('callback_value(__anon_fn_')
}

fn test_reachable_imported_fn_literal_roots_private_callback_helpers() {
	mut a, mut tc := parse_checked_project('reachable_imported_fn_literal_helpers', {
		'printer/printer.v': 'module printer\n\nfn helper() int {\n\treturn 41\n}\n\nfn named(name string) int {\n\treturn name.len\n}\n\nfn consume(cb fn () int, name_to_index fn (string) int) int {\n\treturn cb() + name_to_index("x")\n}\n\npub fn run() int {\n\treturn consume(fn () int {\n\t\treturn helper()\n\t}, named)\n}\n'
		'main.v':            'module main\n\nimport printer\n\nfn main() {\n\t_ := printer.run()\n}\n'
	}, 'main.v')
	mut used := markused.mark_used(a, tc)
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('printer__helper('), c_code
	assert c_code.contains('printer__named('), c_code
	assert c_code.contains('printer__consume(printer____anon_fn_'), c_code
}

fn test_top_level_fn_value_roots_helper() {
	mut a, mut tc := parse_checked_source('top_level_fn_value_helper_cgen',
		'module main\n\nfn helper() int {\n\treturn 41\n}\n\nf := helper\n_ = f()\n')
	mut used := markused.mark_used(a, tc)
	assert used['helper']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('helper(')
}

fn test_top_level_fn_value_compile_keeps_helper() {
	v3_bin := build_v3_bin('top_level_fn_value_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_top_level_fn_value_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_top_level_fn_value_input')
	os.write_file(src, '
fn helper() int {
	return 41
}

f := helper
println(f() + 1)
') or {
		panic(err)
	}
	compile := os.exec([v3_bin, '-b', 'c', '-o', bin, '${src}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('helper('), c_code
}

fn test_nonlocal_sum_receiver_method_is_collected_before_variant_helpers() {
	used := mark_used_source('nonlocal_sum_receiver_method', '
struct A {}
struct B {}

type Choice = A | B

fn make_choice() Choice {
	return Choice(A{})
}

fn (choice Choice) foo() int {
	return 7
}

fn (a A) foo() int {
	return 1
}

fn (b B) foo() int {
	return 2
}

fn main() {
	_ := make_choice().foo()
}
')
	assert used['Choice.foo'] || used['main.Choice.foo'] || used['main__Choice__foo'], used.str()
}

fn test_top_level_sum_receiver_method_is_collected_before_variant_helpers() {
	used := mark_used_source('top_level_sum_receiver_method', '
struct A {}
struct B {}

type Choice = A | B

fn make_choice() Choice {
	return Choice(A{})
}

fn (choice Choice) foo() int {
	return 7
}

fn (a A) foo() int {
	return 1
}

fn (b B) foo() int {
	return 2
}

__global top = make_choice().foo()
')
	assert used['Choice.foo'] || used['main.Choice.foo'] || used['main__Choice__foo'], used.str()
}

fn test_same_module_unqualified_homonym_call_uses_qualified_key() {
	a, tc := parse_checked_project('same_module_unqualified_homonym_call', {
		'main.v': 'module main

import a

fn main() {
	println(a.glob_match(false))
}
'
		'a/a.v':  'module a

fn helper() string {
	return "a"
}

pub fn glob_match(flag bool) string {
	if !flag {
		return helper()
	}
	return "other"
}
'
		'b/b.v':  'module b

fn helper() string {
	return "b"
}

pub fn glob_match(flag bool) string {
	if !flag {
		return helper()
	}
	return "other"
}
'
	}, 'main.v')
	used := markused.mark_used(a, tc)
	assert used['a.glob_match']
	assert used['a.helper']
	assert !used['b.glob_match']
	assert !used['b.helper']
}

fn test_top_level_fn_value_respects_prior_local_shadow() {
	used := mark_used_source('top_level_fn_value_prior_shadow', '
fn helper() int {
	return 1
}

helper := 10
f := helper
_ = f
')
	assert !used['helper']
}

fn test_local_fn_value_keeps_helper_before_future_local_shadow() {
	used := mark_used_source('local_fn_value_future_shadow', '
fn cb() int {
	return 1
}

fn takes(f fn () int) int {
	return f()
}

fn main() {
	takes(cb)
	cb := 0
	_ = cb
}
')
	assert used['cb']
}

fn test_fn_value_call_callee_call_roots_inner_factory() {
	used := mark_used_source('fn_value_call_callee_call', '
fn cb() int {
	return 7
}

fn make_cb() fn () int {
	return cb
}

fn main() {
	_ := make_cb()()
}
')
	assert used['make_cb']
	assert used['cb']
}

fn test_local_ident_reference_does_not_root_dead_function() {
	mut a, mut tc := parse_checked_source('local_ident_shadow_dead_fn_cgen', '
fn C.v3_dead_local_shadow_should_not_link() int

fn unused() int {
	return C.v3_dead_local_shadow_should_not_link()
}

fn echo(unused int) int {
	println(unused)
	return unused
}

fn main() {
	unused := 1
	println(unused)
	_ := echo(unused)
}
')
	mut used := markused.mark_used(a, tc)
	assert !used['unused']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert !c_code.contains('unused('), c_code
	assert !c_code.contains('v3_dead_local_shadow_should_not_link'), c_code
}

fn test_local_fn_value_call_does_not_root_shadowed_dead_function() {
	mut a, mut tc := parse_checked_source('local_fn_value_shadow_dead_fn_cgen', '
fn C.v3_dead_local_fn_value_shadow_should_not_link() int

fn unused() int {
	return C.v3_dead_local_fn_value_shadow_should_not_link()
}

fn used() int {
	return 7
}

fn main() {
	unused := used
	println((unused)())
}
')
	mut used := markused.mark_used(a, tc)
	assert used['used']
	assert !used['unused']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('used('), c_code
	assert !c_code.contains('int unused(void)'), c_code
	assert !c_code.contains('v3_dead_local_fn_value_shadow_should_not_link'), c_code
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
	assert c_code.contains('new_map(sizeof(string), sizeof(i64)')
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
	assert c_code.contains('new_map(sizeof(string), sizeof(i64)')
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

// test_implicit_interface_str_dispatch_seeds_generated_helpers validates this v3 regression case.
fn test_implicit_interface_str_dispatch_seeds_generated_helpers() {
	mut a, mut tc := parse_checked_source('implicit_interface_str_dispatch_helpers', '
interface Printable {
	str() string
}

struct Inner {
	n int
}

struct Foo {
	n      int
	u      u8
	ratio  f64
	ch     rune
	nums   []int
	lookup map[string]u64
	inner  Inner
}

fn main() {
	value := Printable(Foo{
		n:      1
		u:      2
		ratio:  1.25
		ch:     `x`
		nums:   [3, 4]
		lookup: map[string]u64{
			"a": u64(5)
		}
		inner:  Inner{
			n: 6
		}
	})
	println(value.str())
}
')
	mut used := markused.mark_used(a, tc)
	for helper in ['string__plus', 'i64__str', 'u64__str', 'f64__str', 'rune__str'] {
		assert used[helper], helper
	}
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('string__plus('), c_code
	assert c_code.contains('v3_map_str('), c_code
	assert c_code.contains('f64__str('), c_code
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

// test_f32_interpolation_lowers_to_formatter_after_used_filter_transform
// validates this v3 regression case.
fn test_f32_interpolation_lowers_to_formatter_after_used_filter_transform() {
	mut a, mut tc := parse_checked_source('f32_interp_formatter_cgen', '
fn main() {
	value := f32(1.25)
	_ := "\${value}"
}
')
	mut used := markused.mark_used(a, tc)
	assert used['f32.str']
	assert used['f32__str']
	used = transform.transform_with_used(mut a, tc, used)
	tc.diagnose_unknown_calls = false
	tc.reject_unlowered_map_mutation = true
	tc.annotate_types()
	mut g := cgen.FlatGen.new()
	c_code := g.gen_with_used_options(a, used, tc, true)
	assert c_code.contains('f32__str(')
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
	compile := os.exec([v3_bin, '-o', bin, '${src}'])
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
	compile := os.exec([v3_bin, '-o', bin, '${src}'])
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
	compile := os.exec([v3_bin, '-o', bin, '${src}'])
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
	compile := os.exec([v3_bin, '-o', bin, '${src}'])
	assert compile.exit_code == 0, compile.output
}

// test_print_signed_width_compile_keeps_str_runtime_helpers validates this v3 regression case.
fn test_print_signed_width_compile_keeps_str_runtime_helpers() {
	v3_bin := build_v3_bin('print_signed_width_test')

	src := os.join_path(os.temp_dir(), 'v3_markused_print_signed_width_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_markused_print_signed_width_input')
	os.write_file(src, "
fn main() {
	println(i8(-5))
	println(i16(-300))
	println(i32(-70000))
	println(i64(-5000000000))
	println('\${i64(42)}')
}
") or {
		panic(err)
	}
	compile := os.exec([v3_bin, '-o', bin, '${src}'])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '-5\n-300\n-70000\n-5000000000\n42', run.output
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
	compile := os.exec([v3_bin, '-o', bin, '${src}'])
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
	compile := os.exec([v3_bin, '-o', bin, '${src}'])
	assert compile.exit_code == 0, compile.output
}
