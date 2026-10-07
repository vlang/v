module markused

import os
import v.flat
import v.parser
import v.pref
import v.types

fn checked_runtime_helper_source(source string) (&flat.FlatAst, &types.TypeChecker) {
	root := os.join_path(os.temp_dir(), 'v_reachable_runtime_helpers_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	tc.diagnostic_files[path] = true
	tc.diagnose_unknown_calls = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	return a, &tc
}

fn test_runtime_interpolation_helpers_follow_reachable_bodies() {
	for reached in [false, true] {
		call := if reached { 'extra()' } else { '' }
		a, tc := checked_runtime_helper_source('module main
fn extra() {
	x := 17
	println("\${x}")
}
fn main() {
	println(3)
	${call}
}
')
		used := mark_used(a, tc)
		assert used['strings.new_builder'] == reached
		assert used['strings.Builder.write_string'] == reached
		assert used['string_plus_many'] == reached
		full, _ := mark_used_with_generic_usage_full_runtime(a, tc)
		assert full['strings.new_builder']
	}
}

fn test_runtime_string_method_helpers_follow_reachable_bodies() {
	for reached in [false, true] {
		call := if reached { 'extra()' } else { '' }
		a, tc := checked_runtime_helper_source('module main
struct Item {}
fn (i Item) trim_space() int { return 1 }
fn extra() {
	i := Item{}
	println(i.trim_space())
}
fn main() {
	println(3)
	${call}
}
')
		used := mark_used(a, tc)
		assert used['string.trim_space'] == reached
	}
}

fn test_runtime_helpers_keep_struct_defaults_and_field_declarations() {
	a, tc := checked_runtime_helper_source('module main
struct Item {
	text string = "\${17}"
	value ?int
}
fn main() {
	println(3)
}
')
	used := mark_used(a, tc)
	assert used['strings.new_builder']
	assert used['error_with_code']
}

fn test_runtime_helpers_keep_generated_flag_enum_support() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{ kind: .enum_decl, value: 'Flags', typ: 'flag' })
	mut tc := types.TypeChecker.new(&a)
	mut scan := RuntimeHelpersScan{}
	mut used := map[string]bool{}
	mut queue := []string{}
	scan.enqueue_nodes(&a, &tc, []int{}, []bool{len: a.nodes.len}, 'main',
		map[string]string{}, mut used, mut queue)
	assert scan.needs_string_plus_helper
	assert used['string__plus']
}

fn test_runtime_helpers_keep_unused_c_channel_signature() {
	a, tc := checked_runtime_helper_source('module main
fn C.unused_channel() chan int
fn main() {
	println(3)
}
')
	used := mark_used(a, tc)
	assert used['sync.new_channel_st']
	assert used['sync.Channel.closed_error']
}

fn test_float_helpers_follow_reachable_stringification() {
	for reached in [false, true] {
		call := if reached { 'extra()' } else { '' }
		a, tc := checked_runtime_helper_source('module main
fn extra() {
	println(f64(1.25))
}
fn main() {
	println(3)
	${call}
}
')
		used := mark_used(a, tc)
		assert used['f64.str'] == reached
		assert used['strconv__f64_to_str_l'] == reached
		assert !used['f32.str']
		assert !used['strconv.Dec32.get_string_32']
		assert !used['strconv.Dec64.get_string_64']
		full, _ := mark_used_with_generic_usage_full_runtime(a, tc)
		assert full['f32.str']
		assert full['f64.str']
		assert full['strconv.Dec32.get_string_32']
		assert full['strconv.Dec64.get_string_64']
		selfhost := mark_used_without_generic_detection(a, tc)
		assert selfhost['f32.str']
		cached := mark_used_for_cache(a, tc, []string{}, map[string]bool{})
		assert cached['f32.str']
		tested := mark_used_for_tests(a, tc, ['main.v'])
		assert tested['f32.str']
	}
}

fn test_float_helpers_keep_scalar_and_aggregate_formatters() {
	for body in ['println(1.25)', 'println(f32(1.25))', 'println(Number(1.25))',
		'println(Item{ value: 1.25 })', 'println([f64(1.25)])', 'println([f32(1.25)]!)',
		'println({"x": f64(1.25)})', 'println({"x": 1})', 'value := f64(1.25)\nprintln("\${value:.2f}")'] {
		a, tc := checked_runtime_helper_source('module main
type Number = f64
struct Item {
	value f64
}
fn main() {
	${body}
}
')
		used := mark_used(a, tc)
		assert used['f64.str'], body
		if body.contains('f32') {
			assert used['f32.str'], body
			assert used['strconv__f32_to_str_l'], body
		} else {
			assert used['strconv__f64_to_str_l'], body
		}
	}
}

fn test_float_helpers_keep_direct_methods_and_implicit_interface_str() {
	a, tc := checked_runtime_helper_source('module main
interface Printable {
	str() string
}
struct Item {
	value f32
}
fn main() {
	value := f64(1.25)
	println(value.str())
	item := Printable(Item{ value: f32(1.25) })
	println(item.str())
}
')
	used := mark_used(a, tc)
	assert used['f64.str']
	assert used['f32.str']
	assert used['strconv__f32_to_str_l']
}

fn test_direct_str_helpers_keep_lowered_container_formatters() {
	for body in ['values := {"x": 1}\nprintln(values.str())',
		'values := {"x": u128(1)}\nprintln(values.str())', 'values := []f64{}\nprintln(values.str())',
		'values := [f32(1.25)]!\nprintln(values.str())', 'values := [f64(1.25)]\nprintln(values.str())',
		'value := Number(1.25)\nprintln(value.str())',
		'item := Item{ value: f32(1.25) }\nprintln(item.str())'] {
		a, tc := checked_runtime_helper_source('module main
type Number = f64
struct Item {
	value f32
}
fn main() {
	${body}
}
')
		used := mark_used(a, tc)
		assert used['f64.str'], body
		assert used['strconv__f64_to_str_l'] || used['strconv__f32_to_str_l'], body
	}
}

fn test_implicit_interface_str_keeps_nested_float_formatters() {
	for source in [
		'
struct Item {
	kind int
	value ?f64
}
fn main() {
	item := Printable(Item{ value: 1.25 })
	println(item.str())
}
',
		'
type Number = f64 | string
struct Item {
	kind int
	value Number
}
fn main() {
	item := Printable(Item{ value: f64(1.25) })
	println(item.str())
}
',
		'
struct Item[T] {
	kind int
	value T
}
fn main() {
	item := Printable(Item[f64]{ value: 1.25 })
	println(item.str())
}
',
		'
interface Nested { value f64 }
struct Inner { value f64 }
struct Item {
	kind int
	value Nested
}
fn main() {
	item := Printable(Item{ value: Inner{ value: 1.25 } })
	println(item.str())
}
',
	] {
		a, tc := checked_runtime_helper_source('module main
interface Printable {
	kind int
	str() string
}
${source}')
		used := mark_used(a, tc)
		assert used['f64.str'], source
		assert used['strconv__f64_to_str_l'], source
	}
}

fn test_implicit_interface_str_keeps_nested_custom_method_without_float_fields() {
	a, tc := checked_runtime_helper_source('module main
interface Printable {
	outer int
	str() string
}
interface Nested {
	kind int
}
struct Inner {
	kind int
	value f64
}
fn (i Inner) str() string { return "custom inner" }
struct Item {
	outer int
	nested Nested
}
fn main() {
	item := Printable(Item{ nested: Inner{ value: 1.25 } })
	println(item.str())
}
')
	used := mark_used(a, tc)
	assert used['Inner.str']
	assert !used['f64.str']
}

fn test_map_runtime_seeds_follow_reachable_values() {
	for reached in [false, true] {
		call := if reached { 'extra()' } else { '' }
		a, tc := checked_runtime_helper_source('module main
fn extra() {
	m := { "answer": 42 }
	assert m["answer"] == 42
}
fn main() {
	values := [1, 2, 3]
	assert values[0] == 1
	${call}
}
')
		used := mark_used(a, tc)
		for helper in ['new_map', 'map.set', 'map.clone', 'map.free', 'map_hash_int_4',
			'map_hash_string'] {
			assert used[helper] == reached, helper
		}
		full, _ := mark_used_with_generic_usage_full_runtime(a, tc)
		assert full['map.set']
		assert full['map.free']
	}
}

fn test_map_runtime_keeps_defaults_aliases_and_generic_values() {
	for source in [
		'struct Item { values map[string]int }
fn main() { item := Item{}; assert item.values.len == 0 }',
		'struct Item { values map[string]int }
fn main() { items := [2]Item{}; assert items[0].values.len == 0 }',
		'type Values = map[string]int
struct Item { values Values }
fn main() { item := Item{}; assert item.values.len == 0 }',
		'struct Item[T] { value T }
fn make[T]() Item[T] { return Item[T]{} }
fn main() { item := make[map[string]int](); assert item.value.len == 0 }',
		'fn empty() map[string]int { return {} }
fn main() { values := empty(); assert values.len == 0 }',
		'fn C.empty_map() map[string]int
fn main() { values := C.empty_map(); assert values.len == 0 }',
		'__global values map[string]int
fn main() { assert values.len == 0 }',
		'struct Item { values map[string]int }
__global item Item
fn main() { values := [1, 2, 3]; assert values[0] == 1 }',
	] {
		a, tc := checked_runtime_helper_source('module main\n${source}\n')
		used := mark_used(a, tc)
		for helper in ['new_map', 'map.set', 'map.clone', 'map.free', 'map_hash_string'] {
			assert used[helper], '${helper}: ${source}'
		}
	}
}

fn test_map_runtime_uses_collected_imported_global_types() {
	root := os.join_path(os.temp_dir(), 'v_reachable_runtime_globals_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'dependency')) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	for declaration in [
		'struct Container { values map[string]int }\n__global item Container',
		'type Values = map[string]int\n__global item Values',
	] {
		dep_path := os.join_path(root, 'dependency', 'dependency.v')
		main_path := os.join_path(root, 'main.v')
		os.write_file(dep_path, 'module dependency\n${declaration}\n') or { panic(err) }
		os.write_file(main_path, 'module main
import dependency as _
struct Container { marker int }
type Values = []int
fn main() { println(42) }
') or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files([dep_path, main_path])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.enable_globals = true
		tc.collect(a)
		tc.diagnostic_files[dep_path] = true
		tc.diagnostic_files[main_path] = true
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		used := mark_used(a, &tc)
		assert used['new_map'], declaration
		assert used['map.set'], declaration
		assert used['map.free'], declaration
	}
}

fn test_synthetic_closure_import_does_not_root_unused_runtime() {
	mut a, tc := checked_runtime_helper_source('module main
fn main() { values := [1, 2, 3]; assert values[0] == 1 }
')
	a.nodes << flat.Node{
		kind:  .import_decl
		value: 'builtin.closure'
		typ:   '__v3_builtin_closure_runtime'
	}
	used := mark_used(a, tc)
	assert !used['closure.closure_init']
	full, _ := mark_used_with_generic_usage_full_runtime(a, tc)
	assert full['closure.closure_init']
	a.nodes[a.nodes.len - 1].typ = 'closure'
	explicit := mark_used(a, tc)
	assert explicit['closure.closure_init']
}

fn test_closure_runtime_follows_reached_captures_and_method_values() {
	for source in [
		'offset := 3; cb := fn [offset]() int { return offset }; assert cb() == 3',
		'item := Item{ number: 3 }; cb := item.value; assert cb() == 3',
		'item := Item{ number: 3 }; assert (item.value)() == 3',
		'offset := 3; cb := || offset + 1; assert cb() == 4',
	] {
		for reached in [false, true] {
			call := if reached { 'extra()' } else { '' }
			a, tc := checked_runtime_helper_source('module main
struct Item { number int }
fn (i Item) value() int { return i.number }
fn extra() { ${source} }
fn main() { values := [1, 2, 3]; assert values[0] == 1; ${call} }
')
			used := mark_used(a, tc)
			for helper in ['closure.closure_init', 'closure.closure_create_with_data',
				'closure.closure_try_destroy'] {
				assert used[helper] == reached, '${helper}: ${source}'
			}
		}
	}
}

fn test_checked_enum_constants_do_not_root_closure_runtime() {
	for source in ['Choice.first', 'AliasChoice.first'] {
		mut a, tc := checked_runtime_helper_source('module main
enum Choice { first second }
type AliasChoice = Choice
fn main() { value := ${source}; assert value == Choice.first }
')
		a.nodes << flat.Node{
			kind:  .import_decl
			value: 'builtin.closure'
			typ:   '__v3_builtin_closure_runtime'
		}
		used := mark_used(a, tc)
		assert !used['closure.closure_init'], source
		assert !used['closure.closure_create_with_data'], source
		assert !used['closure.closure_try_destroy'], source
		assert !used['new_map'], source
		assert !used['map.set'], source
	}
}

fn test_enum_alias_method_values_keep_closure_runtime() {
	mut a, tc := checked_runtime_helper_source('module main
enum Choice { first second }
type AliasChoice = Choice
fn (value AliasChoice) number() int { return int(value) }
fn main() {
	value := AliasChoice(Choice.first)
	callback := value.number
	assert callback() == 0
}
')
	a.nodes << flat.Node{
		kind:  .import_decl
		value: 'builtin.closure'
		typ:   '__v3_builtin_closure_runtime'
	}
	used := mark_used(a, tc)
	assert used['AliasChoice.number']
	assert used['closure.closure_init']
	assert used['closure.closure_create_with_data']
	assert used['closure.closure_try_destroy']
}

fn test_assignment_targets_do_not_root_closure_runtime() {
	for source in ['item.number = 3', 'item.number += 3', 'unsafe { values.len = 3 }',
		'unsafe { values.len += 1 }', 'item.number, other = 3, 4', 'unsafe { values.len, other = 3, 4 }'] {
		mut a, tc := checked_runtime_helper_source('module main
struct Item { mut: number int }
fn main() {
	mut item := Item{}
	mut values := [0, 0, 0, 0]
	mut other := 0
	${source}
	assert other >= 0
}
')
		a.nodes << flat.Node{
			kind:  .import_decl
			value: 'builtin.closure'
			typ:   '__v3_builtin_closure_runtime'
		}
		used := mark_used(a, tc)
		for helper in ['closure.closure_init', 'closure.closure_create_with_data',
			'closure.closure_try_destroy', 'new_map', 'map.set'] {
			assert !used[helper], '${helper}: ${source}'
		}
	}
}

fn test_assignment_operands_keep_bound_method_values() {
	for source in [
		'mut callback := plain; callback = item.value; assert callback() == 3',
		'mut values := [0, 0, 0, 0]; values[(item.value)()] = 3; assert values[3] == 3',
	] {
		mut a, tc := checked_runtime_helper_source('module main
struct Item { number int }
fn (i Item) value() int { return i.number }
fn plain() int { return 0 }
fn main() { item := Item{ number: 3 }; ${source} }
')
		a.nodes << flat.Node{
			kind:  .import_decl
			value: 'builtin.closure'
			typ:   '__v3_builtin_closure_runtime'
		}
		used := mark_used(a, tc)
		assert used['Item.value'], source
		for helper in ['closure.closure_init', 'closure.closure_create_with_data',
			'closure.closure_try_destroy'] {
			assert used[helper], '${helper}: ${source}'
		}
	}
}

fn test_builtin_closure_globals_wait_for_reached_runtime() {
	root := os.join_path(os.temp_dir(), 'v_reachable_closure_globals_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'vlib', 'builtin', 'closure')) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	for reached in [false, true] {
		path := os.join_path(root, 'vlib', 'builtin', 'closure', 'closure.c.v')
		main_path := os.join_path(root, 'main.v')
		os.write_file(path, 'module closure
struct Closure { live map[string]int }
__global g_closure = Closure{}
pub fn closure_init() {}
') or { panic(err) }
		call := if reached { 'closure.closure_init()' } else { '' }
		os.write_file(main_path, 'module main
import builtin.closure as closure
fn main() { values := [1, 2, 3]; assert values[0] == 1; ${call} }
') or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_files([path, main_path])
		for mut node in a.nodes {
			if node.kind == .import_decl && node.value == 'builtin.closure' {
				node.typ = '__v3_builtin_closure_runtime'
			}
		}
		mut tc := types.TypeChecker.new(a)
		tc.enable_globals = true
		tc.collect(a)
		tc.diagnostic_files[path] = true
		tc.diagnostic_files[main_path] = true
		// The synthetic import's private alias never replaces the runtime's
		// fully qualified declaration names.
		tc.check_semantics()
		assert p.diagnostics.len == 0, p.diagnostics.str()
		assert tc.errors.len == 0, tc.errors.str()
		used := mark_used(a, &tc)
		assert used['closure.closure_init'] == reached
		assert used['new_map'] == reached
		assert used['map.set'] == reached
	}
}

fn test_repeated_script_stringification_keeps_visible_fields() {
	for skipped in [true, false] {
		attribute := if skipped { '@[str: skip]' } else { '' }
		prints := 'println(value)\nprintln("\${value}")\n'.repeat(64)
		a, tc := checked_runtime_helper_source('module main
struct Hidden {}
fn (_ Hidden) str() string { return "hidden" }
struct Item {
	hidden Hidden ${attribute}
	values []f64
}
value := Item{ values: [1.25] }
${prints}
')
		used := mark_used(a, tc)
		assert used['Hidden.str'] == !skipped
		assert used['f64.str']
		assert used['strconv__f64_to_str_l']
	}
}

fn test_script_stringification_helpers_use_imports_and_local_bindings() {
	root := os.join_path(os.temp_dir(), 'v_script_runtime_helpers_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	path := os.join_path(root, 'main.v')
	for body in ['println(math.sin(1.25))', 'value := math.sin(1.25)\nprintln(value)',
		'println("\${math.sin(1.25):.2f}")', 'value := math.sin(1.25)\nprintln("\${value:.2f}")',
		'println(math.small(1.25))', 'value := math.small(1.25)\nprintln(value)',
		'println(math.values())', 'println(math.alias_value())', 'value := math.sin(1.25)\nprintln(1)'] {
		os.write_file(path, 'module main
import math
fn unused() { println(f64(2.5)) }
${body}
') or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		// Only imported declarations are available before script expressions are checked.
		tc.fn_ret_types['math.sin'] = types.Type(types.f64_)
		tc.fn_ret_types['math.small'] = types.Type(types.f32_)
		tc.fn_ret_types['math.values'] = types.Type(types.Array{ elem_type: types.Type(types.f64_) })
		tc.type_aliases['dep.Number'] = 'f64'
		tc.type_alias_modules['dep.Number'] = 'dep'
		tc.type_aliases['other.Number'] = 'int'
		tc.type_alias_modules['other.Number'] = 'other'
		tc.fn_ret_types['math.alias_value'] = types.Type(types.Alias{
			name:      'dep.Number'
			base_type: types.Type(types.f64_)
		})
		tc.cur_module = 'unrelated'
		tc.cur_file = 'unrelated.v'
		tc.register_file_import('dep', 'other')
		assert types.unalias_type(tc.parse_type('dep.Number')) == types.Type(types.int_)
		assert types.unalias_type(tc.parse_canonical_type('dep.Number')) == types.Type(types.f64_)
		collector := CallCollector{ a: a, tc: &tc }
		mut calls := []string{}
		mut local_values := map[string]bool{}
		mut local_types := map[string]string{}
		for file in a.nodes {
			if file.kind != .file { continue }
			for i in 0 .. file.children_count {
				id := a.child(&file, i)
				if markused_is_top_level_stmt(a.node(id)) {
					collector.collect_top_level_stmt_calls(id, 'main', {
						'math': 'math'
					},
						mut local_values, mut local_types, mut calls)
				}
			}
		}
		if body.ends_with('println(1)') {
			assert 'f64.str' !in calls, body
			assert 'strconv__f64_to_str_l' !in calls, body
			used := mark_used(a, &tc)
			assert !used['f64.str']
			assert !used['strconv__f64_to_str_l']
		} else if body.contains('small') {
			assert 'f32.str' in calls, body
			assert 'strconv__f32_to_str_l' in calls, body
			assert 'f64.str' in calls, body
		} else {
			assert 'f64.str' in calls, body
			assert 'strconv__f64_to_str_l' in calls, body
		}
	}
}

fn test_interface_dispatch_cache_waits_for_each_reached_method() {
	_, tc := checked_runtime_helper_source('module main
interface Reader {
	first() int
	second() int
}
struct Item {}
fn (item Item) first() int { return 1 }
fn (item Item) second() int { return 2 }
fn main() {}
')
	mut used := map[string]bool{}
	mut queue := []string{}
	mut processed := map[string]bool{}
	assert !enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue, mut processed)
	assert processed.len == 0
	used['Reader.first'] = true
	assert enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue, mut processed)
	assert used['Item.first']
	assert !used['Item.second']
	assert processed['Reader.first']
	assert !processed['Reader.second']
	first_len := queue.len
	assert !enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue, mut processed)
	assert queue.len == first_len
	// A later body can introduce a second abstract dispatch after the first one was cached.
	used['Reader.second'] = true
	enqueue_ierror_equality_dispatch_helpers(tc, mut used, mut queue, mut processed)
	assert used['Item.second']
	assert processed['Reader.second']
}

fn test_interface_dispatch_cache_keeps_implicit_stringification_dependencies() {
	_, tc := checked_runtime_helper_source('module main
interface Printable {
	kind int
	str() string
}
struct Item {
	kind int
	value ?f64
}
fn main() {
	item := Printable(Item{ value: 1.25 })
	println(item.str())
}
')
	mut used := map[string]bool{}
	mut queue := []string{}
	mut processed := map[string]bool{}
	enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue, mut processed)
	assert !processed['Printable.str']
	used['Printable.str'] = true
	enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue, mut processed)
	assert processed['Printable.str']
	assert used['f64.str']
	assert used['strconv__f64_to_str_l']
	first_len := queue.len
	enqueue_ierror_equality_dispatch_helpers(tc, mut used, mut queue, mut processed)
	assert queue.len == first_len
}

fn test_source_array_helpers_follow_reached_methods_and_keep_full_modes() {
	for reached in [false, true] {
		call := if reached { 'extra()' } else { '' }
		for operation in ['values.insert(0, 3)', 'values.insert(0, [3, 4])', 'values.prepend(3)',
			'values.prepend([3, 4])', '_ := values.reverse()', '_ := values.pop_left()',
			'values.delete(0)', 'values.delete_last()'] {
			a, tc := checked_runtime_helper_source('module main
fn extra() {
	mut values := [1, 2]
	${operation}
}
fn main() {
	println(3)
	${call}
}
')
			used := mark_used(a, tc)
			method := operation.all_after('values.').all_before('(')
			assert used['array.${method}'] == reached, operation
			assert used['array.insert_many'] == (reached && method in ['insert', 'prepend']), operation
			if !reached {
				for helper in ['insert', 'insert_many', 'prepend', 'reverse', 'pop_left', 'delete',
					'delete_last'] {
					assert !used['array.${helper}'], helper
				}
			}
			full, _ := mark_used_with_generic_usage_full_runtime(a, tc)
			selfhost := mark_used_without_generic_detection(a, tc)
			cached := mark_used_for_cache(a, tc, []string{}, map[string]bool{})
			tested := mark_used_for_tests(a, tc, ['main.v'])
			for helper in ['insert', 'insert_many', 'prepend', 'reverse', 'pop_left', 'delete',
				'delete_last'] {
				assert full['array.${helper}'], helper
				assert selfhost['array.${helper}'], helper
				assert cached['array.${helper}'], helper
				assert tested['array.${helper}'], helper
			}
		}
	}
}

fn test_source_array_helpers_keep_top_level_batch_lowering() {
	a, tc := checked_runtime_helper_source('module main
mut values := [1, 2]
values.insert(1, [3, 4])
values.prepend([5, 6])
println(values)
')
	used := mark_used(a, tc)
	assert used['array.insert']
	assert used['array.prepend']
	assert used['array.insert_many']
}

fn test_source_utilities_follow_reached_methods_and_preserve_full_modes() {
	utilities := ['string.all_before', 'string.all_before_last', 'string.all_after',
		'string.all_after_last', 'u8.is_letter', 'u8.is_capital', 'string.is_capital',
		'string.to_lower_ascii', 'rune.to_lower']
	for reached in [false, true] {
		call := if reached { 'extra()' } else { '' }
		for utility in utilities {
			method := utility.all_after('.')
			a, tc := checked_runtime_helper_source('module main
struct Item {}
fn (item Item) ${method}() int { return 17 }
fn extra() {
	item := Item{}
	println(item.${method}())
}
fn main() {
	println(3)
	${call}
}
')
			used := mark_used(a, tc)
			assert used[utility] == reached, utility
			assert used['Item.${method}'] == reached, utility
			if !reached {
				for helper in utilities {
					assert !used[helper], helper
				}
			}
			full, _ := mark_used_with_generic_usage_full_runtime(a, tc)
			selfhost := mark_used_without_generic_detection(a, tc)
			cached := mark_used_for_cache(a, tc, []string{}, map[string]bool{})
			tested := mark_used_for_tests(a, tc, ['main.v'])
			for helper in utilities {
				assert full[helper], helper
				assert selfhost[helper], helper
				assert cached[helper], helper
				assert tested[helper], helper
			}
		}
	}
}

fn test_source_utilities_keep_script_calls_and_field_defaults() {
	for source in [
		'module main
struct Item {}
fn (item Item) all_before() int { return 17 }
item := Item{}
println(item.all_before())
',
		'module main
struct Item {}
fn (item Item) all_before() int { return 17 }
struct Wrapper {
	value int = Item{}.all_before()
}
fn main() {
	value := Wrapper{}
	println(value.value)
}
',
	] {
		a, tc := checked_runtime_helper_source(source)
		used := mark_used(a, tc)
		assert used['Item.all_before'], source
		assert used['string.all_before']
	}
}

fn test_source_utilities_keep_bound_callbacks_and_generic_forwarding() {
	a, tc := checked_runtime_helper_source('module main
struct Item {}
fn (item Item) is_capital() int { return 17 }
fn (item Item) to_lower() int { return 19 }
fn forwarded[T](item T) int { return item.to_lower() }
fn main() {
	item := Item{}
	callback := item.is_capital
	println(callback())
	println(forwarded(item))
}
')
	used := mark_used(a, tc)
	assert used['Item.is_capital']
	assert used['Item.to_lower']
	assert used['string.is_capital']
	assert used['u8.is_capital']
	assert used['rune.to_lower']
}

fn test_specialization_fallback_rejects_nongeneric_prefix_matches() {
	mut a := flat.FlatAst.new()
	id := a.add_node(flat.Node{ kind: .fn_decl, value: 'new' })
	info := FnDeclInfo{ node_id: id, module: 'time' }
	declarations := {
		'new':      [info]
		'time.new': [info]
	}
	assert fn_decl_infos_for_queue_name('new_channel_st', declarations, &a, true).len == 0
	direct := fn_decl_infos_for_queue_name('time.new', declarations, &a, true)
	assert direct.len == 1
	assert direct[0].node_id == id
}

fn test_specialization_fallback_keeps_generic_templates() {
	mut a := flat.FlatAst.new()
	id := a.add_node(flat.Node{ kind: .fn_decl, value: 'identity', payload: flat.node_payload(['T']) })
	info := FnDeclInfo{ node_id: id, module: 'main' }
	declarations := {
		'identity': [info]
	}
	resolved := fn_decl_infos_for_queue_name('identity_T_int', declarations, &a, true)
	assert resolved.len == 1
	assert resolved[0].node_id == id
	assert fn_decl_infos_for_queue_name('identity_T_int', declarations, &a, false).len == 0
}

fn test_specialization_fallback_skips_nongeneric_match_for_earlier_generic_base() {
	mut a := flat.FlatAst.new()
	ordinary := a.add_node(flat.Node{ kind: .fn_decl, value: 'make_value' })
	generic := a.add_node(flat.Node{ kind: .fn_decl, value: 'make', payload: flat.node_payload(['T']) })
	declarations := {
		'make_value': [FnDeclInfo{ node_id: ordinary, module: 'main' }]
		'make':       [FnDeclInfo{ node_id: generic, module: 'main' }]
	}
	resolved := fn_decl_infos_for_queue_name('make_value_T_int', declarations, &a, true)
	assert resolved.len == 1
	assert resolved[0].node_id == generic
}

fn test_specialization_fallback_keeps_mangled_generic_receiver() {
	mut a := flat.FlatAst.new()
	id := a.add_node(flat.Node{ kind: .fn_decl, value: 'Box[T].get' })
	declarations := {
		'dep__Box_T__get': [FnDeclInfo{ node_id: id, module: 'dep' }]
	}
	resolved := fn_decl_infos_for_queue_name('dep__Box_T_int__get', declarations, &a, true)
	assert resolved.len == 1
	assert resolved[0].node_id == id
}

fn test_array_conversion_helpers_follow_reachable_selectors() {
	for method, helper in {
		'string':  '[]rune.string'
		'bytestr': 'Array_u8__bytestr'
		'hex':     'Array_u8__hex'
	} {
		for reached in [false, true] {
			call := if reached { 'extra()' } else { '' }
			a, tc := checked_runtime_helper_source('module main
struct Item {}
fn (i Item) ${method}() int { return 1 }
fn extra() { println(Item{}.${method}()) }
fn main() { println(3)
${call}
}
')
			used := mark_used(a, tc)
			assert used[helper] == reached
			assert !used['data_to_hex_string']
			full, _ := mark_used_with_generic_usage_full_runtime(a, tc)
			assert full[helper]
			assert full['data_to_hex_string']
			cached := mark_used_for_cache(a, tc, []string{}, map[string]bool{})
			assert cached[helper]
			assert cached['data_to_hex_string']
			tested := mark_used_for_tests(a, tc, ['main.v'])
			assert tested[helper]
		}
	}
}
