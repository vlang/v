module types

import os
import v.flat
import v.parser
import v.pref

fn test_selected_reachability_keeps_nested_scope_and_loop_receiver_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_reachability_scope_memo_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dep, 'module dep
pub struct Item {}
pub struct Other {}
pub fn (item Item) method() { item_leaf() }
pub fn (item Other) method() { other_leaf() }
pub fn item_leaf() {}
pub fn other_leaf() {}
pub fn unused() {}
pub fn pair() (Item, Other) { return Item{}, Other{} }
pub fn entry() {
 item, other := pair()
 item.method()
 other.method()
 for entry in [item] { entry.method() }
 { item := Other{}; item.method() }
 closure := fn [item] () { item.method() }
 closure()
}
')!
	os.write_file(main, 'module main
import dep
fn main() { dep.entry() }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([dep, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.diagnostic_files[main] = true
	tc.collect_selected_file_called_fns()
	mut names := tc.selected_file_called_fns.keys()
	names.sort()
	assert names == ['dep.Item.method', 'dep.Other.method', 'dep.entry', 'dep.item_leaf',
		'dep.other_leaf', 'dep.pair'], names.str()
	assert tc.errors.len == 0, tc.errors.str()
	assert isnil(tc.body_resolve_memo)
}

fn test_selected_reachability_preserves_an_existing_memo_phase() {
	root := os.join_path(os.vtmp_dir(), 'v3_reachability_existing_memo_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dep, 'module dep
pub struct Item {}
pub fn (item Item) method() { leaf() }
pub fn leaf() {}
pub fn make() Item { return Item{} }
')!
	os.write_file(main, 'module main
import dep
fn main() { for item in [dep.make()] { item.method() } }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([dep, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	for active in [false, true] {
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.diagnostic_files[main] = true
		tc.arm_body_resolve_memo(0, a.nodes.len - 1)
		saved := tc.body_resolve_memo
		for i in 0 .. saved.filled.len {
			tc.body_resolve_memo.types[i] = new_type_reference(builtin_int_type)
			tc.body_resolve_memo.filled[i] = 1
		}
		if !active { tc.disarm_body_resolve_memo() }
		generation := saved.call_generation
		filled := saved.filled.clone()
		tc.collect_selected_file_called_fns()
		mut names := tc.selected_file_called_fns.keys()
		names.sort()
		assert names == ['dep.Item.method', 'dep.leaf', 'dep.make'], names.str()
		assert voidptr(tc.body_resolve_memo) == voidptr(saved)
		assert saved.active == active
		assert saved.call_generation == generation
		assert saved.filled == filled
		for typ in saved.types {
			assert typ is Primitive
		}
	}
}

fn test_selected_declaration_resolve_memo_caches_only_its_function_range() {
	root := os.join_path(os.vtmp_dir(), 'v3_reachability_decl_memo_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'struct Item {}
fn (item Item) method() {}
fn make() Item { return Item{} }
fn first() { for item in [make()] { item.method() } }
fn second() { for item in [make()] { item.method() } }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	decls := tc.selected_file_fn_decls()
	mut filter := &SmartcastKeyFilter{}
	for name in ['first', 'second'] {
		decl := decls[name] or { panic(name) }
		tc.collect_selected_decl_called_fns(decl, mut filter)
		memo := tc.body_resolve_memo
		assert !isnil(memo)
		assert !memo.active
		assert memo.lo == decl.idx - decl.cost + 1
		assert memo.hi == decl.idx
		mut filled := 0
		for i in 0 .. memo.hi - memo.lo + 1 {
			if memo.filled[i] != 0 {
				filled++
				assert !type_contains_unknown(memo.types[i])
			}
		}
		assert filled > 0
	}
}

fn test_selected_reachability_rebinds_previsited_branch_tail_receiver() {
	root := os.join_path(os.vtmp_dir(), 'v3_reachability_branch_memo_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dep := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dep, 'module dep
pub struct Item {}
pub struct Other {}
pub fn (item Item) make() Item { item_leaf(); return item }
pub fn (item Other) make() Other { other_leaf(); return item }
pub fn (item Item) method() {}
pub fn (item Other) method() {}
pub fn item_leaf() {}
pub fn other_leaf() {}
pub fn entry() {
 item := Item{}
 (if true { item := Other{}; item.make() } else { item.make() }).method()
}
')!
	os.write_file(main, 'module main
import dep
fn main() { dep.entry() }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([dep, main])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.diagnostic_files[main] = true
	tc.collect_selected_file_called_fns()
	mut names := tc.selected_file_called_fns.keys()
	names.sort()
	assert names == ['dep.Item.make', 'dep.Item.method', 'dep.Other.make', 'dep.entry', 'dep.item_leaf',
		'dep.other_leaf'], names.str()
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_selected_reachability_resolve_epochs_follow_scope_and_binding_changes() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	tc.push_scope()
	tc.cur_scope.insert('value', builtin_int_type)
	tc.arm_body_resolve_memo(int(id), int(id))
	assert tc.resolve_type(id) == builtin_int_type
	tc.cur_scope.insert('value', builtin_string_type)
	tc.selected_file_resolve_context_changed()
	assert tc.resolve_type(id) == builtin_string_type
	tc.push_scope()
	tc.selected_file_resolve_context_changed()
	tc.cur_scope.insert('value', builtin_bool_type)
	tc.selected_file_resolve_context_changed()
	assert tc.resolve_type(id) == builtin_bool_type
	tc.pop_scope()
	tc.selected_file_resolve_context_changed()
	assert tc.resolve_type(id) == builtin_string_type
}

fn test_selected_reachability_resolve_epoch_wrap_drops_old_types_and_calls() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	tc.push_scope()
	tc.cur_scope.insert('value', builtin_int_type)
	tc.arm_body_resolve_memo(int(id), int(id))
	assert tc.resolve_type(id) == builtin_int_type
	memo := tc.body_resolve_memo
	tc.body_resolve_memo.call_generations[0] = memo.call_generation
	old_call_generation := memo.call_generation
	tc.cur_scope.insert('value', builtin_string_type)
	for _ in 0 .. 255 { tc.selected_file_resolve_context_changed() }
	assert memo.type_generation == 1
	assert memo.filled[0] == 0
	assert memo.call_generation != old_call_generation
	assert memo.call_generations[0] != memo.call_generation
	assert tc.resolve_type(id) == builtin_string_type
}
