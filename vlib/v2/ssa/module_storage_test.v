module ssa

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token
import v2.transformer
import v2.types

fn module_storage_ssa_tmp_dir(label string) string {
	return os.join_path(os.temp_dir(), 'v2_module_storage_ssa_${label}_${os.getpid()}')
}

fn module_storage_ssa_builder_for_test_sources(label string, sources map[string]string) &Builder {
	tmp_dir := module_storage_ssa_tmp_dir(label)
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{}
	for rel_path, code in sources {
		path := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(path)) or { panic('cannot create ${os.dir(path)}') }
		os.write_file(path, code) or { panic('cannot write ${path}') }
		paths << path
	}
	prefs := &pref.Preferences{
		backend: .x64
		arch:    .x64
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed_files := trans.transform_files(files)
	mut mod := Module.new('module_storage_test')
	mut builder := Builder.new_with_env(mod, env)
	builder.build_all(transformed_files)
	return builder
}

fn module_storage_ssa_for_test_sources(label string, sources map[string]string) &Module {
	builder := module_storage_ssa_builder_for_test_sources(label, sources)
	return builder.mod
}

fn module_storage_ssa_for_test_sources_flat(label string, sources map[string]string) &Module {
	tmp_dir := module_storage_ssa_tmp_dir(label)
	os.mkdir_all(tmp_dir) or { panic('cannot create ${tmp_dir}') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{}
	for rel_path, code in sources {
		path := os.join_path(tmp_dir, rel_path)
		os.mkdir_all(os.dir(path)) or { panic('cannot create ${os.dir(path)}') }
		os.write_file(path, code) or { panic('cannot write ${path}') }
		paths << path
	}
	prefs := &pref.Preferences{
		backend: .arm64
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	flat := ast.flatten_files(files)
	transformed_flat, _ := trans.transform_files_to_flat(&flat, files)
	mut mod := Module.new('module_storage_flat_test')
	mut builder := Builder.new_with_env(mod, env)
	builder.build_all_from_flat(&transformed_flat)
	return mod
}

fn module_storage_ssa_global_names(m &Module) []string {
	mut names := []string{}
	for global in m.globals {
		names << global.name
	}
	return names
}

fn module_storage_ssa_global_value_id(m &Module, name string) ?ValueID {
	for value in m.values {
		if value.kind == .global && value.name == name {
			return value.id
		}
	}
	return none
}

fn module_storage_ssa_has_store_to_global(m &Module, name string) bool {
	global_id := module_storage_ssa_global_value_id(m, name) or { return false }
	for instr in m.instrs {
		if instr.op == .store && instr.operands.len >= 2 && instr.operands[1] == global_id {
			return true
		}
	}
	return false
}

fn module_storage_ssa_func(m &Module, name string) ?Function {
	for func in m.funcs {
		if func.name == name {
			return func
		}
	}
	return none
}

fn module_storage_ssa_return_value_id(m &Module, func Function) ?ValueID {
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			instr := m.instrs[m.values[val_id].index]
			if instr.op == .ret && instr.operands.len > 0 {
				return instr.operands[0]
			}
		}
	}
	return none
}

fn module_storage_ssa_return_value_ids(m &Module, func Function) []ValueID {
	mut ret_vals := []ValueID{}
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			instr := m.instrs[m.values[val_id].index]
			if instr.op == .ret && instr.operands.len > 0 {
				ret_vals << instr.operands[0]
			}
		}
	}
	return ret_vals
}

fn module_storage_ssa_call_callees(m &Module, func Function) []string {
	mut callees := []string{}
	for blk_id in func.blocks {
		for val_id in m.blocks[blk_id].instrs {
			value := m.values[val_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .call || instr.operands.len == 0 {
				continue
			}
			callee_id := instr.operands[0]
			if callee_id <= 0 || callee_id >= m.values.len {
				continue
			}
			callee := m.values[callee_id]
			if callee.kind == .func_ref {
				callees << callee.name
			}
		}
	}
	return callees
}

fn test_module_storage_import_alias_resolves_to_declaring_module_in_ssa() {
	m := module_storage_ssa_for_test_sources('alias', {
		'report/report.v': 'module report

pub __global mut errors = 0
'
		'main.v':          'module main

import report as r

fn main() {
	r.errors += 1
}
'
	})
	names := module_storage_ssa_global_names(m)
	assert 'report__errors' in names
	assert 'r__errors' !in names
	assert module_storage_ssa_has_store_to_global(m, 'report__errors')
}

fn module_storage_selective_import_nested_sources() map[string]string {
	return {
		'main.v':                              'module main

import mymodules.submodule { sub_xy }

fn main() {
	value := sub_xy(10, 7)
	_ = value
}
'
		'mymodules/submodule/sub_functions.v': 'module submodule

pub fn sub_xy(x int, y int) int {
	return x - y
}
'
	}
}

fn module_storage_assert_nested_selective_import_callee(m &Module, label string) {
	main_func := module_storage_ssa_func(m, 'main') or { panic('${label}: missing main') }
	callees := module_storage_ssa_call_callees(m, main_func)
	assert 'submodule__sub_xy' in callees, '${label}: missing submodule__sub_xy in ${callees}'
	assert 'sub_xy' !in callees, '${label}: bare sub_xy callee leaked into ${callees}'
	assert 'mymodules_submodule__sub_xy' !in callees, '${label}: import path callee leaked into ${callees}'
}

fn test_module_storage_legacy_selective_import_nested_module_uses_declared_leaf_module() {
	m := module_storage_ssa_for_test_sources('selective_nested_legacy',
		module_storage_selective_import_nested_sources())
	module_storage_assert_nested_selective_import_callee(m, 'legacy')
}

fn test_module_storage_flat_selective_import_nested_module_uses_declared_leaf_module() {
	m := module_storage_ssa_for_test_sources_flat('selective_nested_flat',
		module_storage_selective_import_nested_sources())
	module_storage_assert_nested_selective_import_callee(m, 'flat')
}

fn module_storage_direct_import_nested_sources() map[string]string {
	return {
		'main.v':                              'module main

import mymodules.submodule

fn main() {
	value := submodule.sub_xy(10, 7)
	_ = value
}
'
		'mymodules/submodule/sub_functions.v': 'module submodule

pub fn sub_xy(x int, y int) int {
	return x - y
}
'
	}
}

fn test_module_storage_legacy_direct_import_nested_module_selector_uses_declared_leaf_module() {
	m := module_storage_ssa_for_test_sources('direct_nested_legacy',
		module_storage_direct_import_nested_sources())
	module_storage_assert_nested_selective_import_callee(m, 'legacy direct')
}

fn test_module_storage_flat_direct_import_nested_module_selector_uses_declared_leaf_module() {
	m := module_storage_ssa_for_test_sources_flat('direct_nested_flat',
		module_storage_direct_import_nested_sources())
	module_storage_assert_nested_selective_import_callee(m, 'flat direct')
}

fn test_module_storage_legacy_direct_import_module_selector_one_arg_stays_call() {
	m := module_storage_ssa_for_test_sources('module_selector_one_arg_call', {
		'main.v':          'module main

import left as l
import right
import helper

fn main() {
	first := l.state == 10 && right.state == 200 && l.score() == 1001 && right.score() == 20005
	direct := l.bump(3)
	transitive := helper.bump_right(5)
	second := direct == 132 && transitive == 2057 && l.state == 13 && right.state == 205
	_ = first
	_ = second
}
'
		'left/left.v':     'module left

pub __global mut state = 10
__global mut private_hits = 1

pub fn bump(delta int) int {
	state += delta
	private_hits += 1
	return state * 10 + private_hits
}

pub fn score() int {
	return state * 100 + private_hits
}
'
		'right/right.v':   'module right

pub __global mut state = 200
__global mut private_hits = 5

pub fn bump(delta int) int {
	state += delta
	private_hits += 2
	return state * 10 + private_hits
}

pub fn score() int {
	return state * 100 + private_hits
}
'
		'helper/helper.v': 'module helper

import right as r

pub fn bump_right(delta int) int {
	return r.bump(delta)
}

pub fn right_score() int {
	return r.score()
}
'
	})
	main_func := module_storage_ssa_func(m, 'main') or { panic('missing main') }
	callees := module_storage_ssa_call_callees(m, main_func)
	assert 'helper__bump_right' in callees
}

fn test_module_storage_legacy_call_or_cast_module_selector_prefers_registered_call() {
	mut builder := module_storage_ssa_builder_for_test_sources('module_selector_call_or_cast_decision', {
		'helper/helper.v': 'module helper

pub fn bump_right(delta int) int {
	return delta + 1
}
'
		'main.v':          'module main

import helper

fn main() {}
'
	})
	sel := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'helper'
		})
		rhs: ast.Ident{
			name: 'bump_right'
		}
	}
	assert !builder.call_or_cast_selector_should_remain_cast(sel, 'helper__bump_right')
}

fn test_module_qualified_alias_array_field_index_has_alias_element_type() {
	m := module_storage_ssa_for_test_sources('selector_alias_array_elem', {
		'v2/ssa/value.v': 'module ssa

pub type ValueID = int
'
		'main.v':         'module main

import v2.ssa

struct Holder {
	items []ssa.ValueID
}

fn get_item(h Holder, i int) ssa.ValueID {
	return h.items[i]
}
'
	})
	get_item := module_storage_ssa_func(m, 'main__get_item') or {
		module_storage_ssa_func(m, 'get_item') or {
			module_storage_ssa_func(m, 'main.get_item') or { panic('missing get_item') }
		}
	}
	mut ret_val := ValueID(0)
	for blk_id in get_item.blocks {
		for val_id in m.blocks[blk_id].instrs {
			instr := m.instrs[m.values[val_id].index]
			if instr.op == .ret && instr.operands.len > 0 {
				ret_val = instr.operands[0]
			}
		}
	}
	assert ret_val != 0
	ret_typ := m.type_store.types[m.values[ret_val].typ]
	assert ret_typ.kind == .int_t
	assert ret_typ.width == 32
}

fn test_map_array_alias_value_index_has_alias_element_type() {
	m := module_storage_ssa_for_test_sources('map_array_alias_value_index', {
		'main.v': 'module main

type TypeID = int

fn read_elem(values map[string][]TypeID, key string, idx int) TypeID {
	param_elem_types := values[key] or { [TypeID(0)] }
	return param_elem_types[idx]
}
'
	})
	read_elem := module_storage_ssa_func(m, 'main__read_elem') or {
		module_storage_ssa_func(m, 'read_elem') or { panic('missing read_elem') }
	}
	ret_val := module_storage_ssa_return_value_id(m, read_elem) or {
		panic('missing read_elem return value')
	}
	ret_typ := m.type_store.types[m.values[ret_val].typ]
	assert ret_typ.kind == .int_t
	assert ret_typ.width == 32
}

fn test_method_map_array_alias_value_index_has_alias_element_type() {
	m := module_storage_ssa_for_test_sources('method_map_array_alias_value_index', {
		'main.v': 'module main

type TypeID = int

struct Builder {
mut:
	fn_param_array_elem_types map[string][]TypeID
}

fn (mut b Builder) call_param_array_elem_type(fn_name string, param_idx int) TypeID {
	param_elem_types := b.fn_param_array_elem_types[fn_name] or { return TypeID(-1) }
	if param_idx >= param_elem_types.len {
		return TypeID(-2)
	}
	elem_type := param_elem_types[param_idx]
	if elem_type == 0 {
		return TypeID(-3)
	}
	return elem_type
}
'
	})
	func := module_storage_ssa_func(m, 'Builder__call_param_array_elem_type') or {
		panic('missing method function')
	}
	ret_vals := module_storage_ssa_return_value_ids(m, func)
	assert ret_vals.len > 0
	for ret_val in ret_vals {
		ret_typ := m.type_store.types[m.values[ret_val].typ]
		assert ret_typ.kind == .int_t
		assert ret_typ.width == 32
	}
}

fn test_flat_method_map_array_alias_value_index_has_alias_element_type() {
	m := module_storage_ssa_for_test_sources_flat('flat_method_map_array_alias_value_index', {
		'main.v': 'module main

type TypeID = int

struct Builder {
mut:
	fn_param_array_elem_types map[string][]TypeID
}

fn (mut b Builder) call_param_array_elem_type(fn_name string, param_idx int) TypeID {
	param_elem_types := b.fn_param_array_elem_types[fn_name] or { return TypeID(-1) }
	if param_idx >= param_elem_types.len {
		return TypeID(-2)
	}
	elem_type := param_elem_types[param_idx]
	if elem_type == 0 {
		return TypeID(-3)
	}
	return elem_type
}
'
	})
	func := module_storage_ssa_func(m, 'Builder__call_param_array_elem_type') or {
		panic('missing method function')
	}
	ret_vals := module_storage_ssa_return_value_ids(m, func)
	assert ret_vals.len > 0
	for ret_val in ret_vals {
		ret_typ := m.type_store.types[m.values[ret_val].typ]
		assert ret_typ.kind == .int_t
		assert ret_typ.width == 32
	}
}

fn test_module_storage_same_short_names_stay_distinct_in_ssa() {
	m := module_storage_ssa_for_test_sources('collisions', {
		'a/a.v':  'module a

pub __global state = 1
'
		'b/b.v':  'module b

pub __global state = 2
'
		'main.v': 'module main

import a
import b

fn main() {
	_ = a.state + b.state
}
'
	})
	names := module_storage_ssa_global_names(m)
	assert 'a__state' in names
	assert 'b__state' in names
	assert 'state' !in names
	a_id := module_storage_ssa_global_value_id(m, 'a__state') or {
		panic('missing a__state global')
	}
	b_id := module_storage_ssa_global_value_id(m, 'b__state') or {
		panic('missing b__state global')
	}
	assert a_id != b_id
}

fn test_module_storage_bare_dunder_name_resolves_to_current_module_in_ssa() {
	m := module_storage_ssa_for_test_sources('bare_dunder', {
		'a/a.v':  'module a

pub __global mut state__x = 1

pub fn bump() int {
	state__x += 1
	return state__x
}
'
		'b/b.v':  'module b

pub __global mut state__x = 2

pub fn bump() int {
	state__x += 1
	return state__x
}
'
		'main.v': 'module main

import a
import b

fn main() {
	_ = a.bump() + b.bump()
	_ = a.state__x + b.state__x
}
'
	})
	names := module_storage_ssa_global_names(m)
	assert 'a__state__x' in names
	assert 'b__state__x' in names
	assert 'state__x' !in names
	assert module_storage_ssa_has_store_to_global(m, 'a__state__x')
	assert module_storage_ssa_has_store_to_global(m, 'b__state__x')
	a_id := module_storage_ssa_global_value_id(m, 'a__state__x') or {
		panic('missing a__state__x global')
	}
	b_id := module_storage_ssa_global_value_id(m, 'b__state__x') or {
		panic('missing b__state__x global')
	}
	assert a_id != b_id
}

fn test_module_storage_c_extern_raw_global_uses_c_symbol_in_ssa() {
	m := module_storage_ssa_for_test_sources('c_extern_raw', {
		'ext/ext.v': 'module ext

@[c_extern]
pub __global raw_status int

pub fn read_status() int {
	return raw_status
}
'
		'main.v':    'module main

import ext

fn main() {
	_ = ext.raw_status
	_ = ext.read_status()
}
'
	})
	names := module_storage_ssa_global_names(m)
	assert 'raw_status' in names
	assert 'ext__raw_status' !in names
	_ := module_storage_ssa_global_value_id(m, 'raw_status') or {
		panic('missing raw_status C extern global')
	}
	assert module_storage_ssa_global_value_id(m, 'ext__raw_status') == none
}
