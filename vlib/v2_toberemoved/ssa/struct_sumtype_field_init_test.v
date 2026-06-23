module ssa

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn struct_sumtype_field_init_source() string {
	return '
module main

type Tree = Empty | Node

struct Empty {}

struct Node {
	value int
}

struct Holder {
	item Tree
}

fn make_holder() Holder {
	return Holder{Node{42}}
}
'
}

fn union_init_shape_source() string {
	return '
module main

union Bits {
	f f64
	u u64
}

fn make_f() Bits {
	return Bits{f: 0.2}
}

fn make_u() Bits {
	return Bits{u: u64(123)}
}
'
}

fn build_struct_sumtype_field_init_legacy(label string) &Module {
	tmp_file := os.join_path(os.vtmp_dir(), 'v2_ssa_struct_sumtype_field_${label}_${os.getpid()}.v')
	os.write_file(tmp_file, struct_sumtype_field_init_source()) or {
		panic('failed to write temp file')
	}
	defer {
		os.rm(tmp_file) or {}
	}
	mut prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	transformed := trans.transform_files(files)
	mut ssa_mod := Module.new('struct_sumtype_field_legacy')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.build_all(transformed)
	return ssa_mod
}

fn build_struct_sumtype_field_init_flat(label string) &Module {
	tmp_file := os.join_path(os.vtmp_dir(),
		'v2_ssa_struct_sumtype_field_flat_${label}_${os.getpid()}.v')
	os.write_file(tmp_file, struct_sumtype_field_init_source()) or {
		panic('failed to write temp file')
	}
	defer {
		os.rm(tmp_file) or {}
	}
	mut prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	flat := ast.flatten_files(files)
	transformed_flat, _ := trans.transform_files_to_flat(&flat, files)
	mut ssa_mod := Module.new('struct_sumtype_field_flat')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.build_all_from_flat(&transformed_flat)
	return ssa_mod
}

fn build_union_init_shape_legacy(label string) &Module {
	tmp_file := os.join_path(os.vtmp_dir(), 'v2_ssa_union_init_shape_${label}_${os.getpid()}.v')
	os.write_file(tmp_file, union_init_shape_source()) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	mut prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	transformed := trans.transform_files(files)
	mut ssa_mod := Module.new('union_init_shape_legacy')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.build_all(transformed)
	return ssa_mod
}

fn build_union_init_shape_flat(label string) &Module {
	tmp_file := os.join_path(os.vtmp_dir(),
		'v2_ssa_union_init_shape_flat_${label}_${os.getpid()}.v')
	os.write_file(tmp_file, union_init_shape_source()) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	mut prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	flat := ast.flatten_files(files)
	transformed_flat, _ := trans.transform_files_to_flat(&flat, files)
	mut ssa_mod := Module.new('union_init_shape_flat')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.build_all_from_flat(&transformed_flat)
	return ssa_mod
}

fn struct_sumtype_field_type_id(m &Module, short_name string) TypeID {
	for type_id, name in m.c_struct_names {
		if name == short_name || name.all_after_last('__') == short_name {
			return TypeID(type_id)
		}
	}
	return 0
}

fn assert_holder_field_value_is_wrapped_tree(m &Module) {
	tree_type := struct_sumtype_field_type_id(m, 'Tree')
	node_type := struct_sumtype_field_type_id(m, 'Node')
	holder_type := struct_sumtype_field_type_id(m, 'Holder')
	assert tree_type != 0, 'missing Tree SSA type'
	assert node_type != 0, 'missing Node SSA type'
	assert holder_type != 0, 'missing Holder SSA type'
	mut found_holder_init := false
	for value in m.values {
		if value.kind != .instruction || value.typ != holder_type {
			continue
		}
		instr := m.instrs[value.index]
		if instr.op != .struct_init || instr.operands.len != 1 {
			continue
		}
		found_holder_init = true
		field_value := instr.operands[0]
		assert field_value > 0 && field_value < m.values.len
		field_type := m.values[field_value].typ
		assert field_type == tree_type, 'Holder.item must be wrapped as Tree, got type ${field_type}'
		assert field_type != node_type, 'Holder.item was stored as raw Node instead of Tree'
	}
	assert found_holder_init, 'missing Holder struct_init'
}

fn assert_union_init_trims_trailing_implicit_zero_fields(m &Module) {
	bits_type := struct_sumtype_field_type_id(m, 'Bits')
	assert bits_type != 0, 'missing Bits SSA type'
	assert m.type_store.types[bits_type].is_union, 'Bits SSA type must be marked as union'
	mut lens := []int{}
	for value in m.values {
		if value.kind != .instruction || value.typ != bits_type {
			continue
		}
		instr := m.instrs[value.index]
		if instr.op == .struct_init {
			lens << instr.operands.len
		}
	}
	lens.sort()
	assert lens == [1, 2], 'union struct_init operands must be [f] and [zero, u], got ${lens}'
}

fn test_struct_sumtype_field_init_wraps_variant_on_legacy_ast() {
	m := build_struct_sumtype_field_init_legacy('legacy')
	assert_holder_field_value_is_wrapped_tree(m)
}

fn test_struct_sumtype_field_init_wraps_variant_on_flat_ast() {
	m := build_struct_sumtype_field_init_flat('flat')
	assert_holder_field_value_is_wrapped_tree(m)
}

fn test_union_init_trims_trailing_implicit_zero_fields_on_legacy_ast() {
	m := build_union_init_shape_legacy('legacy')
	assert_union_init_trims_trailing_implicit_zero_fields(m)
}

fn test_union_init_trims_trailing_implicit_zero_fields_on_flat_ast() {
	m := build_union_init_shape_flat('flat')
	assert_union_init_trims_trailing_implicit_zero_fields(m)
}
