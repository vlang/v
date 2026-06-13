module ssa

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn mut_array_param_append_source() string {
	return r'
module main

fn add_seen(mut seen []string, name string) {
	seen << name
}

fn push_path(mut patterns [][]string, path []string) {
	patterns << path
}

fn push_many_paths(mut patterns [][]string, paths [][]string) {
	patterns << paths
}

fn push_literal_path(mut patterns [][]string, path []string) {
	patterns << [path]
}

fn main() {
	mut seen := []string{}
	add_seen(mut seen, "A")
	path := ["A", "B"]
	mut patterns := [][]string{}
	push_path(mut patterns, path)
	push_many_paths(mut patterns, [path])
	push_literal_path(mut patterns, path)
}
'
}

fn mut_array_param_append_parse(source string) ([]ast.File, &types.Environment, &token.FileSet) {
	tmp_file := os.join_path(os.vtmp_dir(), 'v2_ssa_mut_array_param_append_${os.getpid()}.v')
	os.write_file(tmp_file, source) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .x64
		arch:        .x64
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	return files, env, file_set
}

fn mut_array_param_append_build_legacy(source string) &Module {
	files, env, file_set := mut_array_param_append_parse(source)
	prefs := &vpref.Preferences{
		backend:     .x64
		arch:        .x64
		no_parallel: true
	}
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed := trans.transform_files(files)
	mut mod := Module.new('mut_array_param_append_legacy')
	mut b := Builder.new_with_env(mod, env)
	b.build_all(transformed)
	return mod
}

fn mut_array_param_append_build_flat(source string) &Module {
	files, env, file_set := mut_array_param_append_parse(source)
	prefs := &vpref.Preferences{
		backend:     .x64
		arch:        .x64
		no_parallel: true
	}
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed := trans.transform_files(files)
	flat := ast.flatten_files(transformed)
	mut mod := Module.new('mut_array_param_append_flat')
	mut b := Builder.new_with_env(mod, env)
	b.build_all_from_flat(&flat)
	return mod
}

fn mut_array_param_append_func_index(m &Module, fn_name string) int {
	for i, func in m.funcs {
		if func.name == fn_name {
			return i
		}
	}
	assert false, 'missing SSA function ${fn_name}'
	return 0
}

fn mut_array_param_append_calls(m &Module, fn_name string, callee_name string) [][]ValueID {
	func_idx := mut_array_param_append_func_index(m, fn_name)
	mut calls := [][]ValueID{}
	for block_id in m.funcs[func_idx].blocks {
		for value_id in m.blocks[block_id].instrs {
			value := m.values[value_id]
			if value.kind != .instruction {
				continue
			}
			instr := m.instrs[value.index]
			if instr.op != .call || instr.operands.len == 0 {
				continue
			}
			callee_id := instr.operands[0]
			if callee_id > 0 && callee_id < m.values.len && m.values[callee_id].name == callee_name {
				calls << instr.operands
			}
		}
	}
	return calls
}

fn mut_array_param_append_assert_arg_uses_local_storage(m &Module, fn_name string, arg_id ValueID) {
	assert arg_id > 0 && arg_id < m.values.len, '${fn_name} append argument is out of range'
	mut cur_id := arg_id
	for _ in 0 .. 6 {
		assert cur_id > 0 && cur_id < m.values.len
		value := m.values[cur_id]
		assert value.kind == .instruction
		instr := m.instrs[value.index]
		if instr.op == .alloca {
			return
		}
		assert instr.op in [.bitcast, .load]
		assert instr.operands.len == 1
		cur_id = instr.operands[0]
	}
	assert false, '${fn_name} append argument does not resolve to local storage'
}

fn mut_array_param_append_assert_module(m &Module) {
	add_push_calls := mut_array_param_append_calls(m, 'add_seen', 'builtin__array_push_noscan')
	assert add_push_calls.len == 1
	assert add_push_calls[0].len >= 2
	mut_array_param_append_assert_arg_uses_local_storage(m, 'add_seen', add_push_calls[0][1])

	path_push_calls := mut_array_param_append_calls(m, 'push_path', 'builtin__array_push_noscan')
	assert path_push_calls.len == 1
	assert path_push_calls[0].len >= 2
	mut_array_param_append_assert_arg_uses_local_storage(m, 'push_path', path_push_calls[0][1])

	many_push_calls := mut_array_param_append_calls(m, 'push_many_paths', 'array__push_many')
	assert many_push_calls.len == 1
	assert many_push_calls[0].len >= 2
	mut_array_param_append_assert_arg_uses_local_storage(m, 'push_many_paths',
		many_push_calls[0][1])

	literal_push_calls := mut_array_param_append_calls(m, 'push_literal_path',
		'builtin__array_push_noscan')
	assert literal_push_calls.len == 1
	assert literal_push_calls[0].len >= 2
	mut_array_param_append_assert_arg_uses_local_storage(m, 'push_literal_path',
		literal_push_calls[0][1])
}

fn test_mut_array_param_append_uses_local_storage_legacy_and_flat() {
	source := mut_array_param_append_source()
	legacy := mut_array_param_append_build_legacy(source)
	mut_array_param_append_assert_module(legacy)
	flat := mut_array_param_append_build_flat(source)
	mut_array_param_append_assert_module(flat)
}
