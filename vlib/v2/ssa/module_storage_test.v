module ssa

import os
import v2.parser
import v2.pref
import v2.token
import v2.transformer
import v2.types

fn module_storage_ssa_tmp_dir(label string) string {
	return os.join_path(os.temp_dir(), 'v2_module_storage_ssa_${label}_${os.getpid()}')
}

fn module_storage_ssa_for_test_sources(label string, sources map[string]string) &Module {
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
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	trans.set_file_set(file_set)
	transformed_files := trans.transform_files(files)
	mut mod := Module.new('module_storage_test')
	mut builder := Builder.new_with_env(mod, env)
	builder.build_all(transformed_files)
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
