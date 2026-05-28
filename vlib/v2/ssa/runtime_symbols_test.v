// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ssa

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn build_ssa_for_runtime_symbol_test(code string) &Module {
	return build_ssa_for_runtime_symbol_target_test(code, 'linux')
}

fn build_ssa_for_runtime_symbol_target_test(code string, target_os string) &Module {
	tmp_file := os.join_path(os.vtmp_dir(), 'v2_ssa_runtime_symbols_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	mut prefs := &vpref.Preferences{
		backend:     .x64
		no_parallel: true
	}
	prefs.target_os = target_os
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	mut env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	transformed := trans.transform_files(files)
	mut ssa_mod := Module.new('runtime_symbols')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.target_os = target_os
	b.build_all(transformed)
	return ssa_mod
}

fn stdio_loaded_external_symbol_names(m &Module) []string {
	mut names := []string{}
	for val in m.values {
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		if instr.op != .load || instr.operands.len != 1 {
			continue
		}
		operand := instr.operands[0]
		if operand <= 0 || operand >= m.values.len {
			continue
		}
		global := m.values[operand]
		if global.kind == .global
			&& global.name in ['stdout', 'stderr', 'stdin', '__stdoutp', '__stderrp', '__stdinp'] {
			assert global.index >= 0 && global.index < m.globals.len
			assert m.globals[global.index].linkage == .external
			names << global.name
		}
	}
	return names
}

fn test_c_float_epsilon_macros_are_ssa_constants_not_external_globals() {
	m := build_ssa_for_runtime_symbol_test('
module main

fn seek_set() int {
	return C.SEEK_SET
}

fn flt_epsilon() f32 {
	return C.FLT_EPSILON
}

fn dbl_epsilon() f64 {
	return C.DBL_EPSILON
}

fn main() {
	_ := seek_set()
	_ := flt_epsilon()
	_ := dbl_epsilon()
}
')
	mut has_seek_set_const := false
	mut has_flt_epsilon_const := false
	mut has_dbl_epsilon_const := false
	for val in m.values {
		if val.kind == .global {
			assert val.name != 'SEEK_SET'
			assert val.name != 'FLT_EPSILON'
			assert val.name != 'DBL_EPSILON'
		}
		if val.kind != .constant {
			continue
		}
		if val.name == '1.19209290e-07' {
			typ := m.type_store.types[val.typ]
			assert typ.kind == .float_t
			assert typ.width == 32
			has_flt_epsilon_const = true
		}
		if val.name == '2.2204460492503131e-16' {
			typ := m.type_store.types[val.typ]
			assert typ.kind == .float_t
			assert typ.width == 64
			has_dbl_epsilon_const = true
		}
		if val.name == '0' {
			typ := m.type_store.types[val.typ]
			if typ.kind == .int_t && typ.width == 32 {
				has_seek_set_const = true
			}
		}
	}
	assert has_seek_set_const
	assert has_flt_epsilon_const
	assert has_dbl_epsilon_const
}

fn test_c_stdio_globals_are_loaded_on_non_macos_targets() {
	m := build_ssa_for_runtime_symbol_target_test('
module main

fn main() {
	_ := C.stdout
	_ := C.stderr
	_ := C.stdin
}
',
		'linux')
	load_names := stdio_loaded_external_symbol_names(m)

	assert 'stdout' in load_names
	assert 'stderr' in load_names
	assert 'stdin' in load_names
	assert '__stdoutp' !in load_names
	assert '__stderrp' !in load_names
	assert '__stdinp' !in load_names
}

fn test_c_stdio_globals_are_loaded_on_windows_targets() {
	m := build_ssa_for_runtime_symbol_target_test('
module main

fn main() {
	_ := C.stdout
	_ := C.stderr
	_ := C.stdin
}
',
		'windows')
	load_names := stdio_loaded_external_symbol_names(m)

	assert 'stdout' in load_names
	assert 'stderr' in load_names
	assert 'stdin' in load_names
	assert '__stdoutp' !in load_names
	assert '__stderrp' !in load_names
	assert '__stdinp' !in load_names
}

fn test_c_stdio_globals_are_loaded_from_macos_symbols_on_macos_targets() {
	m := build_ssa_for_runtime_symbol_target_test('
module main

fn main() {
	_ := C.stdout
	_ := C.stderr
	_ := C.stdin
}
',
		'macos')
	load_names := stdio_loaded_external_symbol_names(m)

	assert '__stdoutp' in load_names
	assert '__stderrp' in load_names
	assert '__stdinp' in load_names
	assert 'stdout' !in load_names
	assert 'stderr' !in load_names
	assert 'stdin' !in load_names
}
