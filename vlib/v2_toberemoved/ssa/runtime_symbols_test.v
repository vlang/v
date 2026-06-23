// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ssa

import os
import v2.ast
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
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	transformed := trans.transform_files(files)
	mut ssa_mod := Module.new('runtime_symbols')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.target_os = target_os
	b.build_all(transformed)
	return ssa_mod
}

fn build_ssa_for_runtime_symbol_target_flat_test(code string, target_os string) &Module {
	tmp_file := os.join_path(os.vtmp_dir(), 'v2_ssa_runtime_symbols_flat_${os.getpid()}.v')
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
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	flat := ast.flatten_files(files)
	transformed_flat, _ := trans.transform_files_to_flat(&flat, files)
	mut ssa_mod := Module.new('runtime_symbols_flat')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.target_os = target_os
	b.build_all_from_flat(&transformed_flat)
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

fn called_function_names(m &Module) []string {
	mut names := []string{}
	for val in m.values {
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		if instr.op != .call || instr.operands.len == 0 {
			continue
		}
		callee := instr.operands[0]
		if callee <= 0 || callee >= m.values.len {
			continue
		}
		callee_val := m.values[callee]
		if callee_val.kind == .func_ref {
			names << callee_val.name
		}
	}
	return names
}

fn global_value_names(m &Module) []string {
	return m.values.filter(it.kind == .global).map(it.name)
}

fn store_destination_call_names(m &Module) []string {
	mut names := []string{}
	for val in m.values {
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		if instr.op != .store || instr.operands.len < 2 {
			continue
		}
		dest := instr.operands[1]
		if dest <= 0 || dest >= m.values.len {
			continue
		}
		dest_val := m.values[dest]
		if dest_val.kind != .instruction {
			continue
		}
		dest_instr := m.instrs[dest_val.index]
		if dest_instr.op != .call || dest_instr.operands.len == 0 {
			continue
		}
		callee := dest_instr.operands[0]
		if callee <= 0 || callee >= m.values.len {
			continue
		}
		callee_val := m.values[callee]
		if callee_val.kind == .func_ref {
			names << callee_val.name
		}
	}
	return names
}

fn store_destination_global_names(m &Module) []string {
	mut names := []string{}
	for val in m.values {
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		if instr.op != .store || instr.operands.len < 2 {
			continue
		}
		dest := instr.operands[1]
		if dest <= 0 || dest >= m.values.len {
			continue
		}
		dest_val := m.values[dest]
		if dest_val.kind == .global {
			names << dest_val.name
		}
	}
	return names
}

fn c_errno_write_test_code() string {
	return '
module main

fn reset_errno() {
	C.errno = 0
}

fn bump_errno() {
	C.errno += 1
}

fn main() {
	reset_errno()
	bump_errno()
}
'
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

fn windows_std_handle_macros_code() string {
	return '
module main

fn input_handle() u32 {
	return C.STD_INPUT_HANDLE
}

fn output_handle() u32 {
	return C.STD_OUTPUT_HANDLE
}

fn error_handle() u32 {
	return C.STD_ERROR_HANDLE
}

fn main() {
	_ := input_handle()
	_ := output_handle()
	_ := error_handle()
}
'
}

fn windows_invalid_handle_value_macro_code() string {
	return '
module main

fn invalid_handle() voidptr {
	return C.INVALID_HANDLE_VALUE
}

fn main() {
	_ := invalid_handle()
}
'
}

fn assert_windows_std_handle_macros_are_ssa_constants_not_external_globals(m &Module) {
	global_names := global_value_names(m)
	expected_consts := {
		'STD_INPUT_HANDLE':  '4294967286'
		'STD_OUTPUT_HANDLE': '4294967285'
		'STD_ERROR_HANDLE':  '4294967284'
	}
	mut seen_consts := map[string]bool{}
	for name, const_name in expected_consts {
		assert name !in global_names
		seen_consts[const_name] = false
	}
	for val in m.values {
		if val.kind != .constant || val.name !in seen_consts {
			continue
		}
		typ := m.type_store.types[val.typ]
		assert typ.kind == .int_t
		assert typ.width == 32
		assert typ.is_unsigned
		seen_consts[val.name] = true
	}
	for _, const_name in expected_consts {
		assert seen_consts[const_name]
	}
}

fn test_windows_std_handle_macros_are_ssa_constants_not_external_globals() {
	m := build_ssa_for_runtime_symbol_target_test(windows_std_handle_macros_code(), 'windows')
	assert_windows_std_handle_macros_are_ssa_constants_not_external_globals(m)
}

fn test_windows_std_handle_macros_are_flat_ssa_constants_not_external_globals() {
	m := build_ssa_for_runtime_symbol_target_flat_test(windows_std_handle_macros_code(), 'windows')
	assert_windows_std_handle_macros_are_ssa_constants_not_external_globals(m)
}

fn assert_windows_invalid_handle_value_macro_is_pointer_sized_constant_not_external_global(m &Module) {
	global_names := global_value_names(m)
	assert 'INVALID_HANDLE_VALUE' !in global_names

	mut has_invalid_handle_const := false
	for val in m.values {
		if val.kind != .constant || val.name != '-1' {
			continue
		}
		typ := m.type_store.types[val.typ]
		if typ.kind == .ptr_t {
			has_invalid_handle_const = true
		}
	}
	assert has_invalid_handle_const
}

fn test_windows_invalid_handle_value_macro_is_pointer_sized_constant_not_external_global() {
	m := build_ssa_for_runtime_symbol_target_test(windows_invalid_handle_value_macro_code(),
		'windows')
	assert_windows_invalid_handle_value_macro_is_pointer_sized_constant_not_external_global(m)
}

fn test_windows_invalid_handle_value_macro_is_flat_pointer_sized_constant_not_external_global() {
	m := build_ssa_for_runtime_symbol_target_flat_test(windows_invalid_handle_value_macro_code(),
		'windows')
	assert_windows_invalid_handle_value_macro_is_pointer_sized_constant_not_external_global(m)
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
	global_names := global_value_names(m)

	assert 'stdout' in load_names
	assert 'stderr' in load_names
	assert 'stdin' in load_names
	assert '__stdoutp' !in load_names
	assert '__stderrp' !in load_names
	assert '__stdinp' !in load_names
	assert 'C.stdout' !in global_names
	assert 'C.stderr' !in global_names
	assert 'C.stdin' !in global_names
}

fn test_c_errno_uses_errno_location_on_linux_targets() {
	m := build_ssa_for_runtime_symbol_target_test('
module main

fn read_errno() int {
	return C.errno
}

fn main() {
	_ := read_errno()
}
',
		'linux')
	call_names := called_function_names(m)
	global_names := global_value_names(m)

	assert '__errno_location' in call_names
	assert '__error' !in call_names
	assert 'errno' !in global_names
}

fn test_c_errno_flat_reads_use_errno_location_on_linux_targets() {
	m := build_ssa_for_runtime_symbol_target_flat_test('
module main

fn read_errno() int {
	return C.errno
}

fn main() {
	_ := read_errno()
}
',
		'linux')
	call_names := called_function_names(m)
	global_names := global_value_names(m)

	assert '__errno_location' in call_names
	assert '__error' !in call_names
	assert 'errno' !in global_names
}

fn test_c_errno_writes_use_errno_location_on_linux_targets() {
	m := build_ssa_for_runtime_symbol_target_test(c_errno_write_test_code(), 'linux')
	call_names := called_function_names(m)
	store_call_names := store_destination_call_names(m)
	global_names := global_value_names(m)

	assert call_names.filter(it == '__errno_location').len == 2
	assert store_call_names.filter(it == '__errno_location').len == 2
	assert '__error' !in call_names
	assert 'errno' !in global_names
	assert 'C.errno' !in global_names
}

fn test_c_errno_flat_writes_use_errno_location_on_linux_targets() {
	m := build_ssa_for_runtime_symbol_target_flat_test(c_errno_write_test_code(), 'linux')
	call_names := called_function_names(m)
	store_call_names := store_destination_call_names(m)
	global_names := global_value_names(m)

	assert call_names.filter(it == '__errno_location').len == 2
	assert store_call_names.filter(it == '__errno_location').len == 2
	assert '__error' !in call_names
	assert 'errno' !in global_names
	assert 'C.errno' !in global_names
}

fn test_c_errno_uses_error_on_macos_targets() {
	m := build_ssa_for_runtime_symbol_target_test('
module main

fn read_errno() int {
	return C.errno
}

fn main() {
	_ := read_errno()
}
',
		'macos')
	call_names := called_function_names(m)
	global_names := global_value_names(m)

	assert '__error' in call_names
	assert '__errno_location' !in call_names
	assert 'errno' !in global_names
}

fn test_c_errno_writes_use_error_on_macos_targets() {
	m := build_ssa_for_runtime_symbol_target_test(c_errno_write_test_code(), 'macos')
	call_names := called_function_names(m)
	store_call_names := store_destination_call_names(m)
	global_names := global_value_names(m)

	assert call_names.filter(it == '__error').len == 2
	assert store_call_names.filter(it == '__error').len == 2
	assert '__errno_location' !in call_names
	assert 'errno' !in global_names
	assert 'C.errno' !in global_names
}

fn test_c_errno_flat_writes_use_error_on_macos_targets() {
	m := build_ssa_for_runtime_symbol_target_flat_test(c_errno_write_test_code(), 'macos')
	call_names := called_function_names(m)
	store_call_names := store_destination_call_names(m)
	global_names := global_value_names(m)

	assert call_names.filter(it == '__error').len == 2
	assert store_call_names.filter(it == '__error').len == 2
	assert '__errno_location' !in call_names
	assert 'errno' !in global_names
	assert 'C.errno' !in global_names
}

fn test_c_errno_uses_errno_on_windows_targets() {
	m := build_ssa_for_runtime_symbol_target_test('
module main

fn read_errno() int {
	return C.errno
}

fn main() {
	_ := read_errno()
}
',
		'windows')
	call_names := called_function_names(m)
	global_names := global_value_names(m)

	assert '_errno' in call_names
	assert '__errno_location' !in call_names
	assert '__error' !in call_names
	assert 'errno' !in global_names
	assert 'C.errno' !in global_names
}

fn test_c_errno_writes_use_errno_on_windows_targets() {
	m := build_ssa_for_runtime_symbol_target_test(c_errno_write_test_code(), 'windows')
	call_names := called_function_names(m)
	store_call_names := store_destination_call_names(m)
	store_global_names := store_destination_global_names(m)
	global_names := global_value_names(m)

	assert call_names.filter(it == '_errno').len == 2
	assert store_call_names.filter(it == '_errno').len == 2
	assert '__errno_location' !in call_names
	assert '__error' !in call_names
	assert store_global_names.filter(it == 'errno').len == 0
	assert 'errno' !in global_names
	assert 'C.errno' !in global_names
}

fn test_c_errno_flat_writes_use_errno_on_windows_targets() {
	m := build_ssa_for_runtime_symbol_target_flat_test(c_errno_write_test_code(), 'windows')
	call_names := called_function_names(m)
	store_call_names := store_destination_call_names(m)
	store_global_names := store_destination_global_names(m)
	global_names := global_value_names(m)

	assert call_names.filter(it == '_errno').len == 2
	assert store_call_names.filter(it == '_errno').len == 2
	assert '__errno_location' !in call_names
	assert '__error' !in call_names
	assert store_global_names.filter(it == 'errno').len == 0
	assert 'errno' !in global_names
	assert 'C.errno' !in global_names
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
	global_names := global_value_names(m)

	assert 'stdout' in load_names
	assert 'stderr' in load_names
	assert 'stdin' in load_names
	assert '__stdoutp' !in load_names
	assert '__stderrp' !in load_names
	assert '__stdinp' !in load_names
	assert 'C.stdout' !in global_names
	assert 'C.stderr' !in global_names
	assert 'C.stdin' !in global_names
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
	global_names := global_value_names(m)

	assert '__stdoutp' in load_names
	assert '__stderrp' in load_names
	assert '__stdinp' in load_names
	assert 'stdout' !in load_names
	assert 'stderr' !in load_names
	assert 'stdin' !in load_names
	assert 'C.stdout' !in global_names
	assert 'C.stderr' !in global_names
	assert 'C.stdin' !in global_names
}
