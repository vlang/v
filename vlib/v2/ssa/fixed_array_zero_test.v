module ssa

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

struct FixedArrayZeroStats {
	instrs int
	geps   int
	stores int
}

fn build_ssa_for_fixed_array_zero_test(code string, native_bulk_zero bool) &Module {
	tmp_file := os.join_path(os.vtmp_dir(), 'v2_ssa_fixed_array_zero_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
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
	mut ssa_mod := Module.new('fixed_array_zero')
	mut b := Builder.new_with_env(ssa_mod, env)
	b.native_backend_bulk_zero_alloca = native_bulk_zero
	b.build_all(transformed)
	return ssa_mod
}

fn fixed_array_zero_stats(len int, native_bulk_zero bool) FixedArrayZeroStats {
	m := build_ssa_for_fixed_array_zero_test('
module main

fn main() {
	a := [${len}]u8{}
	_ = a
}
',
		native_bulk_zero)
	mut geps := 0
	mut stores := 0
	for instr in m.instrs {
		if instr.op == .get_element_ptr {
			geps++
		} else if instr.op == .store {
			stores++
		}
	}
	return FixedArrayZeroStats{
		instrs: m.instrs.len
		geps:   geps
		stores: stores
	}
}

fn test_empty_fixed_array_literal_emits_explicit_ssa_zero_stores() {
	small := fixed_array_zero_stats(16, false)
	large17 := fixed_array_zero_stats(17, false)
	large64 := fixed_array_zero_stats(64, false)

	assert small.geps >= 16
	assert small.stores >= 16
	assert large17.geps >= 17
	assert large17.stores >= 17
	assert large64.geps >= 64
	assert large64.stores >= 64
	assert large64.instrs > large17.instrs
}

fn test_native_bulk_zero_capable_backend_keeps_bounded_ssa_zero_stores() {
	small := fixed_array_zero_stats(16, true)
	large17 := fixed_array_zero_stats(17, true)
	large64 := fixed_array_zero_stats(64, true)

	assert small.geps >= 16
	assert small.stores >= 16
	assert large17.geps < small.geps
	assert large17.stores < small.stores
	assert large64.geps == large17.geps
	assert large64.stores == large17.stores
	assert large64.instrs <= large17.instrs + 4
}
