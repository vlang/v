module ssa

import os
import v.parser
import v.pref
import v.types

fn test_native_test_entrypoint_selects_user_files_hooks_and_function_globs() {
	path := os.join_path(os.vtmp_dir(), 'ssa_native_harness_${os.getpid()}_test.v')
	other := os.join_path(os.vtmp_dir(), 'ssa_native_unselected_${os.getpid()}_test.v')
	defer {
		os.rm(path) or {}
		os.rm(other) or {}
	}
	os.write_file(path, 'module sample
fn testsuite_begin() {}
fn testsuite_end() {}
fn before_each() {}
fn after_each() {}
fn test_selected() {}
fn test_skipped() {}
')!
	os.write_file(other, 'module imported
fn testsuite_begin() {}
fn test_imported() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([path, other])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
		test_files:    [path]
		test_run_only: ['sample.test_select*']
	})
	main_function := m.funcs.filter(it.name == 'main')[0]
	mut calls := []string{}
	for block in main_function.blocks {
		for id in m.blocks[block].instrs {
			instruction := m.instrs[m.values[id].index]
			if instruction.op == .call {
				calls << m.values[instruction.operands[0]].name
			}
		}
	}
	assert calls == ['__ssa_init', 'sample.testsuite_begin', 'sample.before_each',
		'sample.test_selected', 'sample.after_each', 'sample.testsuite_end']
	assert native_test_matches_run_only('sample', 'test_selected', ['test_select*'])
	assert native_test_matches_run_only('sample', 'test_selected', [])
	assert !native_test_matches_run_only('sample', 'test_skipped', ['sample.test_select*'])
}

fn test_native_test_entrypoint_checks_propagated_option_and_result_failures() {
	path := os.join_path(os.vtmp_dir(), 'ssa_native_harness_returns_${os.getpid()}_test.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'module main
fn test_optional() ? { return none }
fn test_result() ! {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := build_with_options(a, map[string]bool{}, tc, BuildOptions{
		test_files: [path]
	})
	main_function := m.funcs.filter(it.name == 'main')[0]
	mut checked_returns := 0
	mut failed_exits := 0
	for block in main_function.blocks {
		for id in m.blocks[block].instrs {
			instruction := m.instrs[m.values[id].index]
			if instruction.op == .br {
				checked_returns++
			} else if instruction.op == .call && m.values[instruction.operands[0]].name == 'exit' {
				assert m.values[instruction.operands[1]].name == '1'
				failed_exits++
			}
		}
	}
	assert checked_returns == 2
	assert failed_exits == 2
}

fn test_native_test_entrypoint_qualifies_an_implicit_main_module() {
	path := os.join_path(os.vtmp_dir(), 'ssa_native_implicit_main_${os.getpid()}_test.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'fn test_selected() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
		test_files:    [path]
		test_run_only: ['main.test_select*']
	})
	main_function := m.funcs.filter(it.name == 'main')[0]
	mut found := false
	for block in main_function.blocks {
		for id in m.blocks[block].instrs {
			instruction := m.instrs[m.values[id].index]
			if instruction.op == .call && m.values[instruction.operands[0]].name == 'test_selected' {
				found = true
			}
		}
	}
	assert found
}
