module types

import os
import v.flat
import v.parser
import v.pref

fn recursive_str_context_checker(path string) (&TypeChecker, flat.NodeId) {
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.diagnostic_files['other.v'] = true
	tc.collect(a)
	items := tc.collect_parallel_check_items()
	for item in items {
		tc.check_fn_decl_semantics(item.fn_idx, a.nodes[item.fn_idx], item.file, item.module)
	}
	assert tc.errors.len == 0, tc.errors.str()
	id := tc.recursive_str_fn_decl_id('Value.str') or { panic('missing str declaration') }
	tc.cur_file = path
	tc.cur_module = 'main'
	tc.fn_context.node_id = int(id)
	tc.cur_fn_node_id = int(id)
	return tc, id
}

fn test_recursive_str_context_keeps_helper_diagnostics_above_user_boundary() {
	path := os.join_path(os.vtmp_dir(), 'recursive_str_context_${os.getpid()}.v')
	os.write_file(path, 'module main
struct Value {}
fn (value Value) str() string { return format(value) }
fn format(value Value) string { return value.str() }
')!
	defer { os.rm(path) or {} }
	mut tc, id := recursive_str_context_checker(path)
	// The root itself is outside the boundary, but the helper it invokes is not.
	unsafe { tc.a.user_code_start = int(id) + 1 }
	assert !tc.should_diagnose(id)
	assert !tc.recursive_str_has_diagnostic_context()
	tc.check_recursive_str_calls(id, *tc.a.node(id))
	assert tc.errors.len == 0
	tc.selected_file_called_fns['Value.str'] = true
	assert tc.recursive_str_has_diagnostic_context()
	tc.check_recursive_str_calls(id, *tc.a.node(id))
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == 'cannot call `str()` method recursively'
	assert int(tc.errors[0].node) > int(id)
}

fn test_recursive_str_context_preserves_selected_and_specialized_modes() {
	path := os.join_path(os.vtmp_dir(), 'recursive_str_context_modes_${os.getpid()}.v')
	os.write_file(path, 'module main
struct Value {}
fn (value Value) str() string { return value.str() }
')!
	defer { os.rm(path) or {} }
	mut tc, id := recursive_str_context_checker(path)
	assert !tc.recursive_str_has_diagnostic_context()
	tc.diagnostic_files.clear()
	assert tc.recursive_str_has_diagnostic_context()
	tc.check_recursive_str_calls(id, *tc.a.node(id))
	assert tc.errors.len == 1, tc.errors.str()
	tc.errors.clear()
	tc.diagnostic_files[path] = true
	tc.checker_fixture_mode = true
	assert tc.recursive_str_has_diagnostic_context()
	tc.check_recursive_str_calls(id, *tc.a.node(id))
	assert tc.errors.len == 1, tc.errors.str()
	tc.errors.clear()
	tc.diagnostic_files = {
		'other.v': true
	}
	tc.checker_fixture_mode = false
	tc.fn_context.concrete_generic_receiver_specialization = true
	assert tc.recursive_str_has_diagnostic_context()
	tc.check_recursive_str_calls(id, *tc.a.node(id))
	assert tc.errors.len == 1, tc.errors.str()
	tc.errors.clear()
	tc.fn_context.concrete_generic_receiver_specialization = false
	for index in 0 .. tc.a.nodes.len {
		unsafe { tc.a.specialized_fn_nodes[index] = index == int(id) }
	}
	assert tc.recursive_str_has_diagnostic_context()
	tc.check_recursive_str_calls(id, *tc.a.node(id))
	assert tc.errors.len == 1, tc.errors.str()
	tc.errors.clear()
	tc.checker_fixture_mode = true
	assert !tc.recursive_str_has_diagnostic_context()
	tc.check_recursive_str_calls(id, *tc.a.node(id))
	assert tc.errors.len == 0
	tc.valid_diagnostic_fast = true
	tc.diagnostic_files.clear()
	assert !tc.recursive_str_has_diagnostic_context()
}
