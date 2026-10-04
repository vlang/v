module types

import os
import v.flat
import v.parser
import v.pref

fn test_function_check_context_reset_detaches_shared_arrays_and_clears_semantic_state() {
	mut scope := new_scope(unsafe { nil })
	owner := scope.insert_with_owner('value', builtin_int_type)
	params := ['T', 'U']
	unsafe_states := [[{
		'value': true
	}]]
	pointer_states := [[{
		'value': ['target']
	}]]
	mut ctx := new_function_check_context()
	ctx.method_value_locals['value'] = true
	ctx.method_value_local_owners['value'] = [owner]
	ctx.method_value_local_depth['value'] = 1
	ctx.method_value_stack_mut_owners['value'] = true
	ctx.fn_value_variadic_locals['value'] = true
	ctx.fn_value_variadic_local_owners['value'] = [owner]
	ctx.fn_value_variadic_local_depth['value'] = 1
	ctx.capturing_fn_literal_locals['value'] = true
	ctx.capturing_fn_literal_local_owners['value'] = [owner]
	ctx.capturing_fn_literal_local_depth['value'] = 1
	ctx.mut_param_base_types['value'] = builtin_int_type
	ctx.mut_param_owners['value'] = owner
	ctx.mut_local_owners['value'] = owner
	ctx.closure_copy_owners['value'] = owner
	ctx.captured_interface_value_patterns['value'] = true
	ctx.shared_owners['value'] = [owner]
	ctx.shared_array_owners['value'] = [owner]
	ctx.locked_shared_names['value'] = 1
	ctx.locked_shared_modes['value'] = [u8(1)]
	ctx.locked_shared_base_names['value'] = 'base'
	ctx.pointer_binding_value_keys['value'] = ['target']
	ctx.immutable_reference_aliases['value'] = true
	ctx.unsafe_reference_alias_owners['value'] = true
	ctx.pointer_alias_goto_states['label'] = [{
		'value': ['target']
	}]
	ctx.pointer_alias_backward_goto_targets['label'] = true
	ctx.closure_forbidden_captures['value'] = true
	ctx.local_decl_rhs_by_name['value'] = [LocalDeclRhs{ rhs: flat.NodeId(1) }]
	ctx.bool_condition_exprs['value'] = flat.NodeId(1)
	ctx.unsafe_alias_break_states = unsafe_states
	ctx.pointer_alias_break_states = pointer_states
	ctx.pointer_alias_continue_states = pointer_states
	ctx.generic_params = params
	ctx.node_id = 7
	ctx.concrete_generic_receiver_specialization = true
	ctx.local_decl_rhs_indexed = true
	ctx.has_goto_nodes = true
	ctx.closure_scope = scope
	ctx.lambda_no_captures = true
	ctx.return_type = builtin_int_type
	ctx.undefined_variable_context_depth = 2
	ctx.continue_after_unknown_ident = true
	cloned := clone_function_check_context(ctx)
	ctx.reset_for_sibling_check()
	assert ctx == new_function_check_context()
	assert params == ['T', 'U']
	assert unsafe_states == [[{
		'value': true
	}]]
	assert pointer_states == [[{
		'value': ['target']
	}]]
	assert cloned.method_value_locals['value']
	assert (cloned.mut_param_base_types['value'] or { panic('lost cloned type') }) == builtin_int_type
	assert (cloned.mut_local_owners['value'] or { panic('lost cloned owner') }).storage_key() == owner.storage_key()
	// Reset retains a usable typed map, including maps that contain owners/arrays.
	ctx.method_value_locals['next'] = true
	ctx.mut_local_owners['next'] = owner
	ctx.pointer_binding_value_keys['next'] = ['new_target']
	assert !ctx.method_value_locals['value']
	assert cloned.method_value_locals['value']
	assert 'next' !in cloned.mut_local_owners
	ctx.reset_for_sibling_check()
	assert ctx == new_function_check_context()
}

fn context_reuse_seed_outer(mut tc TypeChecker) string {
	owner := tc.cur_scope.insert_with_owner('outer', builtin_int_type)
	tc.fn_context.node_id = -1
	tc.fn_context.generic_params = ['Outer']
	tc.fn_context.return_type = builtin_string_type
	tc.fn_context.mut_local_owners['outer'] = owner
	tc.fn_context.pointer_binding_value_keys['outer'] = ['outer_target']
	return owner.storage_key()
}

fn context_reuse_assert_outer(tc &TypeChecker, key string) {
	assert tc.fn_context.generic_params == ['Outer']
	assert tc.fn_context.return_type == builtin_string_type
	assert tc.fn_context.mut_local_owners.len == 1
	owner := tc.fn_context.mut_local_owners['outer'] or { panic('lost outer owner') }
	assert owner.storage_key() == key
	assert tc.fn_context.pointer_binding_value_keys == {
		'outer': ['outer_target']
	}
}

fn test_function_check_context_early_errors_restore_outer_before_reuse() {
	path := os.join_path(os.vtmp_dir(), 'v3_function_context_early_errors_${os.getpid()}.v')
	os.write_file(path, 'module main
fn public_builtin() {}
fn (global_receiver int) method() {}
fn next_sibling() {}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.diagnostic_files[path] = true
	tc.collect(a)
	items := tc.collect_parallel_check_items()
	assert items.len == 3
	assert tc.errors.len == 0, tc.errors.str()
	tc.global_names['global_receiver'] = true
	tc.declaration_visibility['builtin.public_builtin'] = DeclarationVisibility{ is_pub: true }
	outer_key := context_reuse_seed_outer(mut tc)
	outer_alias := tc.fn_context
	mut scratch := new_function_check_context()
	for item in items {
		tc.check_fn_decl_semantics_with_context(item.fn_idx, a.nodes[item.fn_idx], item.file,
			item.module, mut scratch)
		context_reuse_assert_outer(&tc, outer_key)
		scratch.reset_for_sibling_check()
		context_reuse_assert_outer(&tc, outer_key)
		assert scratch == new_function_check_context()
	}
	assert tc.errors.any(it.msg.contains('cannot redefine builtin public function')), tc.errors.str()
	assert tc.errors.any(it.msg.contains('cannot use global variable name')), tc.errors.str()
	assert tc.errors.len == 2, tc.errors.str()
	// The original active context's headers were never cleared as scratch storage.
	tc.fn_context.pointer_binding_value_keys['after'] = ['after_target']
	assert outer_alias.pointer_binding_value_keys['after'] == ['after_target']
}

fn context_reuse_check(path string, fast bool, mode int) &TypeChecker {
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.building_v_fast = fast
	tc.valid_resolution_fast = fast
	tc.diagnostic_files[path] = true
	tc.collect(a)
	items := tc.collect_parallel_check_items()
	assert items.len >= 5
	outer_key := context_reuse_seed_outer(mut tc)
	tc.capture_items = true
	if mode == 0 {
		// Direct calls retain fresh function contexts, including transform callers.
		for item in items {
			tc.check_range_lo = item.range_lo
			tc.check_range_hi = item.fn_idx
			tc.check_fn_decl_semantics(item.fn_idx, a.nodes[item.fn_idx], item.file, item.module)
			tc.item_marks << IncrementalItemMark{
				fn_idx:  item.fn_idx
				errors:  tc.errors.len
				notices: tc.notices.len
				pending: tc.pending_ierror_errors.len
			}
		}
		tc.check_range_lo = -1
		tc.check_range_hi = -1
	} else if mode == 1 {
		tc.check_fn_items_serial(items)
	} else {
		// The first arena is released before the next siblings are checked, even
		// when this fixture is too small for automatic cost-based splitting.
		middle := items.len / 2
		tc.check_scoped_batches(items[..middle], 1)
		tc.check_scoped_batches(items[middle..], 1)
	}
	context_reuse_assert_outer(&tc, outer_key)
	mut saw_generic := false
	for node in a.nodes {
		if node.kind == .fn_decl && node.value == 'identity' {
			assert node.generic_params() == ['T']
			saw_generic = true
		}
	}
	assert saw_generic
	return tc
}

fn test_function_check_context_reuse_matches_fresh_and_freed_scoped_batches() {
	path := os.join_path(os.vtmp_dir(), 'v3_function_context_reuse_${os.getpid()}.v')
	os.write_file(path, 'module main
type Value = int | string
fn identity[T](value T) T { return value }
fn choose(value Value) int {
 if value is int { return value }
 return 0
}
fn guarded(value ?int) int {
 if mut binding := value { binding++; return binding }
 return 0
}
fn closure(value int) int {
 callback := fn [value] () int { return value }
 return callback()
}
fn loop(value Value) int {
 mut total := 0
 for total < 2 { total++ }
 match value { int { return value + total } string { return total } }
}
fn sibling(binding int) int { return binding }
fn main() { _ = choose(Value(1)); _ = guarded(1); _ = closure(2); _ = loop(Value(1)); _ = sibling(3) }
')!
	defer { os.rm(path) or {} }
	for fast in [false, true] {
		reference := context_reuse_check(path, fast, 0)
		assert reference.errors.len == 0, reference.errors.str()
		for mode in [1, 2] {
			actual := context_reuse_check(path, fast, mode)
			assert actual.errors == reference.errors
			assert actual.notices == reference.notices
			assert actual.item_marks == reference.item_marks
			assert actual.method_values_by_fn == reference.method_values_by_fn
			assert actual.resolved_call_set == reference.resolved_call_set
			assert actual.expr_type_set == reference.expr_type_set
			for index, set in reference.resolved_call_set {
				if set {
					assert actual.cached_resolved_call(flat.NodeId(index)) == reference.cached_resolved_call(flat.NodeId(index))
				}
			}
			for index, set in reference.expr_type_set {
				if set {
					assert actual.expr_type_values[index].name() == reference.expr_type_values[index].name()
				}
			}
			for index, node in reference.a.nodes {
				assert actual.a.nodes[index].generic_params() == node.generic_params()
			}
		}
	}
}
