module types

import v.flat

fn test_mutable_owner_snapshots_detach_nested_loop_bindings() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	next := a.add_val(.ident, 'next')
	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	tc.insert_mut_loop_var(value, builtin_int_type)
	outer := tc.share_mut_local_owners()
	outer_key := (outer['value'] or { panic('missing outer owner') }).storage_key()
	tc.cur_scope = new_scope(tc.cur_scope)
	tc.insert_mut_loop_var(value, builtin_string_type)
	inner := tc.share_mut_local_owners()
	inner_key := (inner['value'] or { panic('missing inner owner') }).storage_key()
	assert inner_key != outer_key
	tc.insert_mut_loop_var(next, builtin_int_type)
	assert 'next' !in outer
	assert 'next' !in inner
	assert 'next' in tc.fn_context.mut_local_owners
	assert (outer['value'] or { panic('changed outer owner') }).storage_key() == outer_key
	assert (inner['value'] or { panic('changed inner owner') }).storage_key() == inner_key
	// Restoring a snapshot must retain its sharing state before another write.
	tc.fn_context.mut_local_owners = outer
	tc.fn_context.mut_local_owners_shared = true
	tc.insert_mut_loop_var(next, builtin_string_type)
	assert 'next' !in outer
	assert 'next' !in inner
	assert tc.fn_context.mut_local_owners.len == 2
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_mutable_owner_shared_context_clone_and_reset_are_independent() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	next := a.add_val(.ident, 'next')
	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	tc.insert_mut_loop_var(value, builtin_int_type)
	snapshot := tc.share_mut_local_owners()
	saved := tc.fn_context
	mut cloned := clone_function_check_context(tc.fn_context)
	assert !cloned.mut_local_owners_shared
	cloned.mut_local_owners.delete('value')
	assert 'value' in snapshot
	assert 'value' in tc.fn_context.mut_local_owners
	tc.fn_context.reset_for_sibling_check()
	assert tc.fn_context.mut_local_owners.len == 0
	assert !tc.fn_context.mut_local_owners_shared
	assert 'value' in snapshot
	assert 'value' in saved.mut_local_owners
	tc.fn_context = saved
	assert tc.fn_context.mut_local_owners_shared
	tc.insert_mut_loop_var(next, builtin_int_type)
	assert 'next' !in snapshot
	assert 'next' !in saved.mut_local_owners
	assert 'next' in tc.fn_context.mut_local_owners
}

fn test_mutable_owner_empty_reused_snapshot_survives_insert_and_reset() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	tc.insert_mut_loop_var(value, builtin_int_type)
	tc.fn_context.reset_for_sibling_check()
	empty := tc.share_mut_local_owners()
	tc.insert_mut_loop_var(value, builtin_int_type)
	assert empty.len == 0
	assert tc.fn_context.mut_local_owners.len == 1
	populated := tc.share_mut_local_owners()
	tc.fn_context.reset_for_sibling_check()
	assert empty.len == 0
	assert populated.len == 1
	assert tc.fn_context.mut_local_owners.len == 0
	assert !tc.fn_context.mut_local_owners_shared
}

fn test_mutable_owner_branch_restores_prior_sharing_state() {
	for fast in [false, true] {
		for initially_shared in [false, true] {
			mut a := flat.FlatAst.new()
			condition := a.add_val(.bool_literal, 'true')
			body := a.add(.block)
			otherwise := a.add(.block)
			start := a.begin_children()
			for child in [condition, body, otherwise] {
				a.add_child(child)
			}
			branch := a.add_node(flat.Node{
				kind:           .if_expr
				children_start: start
				children_count: 3
			})
			mut tc := TypeChecker.new(&a)
			tc.cur_scope = new_scope(tc.file_scope)
			tc.valid_resolution_fast = fast
			tc.fn_context.mut_local_owners_shared = initially_shared
			tc.mark_statement_context(branch)
			tc.check_if_expr(branch, *a.node(branch))
			assert tc.fn_context.mut_local_owners_shared == initially_shared
			assert tc.errors.len == 0, tc.errors.str()
		}
	}
}
