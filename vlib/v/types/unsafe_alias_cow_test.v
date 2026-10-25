module types

import v.flat

fn test_unsafe_alias_shared_snapshots_detach_before_assignment_and_removal() {
	mut a := flat.FlatAst.new()
	alias_id := a.add_val(.ident, 'alias')
	plain_id := a.add_val(.ident, 'plain')
	start := a.begin_children()
	a.add_child(plain_id)
	unsafe_id := a.add_node(flat.Node{
		kind:           .block
		value:          'unsafe'
		children_start: start
		children_count: 1
	})
	mut tc := TypeChecker.new(a)
	typ := Type(Map{ key_type: &builtin_string_type, value_type: &builtin_int_type })
	owner := tc.cur_scope.insert_with_owner('alias', typ)
	tc.cur_scope.insert('plain', typ)
	tc.fn_context.unsafe_reference_alias_owners['existing'] = true
	base := tc.share_unsafe_alias_owners()
	tc.update_unsafe_reference_alias_assignment(alias_id, unsafe_id, typ, .assign)
	assert owner.storage_key() !in base
	assert base == {
		'existing': true
	}
	assert tc.fn_context.unsafe_reference_alias_owners[owner.storage_key()]
	assigned := tc.share_unsafe_alias_owners()
	tc.update_unsafe_reference_alias_assignment(alias_id, plain_id, typ, .assign)
	assert assigned[owner.storage_key()]
	assert owner.storage_key() !in tc.fn_context.unsafe_reference_alias_owners
	assert tc.fn_context.unsafe_reference_alias_owners['existing']
}

fn test_unsafe_alias_shared_context_clone_restore_and_reset_leave_snapshot_intact() {
	mut tc := TypeChecker{}
	tc.fn_context.unsafe_reference_alias_owners['alias'] = true
	base := tc.share_unsafe_alias_owners()
	saved := tc.fn_context
	mut cloned := clone_function_check_context(tc.fn_context)
	assert !cloned.unsafe_alias_owners_shared
	cloned.unsafe_reference_alias_owners.delete('alias')
	assert base['alias']
	tc.fn_context = new_function_check_context()
	tc.fn_context.unsafe_reference_alias_owners['sibling'] = true
	tc.fn_context.reset_for_sibling_check()
	tc.fn_context = saved
	assert tc.fn_context.unsafe_alias_owners_shared
	tc.fn_context.reset_for_sibling_check()
	assert tc.fn_context == new_function_check_context()
	assert base == {
		'alias': true
	}
	tc.fn_context.unsafe_reference_alias_owners['next'] = true
	assert 'next' !in base
}

fn test_unsafe_alias_nested_shared_snapshots_preserve_intersection_fallback() {
	mut tc := TypeChecker{}
	tc.fn_context.unsafe_reference_alias_owners = {
		'a': true
		'b': true
	}
	base := tc.share_unsafe_alias_owners()
	tc.prepare_unsafe_alias_owner_write()
	tc.fn_context.unsafe_reference_alias_owners.delete('a')
	outer := tc.share_unsafe_alias_owners()
	tc.prepare_unsafe_alias_owner_write()
	tc.fn_context.unsafe_reference_alias_owners['c'] = true
	inner_then := tc.share_unsafe_alias_owners()
	tc.fn_context.unsafe_reference_alias_owners = outer
	tc.fn_context.unsafe_alias_owners_shared = true
	tc.prepare_unsafe_alias_owner_write()
	tc.fn_context.unsafe_reference_alias_owners['d'] = true
	inner_else := tc.share_unsafe_alias_owners()
	assert intersect_unsafe_reference_alias_states([inner_then, inner_else], base) == {
		'b': true
	}
	mut fallback := intersect_unsafe_reference_alias_states([], base)
	fallback.delete('a')
	assert base == {
		'a': true
		'b': true
	}
	assert outer == {
		'b': true
	}
	assert inner_then == {
		'b': true
		'c': true
	}
	assert inner_else == {
		'b': true
		'd': true
	}
}
