module types

import v.flat

fn test_pointer_binding_snapshots_detach_on_parameter_unknown_and_invalidation_updates() {
	mut tc := TypeChecker{}
	tc.cur_scope = new_scope(unsafe { nil })
	typ := Type(Pointer{ base_type: &builtin_int_type })
	first := tc.cur_scope.insert_with_owner('first', typ)
	second := tc.cur_scope.insert_with_owner('second', typ)
	tc.initialize_pointer_parameter_binding(first, typ)
	base := tc.share_pointer_binding_value_keys()
	tc.initialize_unknown_pointer_binding(second, typ)
	assert base.len == 1
	assert first.storage_key() in base
	assert second.storage_key() !in base
	then_state := tc.share_pointer_binding_value_keys()
	tc.invalidate_all_pointer_binding_aliases()
	assert base[first.storage_key()] == [pointer_binding_parameter_value(first.storage_key())]
	assert then_state[first.storage_key()] == [pointer_binding_parameter_value(first.storage_key())]
	assert tc.fn_context.pointer_binding_value_keys[first.storage_key()] == [pointer_binding_unknown_value(first.storage_key())]
	assert second.storage_key() in then_state
}

fn test_pointer_binding_shared_context_reset_and_clone_leave_snapshot_storage_intact() {
	mut tc := TypeChecker{}
	tc.fn_context.pointer_binding_value_keys['pointer'] = ['target']
	base := tc.share_pointer_binding_value_keys()
	saved := tc.fn_context
	mut cloned := clone_function_check_context(tc.fn_context)
	assert !cloned.pointer_binding_values_shared
	cloned.pointer_binding_value_keys['pointer'][0] = 'worker target'
	assert base['pointer'] == ['target']
	tc.fn_context = new_function_check_context()
	tc.fn_context.pointer_binding_value_keys['sibling'] = ['other target']
	tc.fn_context.reset_for_sibling_check()
	tc.fn_context = saved
	assert tc.fn_context.pointer_binding_values_shared
	tc.fn_context.reset_for_sibling_check()
	assert tc.fn_context == new_function_check_context()
	assert base == {
		'pointer': ['target']
	}
	tc.fn_context.pointer_binding_value_keys['pointer'] = ['new target']
	assert base['pointer'] == ['target']
}

fn test_pointer_binding_shared_state_detaches_before_assignment_mut_call_and_goto() {
	mut a := flat.FlatAst.new()
	pointer_id := a.add_val(.ident, 'pointer')
	target_id := a.add_val(.ident, 'target')
	label_id := a.add_val(.label_stmt, 'backward')
	start := a.begin_children()
	a.add_child(target_id)
	address_id := a.add_node(flat.Node{
		kind:           .prefix
		op:             .amp
		children_start: start
		children_count: 1
	})
	mut tc := TypeChecker.new(a)
	typ := Type(Pointer{ base_type: &builtin_int_type })
	owner := tc.cur_scope.insert_with_owner('pointer', typ)
	target := tc.cur_scope.insert_with_owner('target', builtin_int_type)
	tc.initialize_pointer_parameter_binding(owner, typ)
	base := tc.share_pointer_binding_value_keys()
	tc.update_pointer_binding_alias_assignment(pointer_id, address_id, typ, .assign, base)
	assert base[owner.storage_key()] == [pointer_binding_parameter_value(owner.storage_key())]
	assert tc.fn_context.pointer_binding_value_keys[owner.storage_key()] == [tc.pointer_binding_storage_value(target)]
	assigned := tc.share_pointer_binding_value_keys()
	tc.invalidate_pointer_binding_alias_after_mut_call(pointer_id)
	assert assigned[owner.storage_key()] == [tc.pointer_binding_storage_value(target)]
	unknown := tc.share_pointer_binding_value_keys()
	tc.record_pointer_binding_alias(owner, address_id, typ)
	assert unknown[owner.storage_key()] == [pointer_binding_unknown_value(owner.storage_key())]
	recorded := tc.share_pointer_binding_value_keys()
	tc.fn_context.pointer_alias_backward_goto_targets['backward'] = true
	tc.update_pointer_alias_goto_state(label_id)
	assert recorded[owner.storage_key()] == [tc.pointer_binding_storage_value(target)]
	assert tc.fn_context.pointer_binding_value_keys[owner.storage_key()] == [pointer_binding_unknown_value(owner.storage_key())]
}

fn test_pointer_binding_nested_shared_snapshots_preserve_merge_fallback_and_order() {
	mut tc := TypeChecker{}
	tc.fn_context.pointer_binding_value_keys = {
		'p': ['a']
		'q': ['b']
	}
	base := tc.share_pointer_binding_value_keys()
	tc.prepare_pointer_binding_write()
	tc.fn_context.pointer_binding_value_keys['p'] = ['c']
	outer_then := tc.share_pointer_binding_value_keys()
	tc.prepare_pointer_binding_write()
	tc.fn_context.pointer_binding_value_keys['q'] = ['d']
	inner_then := tc.share_pointer_binding_value_keys()
	tc.fn_context.pointer_binding_value_keys = outer_then
	tc.fn_context.pointer_binding_values_shared = true
	tc.prepare_pointer_binding_write()
	tc.fn_context.pointer_binding_value_keys['p'] = ['e']
	inner_else := tc.share_pointer_binding_value_keys()
	merged := merge_pointer_binding_snapshots([inner_then, inner_else], outer_then)
	assert merged['p'] == ['c', 'e']
	assert merged['q'] == ['d', 'b']
	assert base == {
		'p': ['a']
		'q': ['b']
	}
	assert outer_then == {
		'p': ['c']
		'q': ['b']
	}
	assert inner_then == {
		'p': ['c']
		'q': ['d']
	}
}

fn test_pointer_binding_shared_state_detaches_before_indirect_assignment() {
	mut a := flat.FlatAst.new()
	link_id := a.add_val(.ident, 'link')
	target_id := a.add_val(.ident, 'target')
	address_start := a.begin_children()
	a.add_child(target_id)
	address_id := a.add_node(flat.Node{
		kind:           .prefix
		op:             .amp
		children_start: address_start
		children_count: 1
	})
	deref_start := a.begin_children()
	a.add_child(link_id)
	deref_id := a.add_node(flat.Node{
		kind:           .prefix
		op:             .mul
		children_start: deref_start
		children_count: 1
	})
	mut tc := TypeChecker.new(a)
	typ := Type(Pointer{ base_type: &builtin_int_type })
	owner := tc.cur_scope.insert_with_owner('pointer', typ)
	link := tc.cur_scope.insert_with_owner('link', Type(Pointer{ base_type: &typ }))
	target := tc.cur_scope.insert_with_owner('target', builtin_int_type)
	tc.initialize_pointer_parameter_binding(owner, typ)
	tc.fn_context.pointer_binding_value_keys[link.storage_key()] = [owner.storage_key()]
	base := tc.share_pointer_binding_value_keys()
	tc.update_pointer_binding_alias_assignment(deref_id, address_id, typ, .assign, base)
	assert base[owner.storage_key()] == [pointer_binding_parameter_value(owner.storage_key())]
	assert tc.fn_context.pointer_binding_value_keys[owner.storage_key()] == [tc.pointer_binding_storage_value(target)]
}
