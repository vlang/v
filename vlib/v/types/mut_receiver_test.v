module types

import v.flat

fn test_pointer_valued_receiver_can_mutate_pointee_without_mutable_binding() {
	mut a := flat.FlatAst.new()
	p_id := a.add_val(.ident, 'p')
	amp_children := a.begin_children()
	a.add_child(p_id)
	amp_id := a.add_node(flat.Node{
		kind:           .prefix
		op:             .amp
		children_start: amp_children
		children_count: 1
	})
	deref_children := a.begin_children()
	a.add_child(p_id)
	deref_id := a.add_node(flat.Node{
		kind:           .prefix
		op:             .mul
		children_start: deref_children
		children_count: 1
	})

	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	owner := tc.cur_scope.insert_with_owner('p', Type(Pointer{
		base_type: Type(int_)
	}))
	assert tc.mut_receiver_expr_is_mutable_lvalue(p_id)
	assert tc.mut_receiver_expr_is_mutable_lvalue(amp_id)
	assert tc.mut_receiver_expr_is_mutable_lvalue(deref_id)

	tc.fn_context.mut_local_owners['p'] = owner
	assert tc.mut_receiver_expr_is_mutable_lvalue(p_id)
	assert tc.mut_receiver_expr_is_mutable_lvalue(amp_id)
	assert tc.mut_receiver_expr_is_mutable_lvalue(deref_id)
}

fn test_global_and_local_pointer_receivers_can_mutate_their_pointees() {
	mut a := flat.FlatAst.new()
	g_id := a.add_val(.ident, 'g')
	mut tc := TypeChecker.new(&a)
	mut global_scope := tc.file_scope
	global_scope.insert('g', Type(Pointer{
		base_type: Type(int_)
	}))
	// Match the extra private file scope installed by a parallel checker.
	tc.file_scope = new_scope(global_scope)
	tc.cur_scope = new_scope(tc.file_scope)
	assert tc.mut_receiver_expr_is_mutable_lvalue(g_id)

	tc.cur_scope.insert('g', Type(Pointer{
		base_type: Type(int_)
	}))
	assert tc.mut_receiver_expr_is_mutable_lvalue(g_id)
}

fn test_const_mut_receiver_is_immutable() {
	mut a := flat.FlatAst.new()
	value_id := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	tc.const_types['value'] = Type(Struct{
		name_id: flat.source_type_id('S')
	})
	tc.cur_scope = new_scope(tc.file_scope)
	assert !tc.mut_receiver_expr_is_mutable_lvalue(value_id)

	tc.cur_scope.insert('value', Type(Struct{
		name_id: flat.source_type_id('S')
	}))
	assert !tc.mut_receiver_expr_is_mutable_lvalue(value_id)
}

fn test_mut_param_value_wrappers_preserve_tail_and_binding_identity() {
	mut a := flat.FlatAst.new()
	receiver_id := a.add_val(.ident, 'receiver')
	side_effect_id := a.add_val(.ident, 'other')
	mut wrapped_id := receiver_id
	for kind in [flat.NodeKind.paren, .expr_stmt, .block, .paren, .expr_stmt, .block] {
		start := a.begin_children()
		if kind == .block {
			a.add_child(side_effect_id)
		}
		a.add_child(wrapped_id)
		wrapped_id = a.add_node(flat.Node{
			kind:           kind
			children_start: start
			children_count: if kind == .block { 2 } else { 1 }
		})
	}
	mut empty_tail_ids := []flat.NodeId{}
	for kind in [flat.NodeKind.block, .expr_stmt, .paren] {
		empty_id := a.add(kind)
		start := a.begin_children()
		a.add_child(receiver_id)
		a.add_child(empty_id)
		empty_tail_ids << a.add_node(flat.Node{
			kind:           .block
			children_start: start
			children_count: 2
		})
	}
	start := a.begin_children()
	a.add_child(wrapped_id)
	cast_id := a.add_node(flat.Node{
		kind:           .cast_expr
		value:          'int'
		children_start: start
		children_count: 1
	})
	base := Type(Struct{ name_id: flat.source_type_id('Receiver') })
	raw := Type(Pointer{ base_type: base })
	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	owner := tc.cur_scope.insert_with_owner('receiver', raw)
	tc.fn_context.node_id = int(receiver_id)
	tc.fn_context.mut_param_base_types['receiver'] = base
	tc.fn_context.mut_param_owners['receiver'] = owner
	assert tc.mut_param_expr_base(wrapped_id, raw) or { panic('missing wrapped receiver') } == base
	assert tc.mut_param_expr_base(cast_id, raw) == none
	for id in empty_tail_ids {
		assert tc.mut_param_expr_base(id, raw) == none
	}
	assert tc.mut_param_expr_base(flat.empty_node, raw) == none
	assert tc.mut_param_expr_base(flat.NodeId(a.nodes.len), raw) == none
	tc.cur_scope = new_scope(tc.cur_scope)
	tc.cur_scope.insert('receiver', raw)
	assert tc.mut_param_expr_base(wrapped_id, raw) == none
}
