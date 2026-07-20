module types

import v3.flat

fn test_dereferenced_mut_receiver_checks_root_binding() {
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
	owner := tc.cur_scope.insert_with_owner('p', Type(Pointer{
		base_type: Type(int_)
	}))
	assert !tc.mut_receiver_expr_is_mutable_lvalue(p_id)
	assert !tc.mut_receiver_expr_is_mutable_lvalue(amp_id)
	assert !tc.mut_receiver_expr_is_mutable_lvalue(deref_id)

	tc.fn_context.mut_local_owners['p'] = owner
	assert tc.mut_receiver_expr_is_mutable_lvalue(p_id)
	assert tc.mut_receiver_expr_is_mutable_lvalue(amp_id)
	assert tc.mut_receiver_expr_is_mutable_lvalue(deref_id)
}
