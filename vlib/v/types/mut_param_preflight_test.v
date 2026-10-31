module types

import v.flat

fn test_mut_param_expr_base_empty_populated_and_cleared() {
	mut a := flat.FlatAst.new()
	receiver := a.add_val(.ident, 'receiver')
	other := a.add_val(.ident, 'other')
	start := a.begin_children()
	a.add_child(receiver)
	address := a.add_node(flat.Node{ kind: .prefix, op: .amp, children_start: start, children_count: 1 })
	mut wrapped := receiver
	for kind in [flat.NodeKind.paren, .expr_stmt, .block] {
		s := a.begin_children()
		if kind == .block { a.add_child(other) }
		a.add_child(wrapped)
		wrapped = a.add_node(flat.Node{
			kind:           kind
			children_start: s
			children_count: if kind == .block {
				2
			} else {
				1
			}
		})
	}
	base := Type(Struct{ name_id: flat.source_type_id('Receiver') })
	raw := Type(Pointer{ base_type: base })
	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	owner := tc.cur_scope.insert_with_owner('receiver', raw)
	tc.fn_context.node_id = int(receiver)
	for id in [receiver, address, wrapped, other, flat.empty_node, flat.NodeId(a.nodes.len)] {
		assert tc.mut_param_expr_base(id, raw) == none
	}
	tc.fn_context.mut_param_base_types['receiver'] = base
	tc.fn_context.mut_param_owners['receiver'] = owner
	assert tc.mut_param_expr_base(receiver, raw) or { panic('missing identifier base') } == base
	assert tc.mut_param_expr_base(wrapped, raw) or { panic('missing wrapper base') } == base
	address_base := tc.mut_param_expr_base(address, raw) or { panic('missing address base') }
	assert address_base == raw
	assert tc.mut_param_expr_base(other, raw) == none
	tc.cur_scope = new_scope(tc.cur_scope)
	tc.cur_scope.insert('receiver', raw)
	assert tc.mut_param_expr_base(receiver, raw) == none
	assert tc.mut_param_expr_base(address, raw) == none
	tc.fn_context.mut_param_base_types.clear()
	for id in [receiver, address, wrapped, other, flat.empty_node, flat.NodeId(a.nodes.len)] {
		assert tc.mut_param_expr_base(id, raw) == none
	}
}
