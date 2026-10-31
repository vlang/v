module transform

import v.flat
import v.types

fn borrow_mentions_decl(mut a flat.FlatAst, names []string, body flat.NodeId) flat.NodeId {
	mut params := []flat.NodeId{}
	for name in names {
		params << a.add_node(flat.Node{ kind: .param, value: name })
	}
	start := a.begin_children()
	for param in params {
		a.add_child(param)
	}
	a.add_child(body)
	return a.add_node(flat.Node{ kind: .fn_decl, children_start: start, children_count: names.len + 1 })
}

@[unsafe]
fn test_borrow_mentions_preserves_parameter_bits_contexts_and_slot_collisions() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.ident, 'left')
	for a.nodes.len < int(left) + 4096 {
		a.add_val(.ident, 'unused')
	}
	right := a.add_val(.ident, 'right')
	start := a.begin_children()
	a.add_child(left)
	a.add_child(right)
	body := a.add_node(flat.Node{ kind: .block, children_start: start, children_count: 2 })
	first := borrow_mentions_decl(mut a, ['left', 'right'], body)
	second := borrow_mentions_decl(mut a, ['right', 'left'], body)
	many := borrow_mentions_decl(mut a, []string{len: 65, init: 'argument_${index}'}, body)
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	memo := &BorrowParamMentions{}
	for decl in [first, second, first] {
		for param in ['left', 'right'] {
			t.borrow_param_mentions = memo
			t.begin_borrow_param_mentions(BorrowCandidate{ decl: int(decl), name: param }, a.node(decl))
			for id in [left, right, body, left, right] {
				expected := id == body || a.node(id).value == param
				assert t.fixed_array_expr_mentions_param(id, param) == expected
				t.borrow_param_mentions = unsafe { nil }
				assert t.fixed_array_expr_mentions_param(id, param) == expected
				t.borrow_param_mentions = memo
			}
		}
	}
	t.borrow_param_mentions = memo
	t.begin_borrow_param_mentions(BorrowCandidate{ decl: int(many), name: 'left' }, a.node(many))
	assert memo.bit == 0
	assert t.fixed_array_expr_mentions_param(left, 'left')
	assert !t.fixed_array_expr_mentions_param(right, 'left')
}
