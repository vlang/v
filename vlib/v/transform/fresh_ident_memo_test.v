module transform

import v.flat
import v.types

fn test_fresh_ident_keeps_existing_body_memo_and_binding_type() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'items')
	index := a.add_val(.int_literal, '0')
	start := a.begin_children()
	a.add_child(base)
	a.add_child(index)
	lookup := a.add_node(flat.Node{
		kind:           .index
		children_start: i32(start)
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('items', flat.source_type_id('[]string'))
	t.begin_node_type_memo(0, a.nodes.len - 1)
	assert t.node_type(lookup) == flat.source_type_string
	generation := t.node_type_memo.generation
	filled := t.node_type_memo.filled[int(lookup)]
	for name in ['items', 'unbound'] {
		prior_len := a.nodes.len
		if name == 'items' {
			tc.expr_type_set = []bool{len: prior_len + 1}
			tc.expr_type_set[prior_len] = true
		}
		fresh := t.make_ident(name)
		assert int(fresh) == prior_len
		node := a.node(fresh)
		if name == 'items' {
			assert !tc.expr_type_set[int(fresh)]
		}
		assert node.kind == .ident
		assert node.value == name
		assert node.typ == if name == 'items' { flat.source_type_id('[]string') } else { u32(0) }
		assert t.node_type_memo.generation == generation
		assert t.node_type_memo.filled[int(lookup)] == filled
		assert t.node_type(lookup) == flat.source_type_string
	}
	// Real writes to an attached descendant still invalidate its cached ancestor.
	t.set_var_type('other', flat.source_type_id('[]bool'))
	t.set_node_value(int(base), 'other')
	assert t.node_type(lookup) == flat.source_type_bool
}
