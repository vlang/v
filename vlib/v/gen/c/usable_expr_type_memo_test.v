module c

import v.flat
import v.types

fn test_usable_expr_type_memo_node_zero_starts_empty_and_reuses_its_result() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	assert id == 0
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['value'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(id) == types.Type(types.int_)
	// Change the uncached source to distinguish reuse from another resolution.
	g.cur_param_types['value'] = types.Type(types.bool_)
	assert g.usable_expr_type(id) == types.Type(types.int_)
}

fn test_usable_expr_type_memo_requires_the_exact_node_id_after_slot_collisions() {
	mut a := flat.FlatAst.new()
	a.nodes = []flat.Node{len: usable_expr_type_memo_slots + 1}
	a.nodes[0] = flat.Node{ kind: .ident, value: 'first' }
	a.nodes[usable_expr_type_memo_slots] = flat.Node{ kind: .ident, value: 'second' }
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['first'] = types.Type(types.int_)
	g.cur_param_types['second'] = types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(flat.NodeId(0)) == types.Type(types.int_)
	assert g.usable_expr_type(flat.NodeId(usable_expr_type_memo_slots)) == types.Type(types.String{})
	assert g.usable_expr_type(flat.NodeId(0)) == types.Type(types.int_)
	assert g.usable_expr_type(flat.NodeId(usable_expr_type_memo_slots)) == types.Type(types.String{})
}

fn test_usable_expr_type_memo_bypasses_completed_bodies_and_resets_for_the_next_body() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['value'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(id) == types.Type(types.int_)
	g.end_usable_expr_type_memo()
	g.cur_param_types['value'] = types.Type(types.String{})
	assert g.usable_expr_type(id) == types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(id) == types.Type(types.String{})
	g.cur_param_types['value'] = types.Type(types.bool_)
	assert g.usable_expr_type(id) == types.Type(types.String{})
}

fn test_usable_expr_type_memo_rollover_does_not_revive_generation_one_entries() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.ident, 'first')
	second := a.add_val(.ident, 'second')
	mut g := FlatGen.new()
	g.a = &a
	g.memo_usable_expr_types = true
	g.cur_param_types['first'] = types.Type(types.int_)
	g.cur_param_types['second'] = types.Type(types.String{})
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type(first) == types.Type(types.int_)
	assert g.usable_expr_type(second) == types.Type(types.String{})
	g.end_usable_expr_type_memo()
	g.usable_expr_type_memo.generation = ~u32(0)
	g.cur_param_types['first'] = types.Type(types.bool_)
	g.cur_param_types['second'] = types.Type(types.int_)
	g.begin_usable_expr_type_memo()
	assert g.usable_expr_type_memo.generation == 1
	assert g.usable_expr_type(first) == types.Type(types.bool_)
	assert g.usable_expr_type(second) == types.Type(types.int_)
}
