module transform

import v.flat
import v.types

fn test_mut_optional_argument_view_is_independent_of_storage_type() {
	mut a := flat.FlatAst.new()
	storage := flat.source_type_id('&?&int')
	view := flat.source_type_id('?int')
	addr := a.add_node(flat.Node{
		kind:    .prefix
		op:      .amp
		typ:     storage
		payload: flat.node_payload_with_constraints(['kept_marker'], ['KeptConstraint'])
	})
	a.nodes[int(addr)].set_call_type_args([flat.source_type_int])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	original_payload := t.a.nodes[int(addr)].payload
	t.mark_mut_optional_arg(addr, 0)
	assert t.a.nodes[int(addr)].payload == original_payload
	t.mark_mut_optional_arg(addr, view)
	node := t.a.nodes[int(addr)]
	assert node.typ == storage
	assert node.generic_params() == ['kept_marker', mut_optional_arg_marker]
	assert node.generic_constraint_ids().len == 0
	assert node.call_type_args() == [flat.source_type_int]
	assert node.mut_optional_arg_view_type_id() == view
	assert t.generic_call_arg_type_for_inference(addr) == view
	copy := node.clone_owned()
	cloned_view := mut_optional_arg_view_type(copy) or { panic('missing cloned optional argument view') }
	assert cloned_view == view
}

fn test_mut_optional_argument_first_view_is_retained_on_repeated_mark() {
	mut a := flat.FlatAst.new()
	addr := a.add_node(flat.Node{ kind: .prefix, op: .amp, typ: flat.source_type_id('&?&int') })
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	view := flat.source_type_id('?int')
	t.mark_mut_optional_arg(addr, view)
	payload := t.a.nodes[int(addr)].payload
	t.mark_mut_optional_arg(addr, flat.source_type_id('?string'))
	assert t.a.nodes[int(addr)].payload == payload
	assert t.a.nodes[int(addr)].generic_params() == [mut_optional_arg_marker]
	assert t.generic_call_arg_type_for_inference(addr) == view
}

fn test_mut_optional_argument_view_requires_amp_marker_and_numeric_metadata() {
	view := flat.source_type_id('?int')
	payload := flat.node_payload_with_mut_optional_arg_view_type([mut_optional_arg_marker], view)
	valid := flat.Node{ kind: .prefix, op: .amp, payload: payload }
	actual := mut_optional_arg_view_type(valid) or { panic('missing optional argument view') }
	assert actual == view
	assert mut_optional_arg_view_type(flat.Node{ kind: .ident, op: .amp, payload: payload }) == none
	assert mut_optional_arg_view_type(flat.Node{ kind: .prefix, op: .mul, payload: payload }) == none
	assert mut_optional_arg_view_type(flat.Node{ kind: .prefix, op: .amp }) == none
	assert mut_optional_arg_view_type(flat.Node{
		kind:    .prefix
		op:      .amp
		payload: flat.node_payload_with_mut_optional_arg_view_type(['other_marker'], view)
	}) == none
	assert mut_optional_arg_view_type(flat.Node{
		kind:    .prefix
		op:      .amp
		payload: flat.node_payload([mut_optional_arg_marker])
	}) == none
}

fn test_mut_optional_argument_view_uses_deferred_base_write_path() {
	mut a := flat.FlatAst.new()
	storage := flat.source_type_id('&?&int')
	view := flat.source_type_id('?int')
	addr := a.add_node(flat.Node{ kind: .prefix, op: .amp, typ: storage })
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.base_write_intercept = true
	t.shared_base_nodes = t.a.nodes.len
	t.item_range_lo = t.a.nodes.len
	t.item_range_hi = t.a.nodes.len
	t.defer_oor_writes = true
	t.mark_mut_optional_arg(addr, view)
	assert t.a.nodes[int(addr)].payload == 0
	assert t.deferred_base_writes.len == 1
	t.flush_deferred_base_writes()
	assert t.a.nodes[int(addr)].typ == storage
	assert t.a.nodes[int(addr)].mut_optional_arg_view_type_id() == view
	assert t.generic_call_arg_type_for_inference(addr) == view
}
