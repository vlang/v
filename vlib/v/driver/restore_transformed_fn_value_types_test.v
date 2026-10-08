module driver

import v.flat
import v.types

fn restored_type_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: start
		children_count: children.len
	})
}

fn test_restored_types_extend_unset_slots_and_preserve_sparse_signature_modes() {
	mut a := flat.FlatAst.new()
	existing := a.add_val(.ident, 'existing')
	unset := a.add_val(.int_literal, '1')
	first := a.add_val(.ident, 'retain')
	second := a.add_val(.ident, 'retain')
	missing := a.add_val(.ident, 'missing')
	mut tc := types.TypeChecker.new(&a)
	boolean := types.builtin_type_value('bool')
	integer := types.builtin_type_value('int')
	params := [types.Type(types.Pointer{ base_type: integer }), types.Type(types.Array{
		elem_type: boolean
	})]
	tc.expr_type_values = [boolean]
	tc.expr_type_set = [true]
	tc.fn_param_types['retain'] = params
	tc.fn_ret_types['retain'] = integer
	tc.fn_variadic['retain'] = true
	tc.mut_receiver_methods['retain'] = true
	tc.fn_ret_types['missing'] = integer
	tc.sparse_resolved_fn_values[int(first)] = 'retain'
	tc.sparse_resolved_fn_values[int(second)] = 'retain'
	tc.sparse_resolved_fn_values[int(missing)] = 'missing'
	tc.sparse_resolved_fn_values[-1] = 'retain'
	tc.sparse_resolved_fn_values[a.nodes.len + 1] = 'retain'
	restore_transformed_fn_value_types(mut tc, &a, map[string]bool{})
	assert tc.expr_type_values.len == a.nodes.len
	assert tc.expr_type_set.len == a.nodes.len
	assert tc.expr_type_values[int(existing)] == boolean && tc.expr_type_set[int(existing)]
	assert tc.expr_type_values[int(unset)] is types.Void && !tc.expr_type_set[int(unset)]
	assert tc.expr_type_values[int(missing)] is types.Void && !tc.expr_type_set[int(missing)]
	expected := types.Type(types.FnType{ params: params, is_variadic: true, return_type: integer })
	assert tc.expr_type_values[int(first)] == expected && tc.expr_type_set[int(first)]
	assert tc.expr_type_values[int(second)] == expected && tc.expr_type_set[int(second)]
	// Replacing one expression slot cannot alter another restored signature.
	tc.expr_type_values[int(first)] = boolean
	assert tc.expr_type_values[int(second)] == expected
	assert tc.fn_param_types['retain'] == params
	assert tc.fn_variadic['retain'] && tc.mut_receiver_methods['retain']
	assert (tc.expr_type_values[int(second)] as types.FnType).is_variadic
	// A later restoration must observe a refreshed fixed-array signature.
	tc.fn_variadic['retain'] = false
	restore_transformed_fn_value_types(mut tc, &a, map[string]bool{})
	fixed := types.Type(types.FnType{ params: params, return_type: integer })
	assert tc.expr_type_values[int(first)] == fixed
	assert tc.expr_type_values[int(second)] == fixed
}

fn test_restored_direct_types_keep_module_names_and_c_receiver_identity() {
	mut a := flat.FlatAst.new()
	first_module := a.add_val(.module_decl, 'first')
	first_callee := a.add_val(.ident, 'value')
	first_call := restored_type_node(mut a, .call, '', [first_callee])
	native := a.add_val(.ident, 'native')
	native_selector := restored_type_node(mut a, .selector, 'field', [native])
	local := a.add_val(.ident, 'native')
	local_selector := restored_type_node(mut a, .selector, 'field', [local])
	first_body := restored_type_node(mut a, .block, '', [first_call, native_selector, local_selector])
	first_fn := restored_type_node(mut a, .fn_decl, 'run', [first_body])
	second_module := a.add_val(.module_decl, 'second')
	second_callee := a.add_val(.ident, 'value')
	second_call := restored_type_node(mut a, .call, '', [second_callee])
	second_body := restored_type_node(mut a, .block, '', [second_call])
	second_fn := restored_type_node(mut a, .fn_decl, 'run', [second_body])
	unused_callee := a.add_val(.ident, 'value')
	unused_call := restored_type_node(mut a, .call, '', [unused_callee])
	unused_body := restored_type_node(mut a, .block, '', [unused_call])
	unused_fn := restored_type_node(mut a, .fn_decl, 'unused', [unused_body])
	a.nodes[int(first_callee)].typ = 'fn (int) int'
	a.nodes[int(unused_fn)].set_generic_params_and_constraints(['T'], ['Record'])
	a.intern_node_texts_from(0)
	canonical_nodes := a.nodes.clone()
	canonical_text_count := a.text_values.len
	mut tc := types.TypeChecker.new(&a)
	integer := types.builtin_type_value('int')
	boolean := types.builtin_type_value('bool')
	tc.top_level_idx = [int(first_module), int(first_fn), int(second_module), int(second_fn),
		int(unused_fn)]
	tc.fn_param_types['first.value'] = [integer]
	tc.fn_ret_types['first.value'] = integer
	tc.fn_param_types['second.value'] = [boolean]
	tc.fn_ret_types['second.value'] = boolean
	tc.fn_param_types['C.native'] = [boolean]
	tc.fn_ret_types['C.native'] = integer
	tc.set_resolved_fn_value(int(native), 'C.native')
	tc.register_synth_type(local, types.Type(types.Struct{ name: 'Receiver' }))
	restore_transformed_fn_value_types(mut tc, &a, {
		'main':       true
		'first.run':  true
		'second.run': true
	})
	assert tc.expr_type_values[int(first_callee)] == types.Type(types.FnType{
		params:      [integer]
		return_type: integer
	})
	assert tc.expr_type_values[int(second_callee)] == types.Type(types.FnType{
		params:      [boolean]
		return_type: boolean
	})
	assert tc.expr_type_values[int(native)] == types.Type(types.FnType{
		params:      [boolean]
		return_type: integer
	})
	assert tc.expr_type_values[int(local)] == types.Type(types.Struct{ name: 'Receiver' })
	assert !tc.expr_type_set[int(unused_callee)]
	assert_restoration_preserves_canonical_node_texts(&a, canonical_nodes, canonical_text_count)
	// A second restoration observes refreshed signatures rather than retaining
	// the previous invocation's wrapper cache.
	tc.fn_ret_types['first.value'] = boolean
	restore_transformed_fn_value_types(mut tc, &a, {
		'main':      true
		'first.run': true
	})
	assert tc.expr_type_values[int(first_callee)] == types.Type(types.FnType{
		params:      [integer]
		return_type: boolean
	})
	assert_restoration_preserves_canonical_node_texts(&a, canonical_nodes, canonical_text_count)
}

// Restoration keeps text owned by the AST intact while refreshing semantic
// signatures, so the ordinary path can omit another canonicalization barrier.
fn assert_restoration_preserves_canonical_node_texts(a &flat.FlatAst, canonical_nodes []flat.Node, canonical_text_count int) {
	assert a.nodes == canonical_nodes
	assert a.text_values.len == canonical_text_count
	for i, node in a.nodes {
		assert node.value.str == canonical_nodes[i].value.str
		assert node.typ.str == canonical_nodes[i].typ.str
		assert node.type_text_id() == canonical_nodes[i].type_text_id()
		assert node.payload == canonical_nodes[i].payload
	}
}
