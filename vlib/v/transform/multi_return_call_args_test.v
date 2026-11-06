module transform

import v.flat
import v.types

fn test_mixed_multi_return_arguments_preserve_fixed_array_reference_checks() {
	for snapshot in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		pair_type := types.Type(types.MultiReturn{ types: [types.Type(types.int_),
			types.Type(types.int_)] })
		pair := if snapshot {
			t.make_ident('pair_snapshot')
		} else {
			t.make_call_typed('pair', [], t.semantic_type_name(pair_type))
		}
		tc.register_synth_type(pair, pair_type)
		fixed_type := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
		array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
		t.set_var_type('fixed', flat.source_type_id('[2]int'))
		tc.cur_scope.insert('fixed', fixed_type)
		fixed := t.make_ident('fixed')
		tc.register_synth_type(fixed, fixed_type)
		params := [types.Type(types.int_), types.Type(types.int_), array_ref]
		tc.fn_param_types['retain'] = params
		tc.fn_ret_types['retain'] = array_ref
		call := t.make_call_typed('retain', [pair, fixed], flat.source_type_id('&[]int'))
		t.set_resolved_call_entry(int(call), 'retain')
		node := t.a.nodes[int(call)]
		assert t.call_has_mut_fixed_array_args(node, params, 0, -1)
		assert t.call_argument_borrows_fixed_array(call, node, 2)
		t.mark_fixed_array_reference_argument_escapes(call, node, map[string]bool{}, map[string][]string{}, map[string]string{}, {
			'fixed': true
		})
		assert 'fixed' in t.escaping_fixed_array_view_sources
	}
}

fn test_mixed_multi_return_arguments_preserve_implicit_reference_escape_checks() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	pair_type := types.Type(types.MultiReturn{ types: [types.Type(types.int_), types.Type(types.int_)] })
	pair := t.make_call_typed('pair', [], t.semantic_type_name(pair_type))
	tc.register_synth_type(pair, pair_type)
	value := t.make_ident('value')
	tc.register_synth_type(value, types.Type(types.Struct{ name_id: flat.source_type_id('Value') }))
	tc.fn_param_types['retain'] = [types.Type(types.int_), types.Type(types.int_),
		tc.parse_type(flat.source_type_voidptr)]
	tc.fn_ret_types['retain'] = types.Type(types.int_)
	call := t.make_call_typed('retain', [pair, value], flat.source_type_int)
	t.set_resolved_call_entry(int(call), 'retain')
	node := t.a.nodes[int(call)]
	t.mark_implicit_reference_argument_escapes(call, node, map[string][]string{}, map[string]string{}, {
		'value': true
	})
	assert 'value' in t.escaping_amp_sources
	t.escaping_amp_sources.clear()
	t.preserve_call_provenance = true
	t.mark_call_provenance_argument_escapes(call, node, map[string][]string{}, map[string]string{}, map[string][]string{})
	assert 'value' in t.escaping_amp_sources
}

fn test_mixed_multi_return_borrow_proof_uses_the_expanded_parameter() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	pair_type := types.Type(types.MultiReturn{ types: [types.Type(types.int_), types.Type(types.int_)] })
	pair := t.make_call_typed('pair', [], t.semantic_type_name(pair_type))
	tc.register_synth_type(pair, pair_type)
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	value := t.make_ident('value')
	tc.register_synth_type(value, array_ref)
	decl := a.add_node(flat.Node{ kind: .fn_decl, value: 'retain' })
	t.call_param_types_index_ready = true
	t.call_param_types_decl_index['retain'] = FnParamDeclRef{ idx: int(decl) }
	t.call_param_types_decl_cache[int(decl)] = [types.Type(types.int_), types.Type(types.int_),
		array_ref]
	t.fixed_array_borrow_params[int(decl)] = [true, true, false]
	call := t.make_call_typed('retain', [pair, value], flat.source_type_id('&[]int'))
	t.set_resolved_call_entry(int(call), 'retain')
	mut dependencies := []BorrowParamRef{}
	assert t.fixed_array_call_uses_borrow(call, a.nodes[int(call)], 'value', mut dependencies)
	assert dependencies == [BorrowParamRef{ decl: int(decl), index: 2 }]
}
