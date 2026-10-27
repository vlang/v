module transform

import v.flat
import v.types

fn test_callee_signature_keeps_raw_binding_precedence_and_checked_fallback() {
	mut a := flat.FlatAst.new()
	callback := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('RawCallback'), flat.source_type_id('fn () string'))
	tc.register_synth_type(callback, tc.parse_type(flat.source_type_id('fn () bool')))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type_with_raw('callback', flat.source_type_id('fn () int'), flat.source_type_id('RawCallback'))
	raw_signature := t.call_callee_fn_type(callback) or { panic('missing raw callback') }
	assert raw_signature.return_type is types.String
	// An unusable raw spelling falls through to the live binding, then the checker.
	t.set_var_type_with_raw('callback', flat.source_type_id('fn () int'), flat.source_type_int)
	live_signature := t.call_callee_fn_type(callback) or { panic('missing live callback') }
	assert types.semantic_types_equal(*live_signature.return_type, types.Type(types.int_))
	t.unset_var_type('callback')
	checked_signature := t.call_callee_fn_type(callback) or { panic('missing checked callback') }
	assert types.semantic_types_equal(*checked_signature.return_type, types.Type(types.bool_))
}

fn test_generic_callback_detection_keeps_each_type_fallback() {
	mut a := flat.FlatAst.new()
	callback := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	callback_type := flat.source_type_id('fn (int) int')
	t.set_var_type_with_raw('callback', flat.source_type_int, callback_type)
	assert t.generic_call_base_is_fn_value(callback, a.node(callback))
	t.set_var_type_with_raw('callback', callback_type, flat.source_type_int)
	assert t.generic_call_base_is_fn_value(callback, a.node(callback))
	t.unset_var_type('callback')
	t.set_node_typ(int(callback), callback_type)
	assert t.generic_call_base_is_fn_value(callback, a.node(callback))
	t.set_node_typ(int(callback), 0)
	tc.register_synth_type(callback, tc.parse_type(callback_type))
	assert t.generic_call_base_is_fn_value(callback, a.node(callback))
	other := a.add_val(.ident, 'ordinary')
	t.set_var_type('ordinary', flat.source_type_int)
	assert !t.generic_call_base_is_fn_value(other, a.node(other))
}

fn test_known_callback_views_do_not_resolve_later_type_candidates() {
	mut a := flat.FlatAst.new()
	callback := a.add_node(flat.Node{
		kind:  .ident
		value: 'callback'
		typ:   flat.source_type_id('fn () int')
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.begin_node_type_memo(0, int(callback))
	assert t.call_arg_is_fn_pointer_value(callback, a.node(callback))
	assert t.node_type_memo.filled[int(callback)] == 0
	t.set_var_type('callback', flat.source_type_id('fn () string'))
	assert t.generic_call_base_is_fn_value(callback, a.node(callback))
	assert t.node_type_memo.filled[int(callback)] == 0
}

fn test_resolved_receiver_keeps_raw_alias_and_pointer_views() {
	for spelling in ['Thing', '&Thing', 'AliasThing'] {
		mut a := flat.FlatAst.new()
		base := a.add_node(flat.Node{ kind: .ident, value: 'object', typ: flat.source_type_id(spelling) })
		mut tc := types.TypeChecker.new(&a)
		tc.type_aliases.set_id(flat.source_type_id('AliasThing'), flat.source_type_id('Thing'))
		tc.structs[flat.source_type_id('Thing')] = []types.StructField{}
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.structs['Thing'] = StructInfo{ name: flat.source_type_id('Thing') }
		t.set_var_type_with_raw('object', flat.source_type_id(spelling), flat.source_type_id(spelling))
		result := t.receiver_base_for_resolved_method(base, 'Thing.run', [], flat.source_type_id(spelling))
		assert a.node(result).kind == .ident
		assert a.node(result).value == 'object'
		assert t.expected_expr_type == 0
		assert t.pending_stmts.len == 0
	}
}
