module c

import v.flat
import v.types

fn test_target_module_static_signature_keeps_empty_missing_and_alias_fallbacks() {
	for mode in ['empty_params', 'return_only', 'generic_only', 'return_with_arg', 'generic_with_arg',
		'wrong_static_arity', 'no_static'] {
		mut a := flat.FlatAst.new()
		callee := a.add_val(.ident, 'short.work')
		arg := a.add_val(.int_literal, '7')
		a.children << callee
		with_arg := mode in ['return_with_arg', 'generic_with_arg']
		if with_arg { a.children << arg }
		call := a.add_node(flat.Node{
			kind:           .call
			children_start: 0
			children_count: if with_arg {
				2
			} else {
				1
			}
		})
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.imports['short'] = 'library'
		tc.fn_param_types['library.work'] = []types.Type{}
		match mode {
			'empty_params' { tc.fn_param_types['short.work'] = []types.Type{} }
			'return_only', 'return_with_arg' {
				tc.fn_ret_types['short.work'] = types.Type(types.int_)
			}
			'generic_only', 'generic_with_arg' {
				tc.fn_generic_params['short.work'] = types.generic_param_ids(['T'])
			}
			'wrong_static_arity' { tc.fn_param_types['short.work'] = [types.Type(types.int_)] }
			else {}
		}
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		resolved := g.target_module_call_name('short.work', a.node(call)) or { '' }
		expected := match mode {
			'empty_params', 'return_only' { 'short.work' }
			'wrong_static_arity', 'no_static' { 'library.work' }
			else { '' }
		}
		assert resolved == expected, mode
	}
}

fn test_target_module_signature_preserves_variadic_and_synthetic_marker_rules() {
	for synthetic in [false, true] {
		mut a := flat.FlatAst.new()
		callee := a.add_val(.ident, 'short.work')
		arg := a.add_val(.ident, if synthetic { 'short' } else { 'value' })
		a.children << callee
		a.children << arg
		call := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 2 })
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.imports['short'] = 'library'
		tc.cur_scope.insert('short', types.Type(types.int_))
		tc.fn_param_types['short.work'] = [types.Type(types.Array{ elem_type: types.Type(types.int_) })]
		tc.fn_variadic['short.work'] = true
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		assert g.target_module_call_arg_start('short.work', a.node(call)) == if synthetic {
			2
		} else {
			1
		}
		resolved := g.target_module_call_name('short.work', a.node(call)) or { '' }
		assert resolved == if synthetic { 'short.work' } else { '' }
		if synthetic {
			assert !g.gen_transformed_method_ident_call(call, a.node(call), a.node(callee))
			assert g.sb.len == 0
		}
	}
}

fn test_transformed_method_checks_receiver_before_discarded_c_name_resolution() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'library.read')
	arg := a.add_val(.int_literal, '7')
	a.children << callee
	a.children << arg
	call := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 2 })
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.fn_param_types['library.read'] = [types.Type(types.int_)]
	tc.fn_ret_types['library.read'] = types.Type(types.int_)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert !g.gen_transformed_method_ident_call(call, a.node(call), a.node(callee))
	assert g.sb.len == 0
	assert 'library.read' !in g.c_name_cache.entries
	g.gen_call(call, a.node(call))
	assert g.sb.str() == 'library__read(7)'
}

fn test_transformed_method_keeps_source_and_emitted_parameter_lookup() {
	for emitted_only in [false, true] {
		mut a := flat.FlatAst.new()
		callee := a.add_val(.ident, 'int.read')
		arg := a.add_val(.int_literal, '7')
		a.children << callee
		a.children << arg
		call := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 2 })
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		key := if emitted_only { 'int__read' } else { 'int.read' }
		tc.fn_param_types[key] = [types.Type(types.int_)]
		tc.fn_ret_types[key] = types.Type(types.int_)
		tc.register_synth_type(arg, types.Type(types.int_))
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		assert g.gen_transformed_method_ident_call(call, a.node(call), a.node(callee))
		assert g.sb.str() == 'int__read(7)'
	}
}
