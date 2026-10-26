module c

import v.flat
import v.types

fn test_foreign_global_does_not_shadow_current_module_qualifier() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'answer'
	mut g := FlatGen.new()
	g.tc = &tc
	g.const_vals['answer.value'] = flat.NodeId(1)
	g.global_types['other.answer'] = types.Type(types.int_)
	g.global_modules['answer'] = 'other'
	g.global_modules['other.answer'] = 'other'
	assert g.current_module_selector_const_name('answer', 'value') or { '' } == 'answer.value'
	g.global_types['answer.answer'] = types.Type(types.int_)
	g.global_modules['answer.answer'] = 'answer'
	assert g.current_module_selector_const_name('answer', 'value') == none
}

fn test_bare_main_and_builtin_globals_keep_value_precedence_after_foreign_owner() {
	for current in ['main', 'builtin'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = current
		mut g := FlatGen.new()
		g.tc = &tc
		g.const_vals['value'] = flat.NodeId(1)
		g.const_modules['value'] = current
		g.global_types[current] = types.Type(types.int_)
		g.global_modules[current] = 'other'
		g.global_types['other.${current}'] = types.Type(types.int_)
		g.global_modules['other.${current}'] = 'other'
		assert g.current_module_global_type_for_ident(current) != none
		assert g.current_module_selector_const_name(current, 'value') == none
	}
}

fn test_ident_emission_keeps_scoped_and_parameter_value_precedence() {
	for scoped in [false, true] {
		mut a := flat.FlatAst.new()
		id := a.add_val(.ident, 'value')
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.cur_scope = types.new_scope(tc.file_scope)
		if scoped {
			tc.cur_scope.insert('value', types.Type(types.int_))
		}
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.global_types['value'] = types.Type(types.int_)
		g.global_modules['value'] = 'main'
		g.const_vals['value'] = flat.NodeId(1)
		g.const_modules['value'] = 'main'
		g.cur_param_types['value'] = &types.Type(types.int_)
		expected := g.local_decl_cname('value')
		g.gen_nonliteral_expr(id, a.node(id))
		assert g.sb.str() == expected
	}
}

fn test_ident_emission_uses_nearest_binding_storage_flags() {
	for shared_storage in [false, true] {
		mut a := flat.FlatAst.new()
		id := a.add_val(.ident, 'value')
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.cur_scope = types.new_scope(tc.file_scope)
		outer := tc.cur_scope.insert_with_owner('value', types.Type(types.int_))
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		if shared_storage {
			g.declare_local_shared_storage(outer, true)
		} else {
			g.declare_local_implicit_deref(outer, true)
		}
		g.gen_nonliteral_expr(id, a.node(id))
		assert g.sb.str() == if shared_storage { 'value->val' } else { '(*value)' }
		tc.cur_scope = types.new_scope(tc.cur_scope)
		tc.cur_scope.insert('value', types.Type(types.int_))
		g.gen_nonliteral_expr(id, a.node(id))
		assert g.sb.str() == 'value'
	}
}

fn test_global_type_name_filter_preserves_every_lookup_route_and_rebuild() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.global_types['bare'] = types.Type(types.int_)
	g.global_types['pkg.value'] = types.Type(types.string_)
	g.global_types['other.alias'] = types.Type(types.bool_)
	g.global_types['pkg.世界'] = types.Type(types.u8_)
	g.global_modules['alias'] = 'other'
	g.global_modules['other.alias'] = 'other'
	queries := ['', 'bare', 'value', 'pkg.value', 'alias', 'other.alias', '世界', 'pkg.世界',
		'absent']
	mut expected := map[string]string{}
	for module_name in ['main', 'builtin', 'pkg', 'other'] {
		tc.cur_module = module_name
		for query in queries {
			typ := g.global_type_for_ident(query)
			expected['${module_name}/${query}'] = if value := typ { value.name() } else { '<none>' }
		}
	}
	g.precompute_local_global_suffix_names()
	g.precompute_c_typedef_shadow_names()
	g.precompute_local_name_exceptions()
	for module_name in ['main', 'builtin', 'pkg', 'other'] {
		tc.cur_module = module_name
		for query in queries {
			typ := g.global_type_for_ident(query)
			actual := if value := typ { value.name() } else { '<none>' }
			assert actual == expected['${module_name}/${query}'], '${module_name}/${query}'
		}
	}
	// A filter collision must retain the exact map lookup, including a miss.
	tc.cur_module = 'main'
	assert global_name_filter_slot('bzre') == global_name_filter_slot('bare')
	assert g.global_type_for_ident('bzre') == none
	g.global_types['new_global'] = types.Type(types.u64_)
	g.precompute_local_name_exceptions()
	assert g.global_type_for_ident('new_global')?.name() == 'u64'
	g.global_types.clear()
	g.global_modules.clear()
	g.global_types['replacement'] = types.Type(types.bool_)
	g.precompute_local_name_exceptions()
	assert g.global_type_for_ident('replacement')?.name() == 'bool'
	assert g.global_type_for_ident('bare') == none
}

fn test_call_function_value_signature_keeps_mut_and_empty_parameter_abi() {
	for mode in ['parameter', 'local', 'global'] {
		for param_count in [0, 1] {
			for is_mut in [false, true] {
				mut a := flat.FlatAst.new()
				callee := a.add_val(.ident, 'callback')
				arg := a.add_val(.ident, 'value')
				a.children << callee
				a.children << arg
				call := a.add_node(flat.Node{ kind: .call, children_start: 0, children_count: 2 })
				mut tc := types.TypeChecker.new(&a)
				tc.cur_module = 'main'
				tc.cur_scope = types.new_scope(tc.file_scope)
				tc.cur_scope.insert('value', types.Type(types.int_))
				mut params := []types.FnParam{}
				if param_count > 0 {
					params << types.FnParam{ typ: tc.intern_type_reference(types.Type(types.int_)), is_mut: is_mut }
				}
				fn_type := tc.intern_type_reference(types.Type(types.FnType{ params: params }))
				mut g := FlatGen.new()
				g.a = &a
				g.tc = &tc
				match mode {
					'parameter' { g.cur_param_types['callback'] = fn_type }
					'local' { tc.cur_scope.insert('callback', *fn_type) }
					else { g.global_types['callback'] = *fn_type }
				}
				g.gen_call(call, a.node(call))
				expected_arg := if param_count == 0 {
					''
				} else if is_mut {
					'&value'
				} else {
					'value'
				}
				assert g.sb.str() == 'callback(${expected_arg})', '${mode}/${param_count}/${is_mut}: ${g.sb.str()}'
			}
		}
	}
}
