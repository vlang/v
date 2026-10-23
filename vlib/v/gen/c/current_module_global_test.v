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
