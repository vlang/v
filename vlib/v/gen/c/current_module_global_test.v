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
