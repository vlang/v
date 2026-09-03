module c

import v3.flat
import v3.token
import v3.types

fn cgen_attribute_test_gen() &FlatGen {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	return &g
}

fn test_noinline_attribute_is_preserved_for_generic_specialization() {
	mut g := cgen_attribute_test_gen()
	g.ccompiler = 'clang'
	source_pos := token.new_span(1, 20, 40)
	template_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'helper'
		pos:   source_pos
	})
	specialization_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'helper_T_int'
		pos:   source_pos
	})
	g.a.specialized_fn_nodes[int(specialization_id)] = true
	g.decl_attrs[int(template_id)] = ['noinline']
	g.decl_attrs_by_source_position[flat_fn_source_position_key(g.a.nodes[int(template_id)])] = [
		'noinline',
	]

	assert g.fn_decl_c_attribute(template_id) == ''
	assert g.fn_decl_c_attribute(specialization_id) == ''
	assert g.fn_decl_c_noinline_prefix(template_id) == '__attribute__((noinline)) '
	assert g.fn_decl_c_noinline_prefix(specialization_id) == '__attribute__((noinline)) '

	g.ccompiler = 'msvc'
	assert g.fn_decl_c_attribute(specialization_id) == ''
	assert g.fn_decl_c_noinline_prefix(specialization_id) == ''
	assert g.fn_decl_msvc_noinline_prefix(specialization_id) == '__declspec(noinline) '
}
