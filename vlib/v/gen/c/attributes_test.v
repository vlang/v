module c

import v.flat
import v.token
import v.types

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
	assert g.fn_decl_inlining_prefix(template_id) == '__attribute__((noinline)) '
	assert g.fn_decl_inlining_prefix(specialization_id) == '__attribute__((noinline)) '

	g.ccompiler = 'msvc'
	assert g.fn_decl_c_attribute(specialization_id) == ''
	assert g.fn_decl_inlining_prefix(specialization_id) == '__declspec(noinline) '
}

fn test_inline_hint_preserves_external_linkage_and_specialization_attributes() {
	mut g := cgen_attribute_test_gen()
	g.ccompiler = 'clang'
	pos := token.new_span(1, 20, 40)
	source := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'helper', pos: pos })
	specialized := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'helper_T_int', pos: pos })
	g.a.specialized_fn_nodes[int(specialized)] = true
	g.decl_attrs[int(source)] = ['inline']
	g.decl_attrs_by_source_position[flat_fn_source_position_key(g.a.nodes[int(source)])] = [
		'inline',
	]
	assert g.fn_decl_inlining_prefix(source) == 'inline '
	assert g.fn_decl_inlining_prefix(specialized) == 'inline '
	g.ccompiler = 'msvc'
	assert g.fn_decl_inlining_prefix(specialized) == '__inline '
	g.decl_attrs[int(source)] = ['inline', 'noinline']
	assert g.fn_decl_inlining_prefix(source) == '__declspec(noinline) '
}

fn test_naked_attribute_is_emitted_for_every_c_compiler_but_msvc() {
	mut g := cgen_attribute_test_gen()
	g.ccompiler = 'gcc'
	pos := token.new_span(1, 20, 40)
	naked := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'naked_body', pos: pos })
	ordinary := g.a.add_node(flat.Node{ kind: .fn_decl, value: 'ordinary_body', pos: pos })
	g.decl_attrs[int(naked)] = ['_naked']

	assert g.fn_decl_naked_prefix(naked) == '__attribute__((naked)) '
	assert g.fn_decl_naked_prefix(ordinary) == ''
	// The attribute belongs before the declarator, so it must not also join the
	// `_constructor`/`_destructor` group that is written after the parameter list.
	assert g.fn_decl_c_attribute(naked) == ''

	g.ccompiler = 'clang'
	assert g.fn_decl_naked_prefix(naked) == '__attribute__((naked)) '
	// MSVC spells this `__declspec(naked)` and supports it on x86 only, and it has
	// no inline assembly for a naked body to hold in the first place.
	g.ccompiler = 'msvc'
	assert g.fn_decl_naked_prefix(naked) == ''
}
