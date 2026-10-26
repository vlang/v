module types

import v.flat

fn selector_header_test_add(mut a flat.FlatAst, base flat.NodeId, name string) flat.NodeId {
	start := a.children.len
	a.children << base
	return a.add_node(flat.Node{ kind: .selector, value: name, children_start: start, children_count: 1 })
}

fn test_enum_selector_base_helper_keeps_qualified_names_and_header_guards() {
	mut a := flat.FlatAst.new()
	module_name := a.add_val(.ident, 'dep')
	enum_name := selector_header_test_add(mut a, module_name, 'Color')
	selector := selector_header_test_add(mut a, enum_name, 'red')
	c_name := a.add_val(.ident, 'C')
	c_selector := selector_header_test_add(mut a, c_name, 'errno')
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'main.v'
	tc.cur_module = 'main'
	tc.register_file_import('dep', 'one')
	tc.enum_names['one.Color'] = true
	tc.enum_fields['one.Color'] = ['red']
	header := a.nodes[int(selector)]
	wrapped := tc.enum_selector_type(&header) or { panic('qualified wrapper missing') }
	assert wrapped.source_type_id() == flat.source_type_id('one.Color')
	direct := tc.selector_type(selector, header) or { panic('qualified direct missing') }
	assert direct.source_type_id() == flat.source_type_id('one.Color')
	mut invalid_kind := header
	invalid_kind.kind = .ident
	assert tc.enum_selector_type(&invalid_kind) == none
	mut invalid_count := header
	invalid_count.children_count = 0
	assert tc.enum_selector_type(&invalid_count) == none
	assert tc.selector_type(selector, invalid_count) == none
	mut missing := header
	missing.value = 'missing'
	assert tc.enum_selector_type(&missing) == none
	assert tc.selector_type(selector, missing) == none
	c_type := tc.selector_type(c_selector, a.nodes[int(c_selector)]) or { panic('C errno missing') }
	assert c_type.source_type_id() == flat.source_type_int
}
