module types

import v.flat

fn test_source_struct_field_decls_keeps_field_names_and_source_types() {
	mut a := flat.FlatAst.new()
	file := a.add_val(.file, 'model.v')
	mod := a.add_val(.module_decl, 'model')
	field := a.add_node(flat.Node{ kind: .field_decl, value: 'value', typ: flat.source_type_int })
	non_field := a.add_val(.ident, 'ignored')
	a.children << field
	a.children << non_field
	decl := a.add_node(flat.Node{ kind: .struct_decl, value: 'Item', children_start: 0, children_count: 2 })
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'model'
	tc.top_level_idx = [int(file), int(mod), int(decl)]
	tc.struct_files[flat.source_type_id('model.Item')] = 'model.v'
	fields := tc.source_struct_field_decls(flat.source_type_id('model.Item'))
	assert fields.len == 1
	assert fields[0].name == 'value'
	assert fields[0].typ == flat.source_type_int
	assert tc.source_struct_field_decls(flat.source_type_id('Missing')).len == 0
}

fn test_enum_selector_keeps_saved_header_after_ast_reallocation() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'Color')
	a.children << base
	selector := a.add_node(flat.Node{ kind: .selector, value: 'red', children_start: 0, children_count: 1 })
	snapshot := a.nodes[int(selector)]
	a.nodes[int(selector)].children_count = 0
	growth := a.nodes.cap + 100
	for _ in 0 .. growth {
		a.add_val(.ident, 'padding')
	}
	mut tc := TypeChecker.new(&a)
	tc.enum_names[flat.source_type_id('Color')] = true
	tc.enum_fields[flat.source_type_id('Color')] = ['red']
	from_pointer := tc.enum_selector_type(&snapshot) or { panic('missing enum selector') }
	from_value := tc.selector_type(selector, snapshot) or { panic('missing saved selector') }
	assert from_pointer.source_type_id() == flat.source_type_id('Color')
	assert from_value.source_type_id() == flat.source_type_id('Color')
	assert snapshot.children_count == 1
}
