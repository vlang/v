module types

import v.flat

fn test_attribute_schema_collection_trims_source_ids_and_skips_embeds() {
	mut a := flat.FlatAst.new()
	module_id := a.add_val(.module_decl, 'main')
	label := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'label'
		typ:   flat.source_type_id(' \tstring\n')
	})
	count := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'count'
		typ:   flat.source_type_id(' int ')
	})
	embed := a.add_node(flat.Node{ kind: .field_decl, value: 'Embedded' })
	children := a.begin_children()
	for field in [label, count, embed] {
		a.add_child(field)
	}
	schema_id := a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'Route'
		children_start: children
		children_count: 3
	})
	mut tc := TypeChecker.new(&a)
	tc.top_level_idx = [int(module_id), int(schema_id)]
	tc.declaration_attributes[int(schema_id)] = ['attribute']
	tc.collect_attribute_schemas()
	schemas := tc.attribute_schemas['Route'] or { panic('missing Route schema') }
	assert schemas.len == 1
	assert schemas[0].module == 'main'
	assert schemas[0].fields.len == 2
	assert schemas[0].fields[0].name == 'label'
	assert schemas[0].fields[0].type_id == flat.source_type_string
	assert schemas[0].fields[1].name == 'count'
	assert schemas[0].fields[1].type_id == flat.source_type_int
	assert tc.attribute_field_kind(schemas[0].fields[0].type_id) == .string_
	assert tc.attribute_field_kind(schemas[0].fields[1].type_id) == .integer
}

fn test_attribute_schema_field_ids_keep_alias_and_wrapper_classification() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set_id('Count', flat.source_type_int)
	tc.enum_names['Method'] = true
	assert tc.attribute_field_kind(flat.source_type_id('Count')) == .integer
	assert tc.attribute_field_kind(flat.source_type_bool) == .boolean
	assert tc.attribute_field_kind(flat.source_type_id('Method')) == .enum_
	for spelling in ['?int', '&int', '[]int', 'map[string]int'] {
		assert tc.attribute_field_kind(flat.source_type_id(spelling)) == .unsupported
	}
}

fn test_attribute_schema_enum_id_keeps_written_module_and_short_lookup_order() {
	mut a := flat.FlatAst.new()
	written := a.add_val(.enum_decl, 'declared.Method')
	local := a.add_val(.enum_decl, 'current.Method')
	short := a.add_val(.enum_decl, 'Method')
	mut tc := TypeChecker.new(&a)
	tc.first_type_declaration_ids['declared.Method'] = int(written)
	tc.first_type_declaration_ids['current.Method'] = int(local)
	tc.first_type_declaration_ids['Method'] = int(short)
	id := flat.source_type_id('declared.Method')
	assert tc.attribute_enum_decl(id, 'current')? == written
	tc.first_type_declaration_ids.delete('declared.Method')
	assert tc.attribute_enum_decl(id, 'current')? == local
	tc.first_type_declaration_ids.delete('current.Method')
	assert tc.attribute_enum_decl(id, 'current')? == short
	tc.first_type_declaration_ids.delete('Method')
	assert tc.attribute_enum_decl(id, 'current') == none
}
