module types

import v.flat

fn test_required_field_filter_invalidates_after_ast_growth() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.prepare_check_name_filters()
	assert !tc.has_required_fields
	assert tc.required_fields_ast_nodes == a.nodes.len
	mut field := flat.Node{ kind: .field_decl, value: 'value', typ: flat.source_type_id('int') }
	field.set_generic_params(['', 'required'])
	field_id := a.add_node(field)
	start := a.begin_children()
	a.add_child(field_id)
	decl_id := a.add_node(flat.Node{ kind: .struct_decl, value: 'Required', children_start: start, children_count: 1 })
	tc.type_declaration_ids[flat.source_type_id('Required')] = [int(decl_id)]
	assert tc.required_fields_ast_nodes != a.nodes.len
	assert tc.missing_required_struct_fields(flat.source_type_id('Required'), map[string]bool{}, []u32{}) == ['Required.value']
	tc.prepare_check_name_filters()
	assert tc.has_required_fields
	fork := tc.fork_for_parallel_codegen()
	assert fork.has_required_fields
	assert fork.missing_required_struct_fields(flat.source_type_id('Required'), map[string]bool{}, []u32{}) == ['Required.value']
}

fn test_required_field_filter_checks_complete_attribute_names() {
	mut a := flat.FlatAst.new()
	mut field := flat.Node{ kind: .field_decl, value: 'value' }
	field.set_generic_params(['', 'not_required'])
	a.add_node(field)
	mut tc := TypeChecker.new(&a)
	tc.prepare_check_name_filters()
	assert !tc.has_required_fields
}
