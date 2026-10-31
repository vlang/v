module flat

fn test_constraint_ids_copy_inputs_and_preserve_independent_metadata() {
	constraint := source_type_id('pkg.Box[&Item]')
	mut inputs := [constraint, u32(0)]
	mut node := Node{
		kind:    .fn_decl
		payload: node_payload_with_constraint_ids(['T', 'U'], inputs)
	}
	inputs[0] = source_type_int
	assert node.generic_constraint_ids() == [constraint, u32(0)]
	node.set_generic_params_and_constraint_ids(['A', 'B'], inputs)
	inputs[0] = source_type_string
	assert node.generic_constraint_ids() == [source_type_int, u32(0)]
	assert node.generic_params() == ['A', 'B']
	assert node.clone_owned().generic_constraint_ids() == node.generic_constraint_ids()
}

fn test_constraint_ids_preserve_enum_and_debugger_metadata() {
	mut enum_node := Node{ kind: .enum_decl }
	enum_node.set_enum_backing_type_id(source_type_u64)
	enum_node.set_generic_params_and_constraint_ids(['T'], [source_type_int])
	assert enum_node.enum_backing_type_id() == source_type_u64
	assert enum_node.generic_constraint_ids() == [source_type_int]
	mut debugger_node := Node{ kind: .debugger_stmt }
	debugger_node.set_debugger_value_type_ids([source_type_bool])
	debugger_node.set_generic_params_and_constraint_ids(['T'], [source_type_string])
	assert debugger_node.debugger_value_type_ids() == [source_type_bool]
	assert debugger_node.generic_constraint_ids() == [source_type_string]
}
