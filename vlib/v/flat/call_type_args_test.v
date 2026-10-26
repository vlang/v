module flat

import v.token

fn test_call_type_args_are_owned_and_independent_of_call_markers() {
	mut args := [source_type_int, source_type_id('dep.Value[T]')]
	mut node := Node{ kind: .call, value: method_value_borrow_receiver_marker }
	node.set_call_type_args(args)
	args[0] = source_type_string
	assert node.call_type_args() == [source_type_int, source_type_id('dep.Value[T]')]
	assert node.call_type_args().data == node.payload_ptr().call_type_args.ids.data
	assert node.value == method_value_borrow_receiver_marker
	assert node.value_text() == method_value_borrow_receiver_marker
	node.value = ''
	assert node.value_text() == 'int, dep.Value[T]'
	assert node.source_value_id() == 0
	node.source_value = source_type_id('original_operand')
	assert node.value_text() == 'original_operand'
	node.source_value = 0
	node.set_call_type_args([]u32{})
	assert node.value_text() == ''
	assert node.payload == 0
}

fn test_call_type_args_survive_independent_metadata_setters_and_owned_copies() {
	args := [source_type_id('dep.Alias'), source_type_u32]
	mut node := Node{ kind: .call, children_start: 3, children_count: 2 }
	node.set_generic_params_and_constraint_ids(['T'], [source_type_id('dep.Constraint')])
	node.set_enum_backing_type_id(source_type_u16)
	node.set_debugger_value_type_ids([source_type_string])
	node.set_call_type_args(args)
	assert node.generic_params() == ['T']
	assert node.generic_constraint_ids() == [source_type_id('dep.Constraint')]
	assert node.payload_ptr().enum_backing_type == source_type_u16
	assert node.payload_ptr().debugger_value_types.ids == [source_type_string]
	node.set_generic_params(['U'])
	node.set_generic_params_and_constraints(['U'], ['dep.OtherConstraint'])
	node.set_generic_params_and_constraint_ids(['U'], [source_type_id('dep.FinalConstraint')])
	node.set_enum_backing_type_id(source_type_u8)
	node.set_debugger_value_type_ids([source_type_bool])
	assert node.call_type_args() == args
	cloned := node.clone_owned()
	assert cloned.call_type_args().data != node.call_type_args().data
	for copy in [cloned, node.with_pos(token.new_pos(2, 40)), node.with_shifted_children(7)] {
		assert copy.call_type_args() == args
		assert copy.generic_params() == ['U']
		assert copy.generic_constraint_ids() == [source_type_id('dep.FinalConstraint')]
		assert copy.payload_ptr().enum_backing_type == source_type_u8
		assert copy.payload_ptr().debugger_value_types.ids == [source_type_bool]
	}
	node.set_call_type_args([source_type_voidptr])
	assert cloned.call_type_args() == args
}

fn test_call_type_args_survive_ast_text_publication_without_spelling_entries() {
	mut ast := FlatAst.new()
	args := [source_type_id('dep.Payload[T]')]
	mut node := Node{ kind: .call }
	node.set_call_type_args(args)
	for _ in 0 .. 3 {
		ast.nodes << node
	}
	ast.intern_node_texts_range(0, 1)
	ast.intern_node_texts_range_wide(1, 2)
	ast.intern_node_texts_at([2])
	for stored in ast.nodes {
		assert stored.call_type_args() == args
		assert stored.value == ''
		assert stored.source_value == 0
		assert stored.value_text() == 'dep.Payload[T]'
	}
	assert ast.text_count() == 0
}
