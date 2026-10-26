module flat

import v.token

fn test_lowered_type_metadata_slots_preserve_each_other_and_existing_metadata() {
	fn_type := source_type_id('fn (string) int')
	variant := source_type_id('dep.Variant')
	view := source_type_id('?&dep.Record')
	mut node := Node{ kind: .selector, value: 'method', typ: source_type_int, children_start: 2 }
	node.set_generic_params_and_constraint_ids(['T'], [source_type_id('dep.Constraint')])
	node.set_enum_backing_type_id(source_type_u16)
	node.set_debugger_value_type_ids([source_type_bool])
	node.set_call_type_args([source_type_string])
	node.set_fn_value_type_id(fn_type)
	before := node.payload
	node.set_generated_variant_type_id(variant)
	node.set_mut_optional_arg_view_type_id(view)
	assert node_payload_at(before).generated_variant_type == 0
	assert node_payload_at(before).mut_optional_arg_view_type == 0
	node.set_generic_params(['U'])
	node.set_generic_params_and_constraints(['U'], ['dep.OtherConstraint'])
	node.set_generic_params_and_constraint_ids(['U'], [source_type_id('dep.FinalConstraint')])
	node.set_enum_backing_type_id(source_type_u8)
	node.set_debugger_value_type_ids([source_type_string])
	node.set_call_type_args([source_type_u32])
	for copy in [node.clone_owned(), node.with_pos(token.new_pos(2, 40)),
		node.with_shifted_children(7)] {
		assert copy.fn_value_type_id() == fn_type
		assert copy.generated_variant_type_id() == variant
		assert copy.mut_optional_arg_view_type_id() == view
		assert copy.has_lowered_type_metadata()
		assert copy.generic_params() == ['U']
		assert copy.generic_constraint_ids() == [source_type_id('dep.FinalConstraint')]
		assert copy.payload_ptr().enum_backing_type == source_type_u8
		assert copy.payload_ptr().debugger_value_types.ids == [source_type_string]
		assert copy.call_type_args() == [source_type_u32]
		assert copy.typ == source_type_int
	}
	cloned := node.clone_owned()
	assert cloned.generic_params().data != node.generic_params().data
	assert cloned.call_type_args().data != node.call_type_args().data
	node.set_fn_value_type_id(source_type_id('fn () bool'))
	assert cloned.fn_value_type_id() == fn_type
}

fn test_lowered_type_metadata_combined_setters_publish_one_independent_payload() {
	mut node := Node{ kind: .selector }
	node.set_generic_params_and_constraint_ids(['T'], [source_type_id('dep.Constraint')])
	node.set_fn_value_type_id(source_type_id('fn () int'))
	node.set_generic_params_and_generated_variant_type_id(['U'], source_type_id('dep.Variant'))
	assert node.generic_constraint_ids() == [source_type_id('dep.Constraint')]
	node.set_generic_params_and_mut_optional_arg_view_type_id(['U', 'marker'], source_type_id('?int'))
	assert node.generic_constraint_ids().len == 0
	node.set_generic_params_and_fn_value_type_id(['marker'], source_type_id('fn () bool'))
	assert node.fn_value_type_id() == source_type_id('fn () bool')
	assert node.generated_variant_type_id() == source_type_id('dep.Variant')
	assert node.mut_optional_arg_view_type_id() == source_type_id('?int')
	assert node.generic_params() == ['marker']
}

fn test_scalar_only_lowered_type_metadata_survives_ast_publication_without_text_entries() {
	mut ast := FlatAst.new()
	mut node := Node{ kind: .selector }
	node.set_fn_value_type_id(source_type_id('fn (dep.Record) []int'))
	node.set_generated_variant_type_id(source_type_id('dep.Variant'))
	node.set_mut_optional_arg_view_type_id(source_type_id('?&dep.Record'))
	for _ in 0 .. 3 {
		ast.nodes << node
	}
	ast.intern_node_texts_range(0, 1)
	ast.intern_node_texts_range_wide(1, 2)
	ast.intern_node_texts_at([2])
	for stored in ast.nodes {
		assert stored.fn_value_type_id() == node.fn_value_type_id()
		assert stored.generated_variant_type_id() == node.generated_variant_type_id()
		assert stored.mut_optional_arg_view_type_id() == node.mut_optional_arg_view_type_id()
		assert stored.payload != node.payload
		assert stored.generic_params().len == 0
	}
	assert ast.text_count() == 0
	node.set_fn_value_type_id(0)
	node.set_generated_variant_type_id(0)
	node.set_mut_optional_arg_view_type_id(0)
	assert node.payload == 0
	assert !node.has_lowered_type_metadata()
	reserved := Node{ payload: canonical_comptime_type_payload }
	assert reserved.fn_value_type_id() == 0
	assert reserved.generated_variant_type_id() == 0
	assert reserved.mut_optional_arg_view_type_id() == 0
	assert reserved.clone_owned().payload == canonical_comptime_type_payload
}

fn test_lowered_type_metadata_factories_keep_marker_names_separate() {
	fn_type := source_type_id('fn () int')
	variant := source_type_id('dep.Variant')
	view := source_type_id('?int')
	function := Node{ payload: node_payload_with_fn_value_type(['fn_marker'], fn_type) }
	variant_node := Node{ payload: node_payload_with_generated_variant_type(['variant_marker'], variant) }
	optional := Node{ payload: node_payload_with_mut_optional_arg_view_type(['optional_marker'], view) }
	assert function.generic_params() == ['fn_marker']
	assert function.fn_value_type_id() == fn_type
	assert variant_node.generic_params() == ['variant_marker']
	assert variant_node.generated_variant_type_id() == variant
	assert optional.generic_params() == ['optional_marker']
	assert optional.mut_optional_arg_view_type_id() == view
	assert node_payload_with_fn_value_type([]string{}, 0) == 0
	assert node_payload_with_generated_variant_type([]string{}, 0) == 0
	assert node_payload_with_mut_optional_arg_view_type([]string{}, 0) == 0
}
