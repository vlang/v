module transform

import v.flat
import v.types

fn test_numeric_reference_queries_remove_one_marker() {
	for text in ['', 'T', '&T', '&&T', '& &T', '&?&&T'] {
		assert transform_without_pointer_prefix(flat.source_type_id(text)) ==
			flat.source_type_id(text.trim_string_left('&'))
	}
	t := Transformer{}
	assert t.is_mut_option_param('', flat.source_type_id('&?&T'))
	assert !t.is_mut_option_param('', flat.source_type_id('&&?&T'))
}

fn test_comptime_loop_dependency_uses_type_identifiers() {
	mut a := flat.FlatAst.new()
	field_type := a.add_node(flat.Node{
		kind: .ident
		typ:  flat.source_type_id('field.typ')
	})
	nested_type := a.add_node(flat.Node{
		kind: .ident
		typ:  flat.source_type_id('map[string][]Box[field.typ]')
	})
	source_type := a.add_node(flat.Node{
		kind:         .cast_expr
		source_value: flat.source_type_id('Box[field.unaliased_typ]')
	})
	mut call := flat.Node{ kind: .call }
	call.set_call_type_args([flat.source_type_id('[]Box[field.typ]')])
	call_type := a.add_node(call)
	unrelated_type := a.add_node(flat.Node{
		kind: .ident
		typ:  flat.source_type_id('MyfieldType')
	})
	expression := a.add_node(flat.Node{
		kind:  .ident
		value: 'field.name'
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cloning_comptime_for_vars = ['field']
	assert t.subtree_references_comptime_for_vars(field_type)
	assert t.subtree_references_comptime_for_vars(nested_type)
	assert t.subtree_references_comptime_for_vars(source_type)
	assert t.subtree_references_comptime_for_vars(call_type)
	assert !t.subtree_references_comptime_for_vars(unrelated_type)
	assert t.subtree_references_comptime_for_vars(expression)
}
