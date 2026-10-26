module transform

import v.flat
import v.types

fn generic_snapshot_node(mut a flat.FlatAst, template flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		...template
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn test_generic_call_arguments_read_snapshot_after_ast_growth() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'convert')
	int_arg := a.add_val(.ident, 'int')
	string_arg := a.add_val(.ident, 'string')
	index := generic_snapshot_node(mut a, flat.Node{ kind: .index }, [callee, int_arg, string_arg])
	call := generic_snapshot_node(mut a, flat.Node{ kind: .call }, [index])
	snapshot := a.nodes[int(call)]
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for _ in 0 .. a.nodes.cap + 100 {
		a.add_val(.ident, 'growth')
		a.children << flat.empty_node
	}
	assert t.explicit_generic_call_arg_text(snapshot) == 'int, string'
	assert t.explicit_generic_call_args(snapshot, 'main') or { panic('missing type arguments') } == [
		flat.source_type_int,
		flat.source_type_string,
	]
	assert t.explicit_generic_call_arg_text(flat.Node{ kind: .call, value: 'u64' }) == 'u64'
	assert t.explicit_generic_call_args(flat.Node{ kind: .call, value: 'u64' }, 'main') or { panic('missing stored type argument') } == [flat.source_type_u64]
	assert t.explicit_generic_call_arg_text(flat.Node{ kind: .call }) == ''
	assert t.explicit_generic_call_args(flat.Node{ kind: .call }, 'main') == none
	a.nodes[int(index)].value = 'range'
	assert t.explicit_generic_call_arg_text(snapshot) == ''
	assert t.explicit_generic_call_args(snapshot, 'main') == none
}

fn test_orm_fields_keep_order_duplicates_and_reassignment_behavior() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.field_init, 'first')
	second := a.add_val(.field_init, 'second')
	ignored := a.add_val(.ident, 'ignored')
	init := generic_snapshot_node(mut a, flat.Node{ kind: .struct_init, value: 'Record' }, [
		first,
		ignored,
		second,
		first,
	])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.record_orm_initialized_fields('record', init)
	assert t.orm_initialized_fields['record'] == ['first', 'second']
	t.record_orm_initialized_fields('', init)
	assert t.orm_initialized_fields.len == 1
	t.record_orm_initialized_fields('record', flat.empty_node)
	assert t.orm_initialized_fields['record'] == ['first', 'second']
	t.record_orm_initialized_fields('record', ignored)
	assert 'record' !in t.orm_initialized_fields
}

fn test_collected_declarations_preserve_field_children_and_enum_values() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'model')
	default_value := a.add_val(.int_literal, '42')
	field := generic_snapshot_node(mut a, flat.Node{ kind: .field_decl, value: 'value', typ: flat.source_type_int }, [default_value])
	generic_snapshot_node(mut a, flat.Node{ kind: .struct_decl, value: 'Record' }, [field])
	variant := a.add_val(.ident, 'int')
	generic_snapshot_node(mut a, flat.Node{ kind: .type_decl, value: 'Choice' }, [variant])
	first := generic_snapshot_node(mut a, flat.Node{ kind: .enum_field, value: 'first' }, [default_value])
	second := a.add_val(.enum_field, 'second')
	generic_snapshot_node(mut a, flat.Node{ kind: .enum_decl, value: 'Kind' }, [first, second])
	global := a.add_node(flat.Node{ kind: .ident, value: 'counter', typ: flat.source_type_i64 })
	generic_snapshot_node(mut a, flat.Node{ kind: .global_decl }, [global])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_types()
	info := t.structs['model.Record'] or { panic('missing record') }
	assert info.fields.len == 1
	assert info.fields[0].name == 'value'
	assert info.fields[0].typ == flat.source_type_int
	assert info.fields[0].default_expr == default_value
	assert t.sum_types.type_ids('model.Choice') or { panic('missing choice') } == [flat.source_type_int]
	assert t.enum_types['model.Kind'] == ['first', 'second']
	assert t.enum_explicit_first_values['model.Kind']
	assert t.globals.type_id('model.counter') or { panic('missing global') } == flat.source_type_i64
}

fn test_collected_generic_sum_preserves_payload_parameter_spelling() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'model')
	generic := a.add_val(.ident, 'T')
	concrete := a.add_val(.ident, 'Item')
	pointer := a.add_val(.ident, '&T')
	generic_snapshot_node(mut a, flat.Node{
		kind:    .type_decl
		value:   'GenericChoice'
		payload: flat.node_payload(['T'])
	}, [generic, concrete, pointer])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_types()
	variants := t.sum_types.type_ids('model.GenericChoice') or { panic('missing generic choice') }
	assert variants.map(flat.source_type_name(it)) == ['T', 'model.Item', '&T']
}
