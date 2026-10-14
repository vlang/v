module transform

import v.flat
import v.types

fn interface_box_scan_node(mut a flat.FlatAst, kind flat.NodeKind, value string, typ string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		typ:            flat.source_type_id(typ)
		children_start: start
		children_count: children.len
	})
}

fn test_interface_box_scan_retains_every_multi_return_assignment_slot() {
	mut a := flat.FlatAst.new()
	first := a.add_node(flat.Node{ kind: .ident, value: 'first', typ: flat.source_type_id('Reader') })
	middle := a.add_node(flat.Node{ kind: .ident, value: 'middle', typ: flat.source_type_id('int') })
	last := a.add_node(flat.Node{ kind: .ident, value: 'last', typ: flat.source_type_id('Reader') })
	result := a.add_val(.ident, 'result')
	assignment := interface_box_scan_node(mut a, .assign, '3', '', [first, result, middle, last])
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Reader'] = true
	tc.register_synth_type(result, types.Type(types.MultiReturn{
		types: [types.Type(types.Struct{ name_id: flat.source_type_id('First') }),
			types.Type(types.int_), types.Type(types.Struct{ name_id: flat.source_type_id('Last') })]
	}))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'

	t.collect_interface_assign_boxes(a.nodes[int(assignment)])

	assert t.interface_boxed_types['Reader\nFirst']
	assert t.interface_boxed_types['Reader\nLast']
	assert !t.interface_boxed_types['Reader\nint']
}

fn test_interface_box_scan_retains_parallel_assignment_boxes() {
	mut a := flat.FlatAst.new()
	first := a.add_node(flat.Node{ kind: .ident, value: 'first', typ: flat.source_type_id('Reader') })
	last := a.add_node(flat.Node{ kind: .ident, value: 'last', typ: flat.source_type_id('Reader') })
	first_value := a.add_val(.ident, 'first_value')
	last_value := a.add_val(.ident, 'last_value')
	assignment := interface_box_scan_node(mut a, .assign, '2', '', [first, first_value, last,
		last_value])
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Reader'] = true
	tc.register_synth_type(first_value, types.Type(types.Struct{ name_id: flat.source_type_id('First') }))
	tc.register_synth_type(last_value, types.Type(types.Struct{ name_id: flat.source_type_id('Last') }))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'

	t.collect_interface_assign_boxes(a.nodes[int(assignment)])

	assert t.interface_boxed_types['Reader\nFirst']
	assert t.interface_boxed_types['Reader\nLast']
}

fn test_interface_box_scan_preserves_interface_alias_after_primitive_arguments() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'consume')
	count := a.add_val(.int_literal, '1')
	label := a.add_val(.string_literal, 'label')
	value := a.add_val(.ident, 'source')
	call := interface_box_scan_node(mut a, .call, '', '', [callee, count, label, value])
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Reader'] = true
	tc.fn_param_types['consume'] = [types.Type(types.int_), types.Type(types.String{}), types.Type(types.Alias{
		name_id:   flat.source_type_id('ReaderAlias')
		base_type: types.Type(types.Interface{ name_id: flat.source_type_id('Reader') })
	})]
	tc.register_synth_type(value, types.Type(types.Struct{ name_id: flat.source_type_id('Source') }))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'

	t.collect_interface_call_boxes(call, a.nodes[int(call)])

	assert t.interface_boxed_types['Reader\nSource']
	assert !t.interface_boxed_types['Reader\nint']
	assert !t.interface_boxed_types['Reader\nstring']
}

fn test_interface_box_param_predicate_preserves_container_and_callback_types() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Reader'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	reader := types.Type(types.Interface{ name_id: flat.source_type_id('Reader') })
	integer := types.Type(types.int_)
	for elem in [integer, reader] {
		expected := elem is types.Interface
		params := [elem, types.Type(types.Pointer{ base_type: elem }), types.Type(types.Array{
			elem_type: elem
		}), types.Type(types.ArrayFixed{ elem_type: elem, len: 2 }), types.Type(types.Map{
			key_type:   types.Type(types.String{})
			value_type: elem
		}), types.Type(types.OptionType{ base_type: elem }), types.Type(types.ResultType{
			base_type: elem
		}), types.Type(types.MultiReturn{ types: [integer, elem] })]
		for module_name in ['main', 'dependency'] {
			t.cur_module = module_name
			for param in params {
				assert t.interface_box_call_param_maybe_uncached(param) == expected
				assert t.interface_box_call_param_maybe(param) == expected
				assert t.interface_box_call_param_maybe(param) == expected
			}
			// Callback signatures do not box their parameter or return types at the
			// call site; the callback's own body handles them.
			callback := types.Type(types.FnType{ params: [elem], return_type: elem })
			assert !t.interface_box_call_param_maybe_uncached(callback)
			assert !t.interface_box_call_param_maybe(callback)
		}
	}
}

fn test_interface_array_spread_classification_and_box_scan() {
	string_type := types.Type(types.String{})
	array_type := types.Type(types.Array{ elem_type: string_type })
	fixed_type := types.Type(types.ArrayFixed{ elem_type: string_type, len: 2 })
	for i, actual in [types.Type(types.int_), string_type, array_type, fixed_type, types.Type(types.Alias{
		name_id:   flat.source_type_id('Codes')
		base_type: array_type
	}), types.Type(types.Alias{ name_id: flat.source_type_id('FixedCodes'), base_type: fixed_type })] {
		mut a := flat.FlatAst.new()
		lhs := a.add_val(.ident, 'values')
		value := a.add_val(.ident, 'source')
		spread := interface_box_scan_node(mut a, .prefix, '...', '', [value])
		append := interface_box_scan_node(mut a, .infix, '', '', [lhs, spread])
		mut tc := types.TypeChecker.new(&a)
		tc.interface_names['Value'] = true
		tc.register_synth_type(lhs, types.Type(types.Array{
			elem_type: types.Type(types.Interface{ name_id: flat.source_type_id('Value') })
		}))
		tc.register_synth_type(value, actual)
		tc.register_synth_type(spread, actual)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'

		assert t.array_append_rhs_is_push_many(lhs, spread, actual.name(), 'Value') == (i >= 2)
		t.collect_interface_append_boxes(a.nodes[int(append)])

		boxed_type := if actual is types.Primitive { 'int' } else { 'string' }
		assert t.interface_boxed_types['Value\n${boxed_type}']
		expected_keys := ['Value\n${boxed_type}', 'Value\n${c_name(boxed_type)}']
		assert t.interface_boxed_types.keys().all(it in expected_keys)
	}
}
