module transform

import v.flat
import v.types

fn interface_box_scan_node(mut a flat.FlatAst, kind flat.NodeKind, value string, typ string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		typ:            typ
		children_start: start
		children_count: children.len
	})
}

fn test_interface_box_scan_retains_every_multi_return_assignment_slot() {
	mut a := flat.FlatAst.new()
	first := a.add_node(flat.Node{ kind: .ident, value: 'first', typ: 'Reader' })
	middle := a.add_node(flat.Node{ kind: .ident, value: 'middle', typ: 'int' })
	last := a.add_node(flat.Node{ kind: .ident, value: 'last', typ: 'Reader' })
	result := a.add_val(.ident, 'result')
	assignment := interface_box_scan_node(mut a, .assign, '3', '', [first, result, middle, last])
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Reader'] = true
	tc.register_synth_type(result, types.Type(types.MultiReturn{
		types: [types.Type(types.Struct{ name: 'First' }), types.Type(types.int_),
			types.Type(types.Struct{ name: 'Last' })]
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
	first := a.add_node(flat.Node{ kind: .ident, value: 'first', typ: 'Reader' })
	last := a.add_node(flat.Node{ kind: .ident, value: 'last', typ: 'Reader' })
	first_value := a.add_val(.ident, 'first_value')
	last_value := a.add_val(.ident, 'last_value')
	assignment := interface_box_scan_node(mut a, .assign, '2', '', [first, first_value, last,
		last_value])
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Reader'] = true
	tc.register_synth_type(first_value, types.Type(types.Struct{ name: 'First' }))
	tc.register_synth_type(last_value, types.Type(types.Struct{ name: 'Last' }))
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
		name:      'ReaderAlias'
		base_type: types.Type(types.Interface{ name: 'Reader' })
	})]
	tc.register_synth_type(value, types.Type(types.Struct{ name: 'Source' }))
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
	reader := types.Type(types.Interface{ name: 'Reader' })
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
