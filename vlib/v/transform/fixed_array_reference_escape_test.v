module transform

import v.flat
import v.types

fn add_fixed_array_reference_generic_struct(mut t Transformer, name string, field_text string) {
	field := t.a.add_node(flat.Node{ kind: .field_decl, value: 'value', typ: field_text })
	start := t.a.children.len
	t.a.children << field
	decl := t.a.add_node(flat.Node{ kind: .struct_decl, value: name, children_start: start, children_count: 1 })
	t.ensure_node_context_map_capacity()
	t.mark_node_context(decl, 'worker', '/tmp/worker.v')
	t.mark_node_context(field, 'worker', '/tmp/worker.v')
	t.tc.type_declaration_ids['worker.${name}'] = [int(decl)]
	t.tc.struct_generic_params['worker.${name}'] = ['T']
	// Deliberately mimic cached checker metadata resolving a source T to worker.T.
	t.tc.structs['worker.${name}'] = [types.StructField{ name: 'value', typ: types.Type(types.Struct{ name: 'worker.T' }) }]
}

fn test_fixed_array_reference_generic_fields_preserve_type_owners() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Inner'] = [types.StructField{ name: 'values', typ: types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 }) }]
	tc.struct_modules['Inner'] = 'main'
	tc.structs['worker.Inner'] = [types.StructField{ name: 'n', typ: types.Type(types.int_) }]
	tc.struct_modules['worker.Inner'] = 'worker'
	tc.structs['worker.T'] = [types.StructField{ name: 'n', typ: types.Type(types.int_) }]
	tc.struct_modules['worker.T'] = 'worker'
	tc.file_modules['/tmp/worker.v'] = 'worker'
	tc.file_imports[file_import_key('/tmp/worker.v', 'main')] = 'leaf'
	tc.structs['leaf.T'] = [types.StructField{ name: 'n', typ: types.Type(types.int_) }]
	tc.struct_modules['leaf.T'] = 'leaf'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	add_fixed_array_reference_generic_struct(mut t, 'Holder', 'T')
	add_fixed_array_reference_generic_struct(mut t, 'Decoy', 'worker.T')
	add_fixed_array_reference_generic_struct(mut t, 'ImportedDecoy', 'main.T')
	add_fixed_array_reference_generic_struct(mut t, 'Rows', '[]T')
	add_fixed_array_reference_generic_struct(mut t, 'Reference', '&T')
	add_fixed_array_reference_generic_struct(mut t, 'Nested', 'Holder[T]')
	t.cur_module = 'worker'
	t.cur_file = '/tmp/worker.v'
	tc.cur_module = 'worker'
	tc.cur_file = '/tmp/worker.v'
	for name, expected in {
		'worker.Holder[Inner]':                true
		'worker.Holder[worker.Inner]':         false
		'worker.Decoy[Inner]':                 false
		'worker.ImportedDecoy[Inner]':         false
		'worker.Rows[Inner]':                  false
		'worker.Reference[Inner]':             false
		'worker.Nested[Inner]':                true
		'worker.Holder[worker.Holder[Inner]]': true
	} {
		mut seen := map[string]bool{}
		assert t.escape_value_contains_fixed_array(types.Type(types.Struct{ name: name }), mut seen) == expected, name
	}
}

fn test_fixed_array_reference_generic_call_uses_conservative_escape_scan() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Holder'] = [types.StructField{ name: 'values', typ: types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 }) }]
	tc.fn_param_types['forward'] = [types.Type(types.Pointer{ base_type: types.Type(types.Unknown{ reason: 'generic placeholder `T`' }) })]
	tc.fn_ret_types['forward'] = types.Type(types.Array{ elem_type: types.Type(types.int_) })
	tc.cur_scope.insert('holder', types.Type(types.Struct{ name: 'Holder' }))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('holder', 'Holder')
	arg := t.make_ident('holder')
	tc.register_synth_type(arg, types.Type(types.Struct{ name: 'Holder' }))
	call := t.make_call_typed('forward', [arg], '[]int')
	t.set_resolved_call_entry(int(call), 'forward')
	t.fast_escape_precheck = true
	t.item_escape_scan_known = true
	t.item_escape_scan_needed = false
	t.mark_escaping_amp_ptrs([call])
	assert 'holder' in t.escaping_fixed_array_view_sources
}

fn test_fixed_array_reference_value_parameter_promotion_preserves_reference_abi() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	param := t.a.add_node(flat.Node{ kind: .param, value: 'values', typ: '[2]int' })
	start := t.a.children.len
	t.a.children << param
	fn_node := flat.Node{ kind: .fn_decl, value: 'forward', children_start: start, children_count: 1 }
	t.set_var_type('values', '[2]int')
	t.escaping_fixed_array_view_sources['values'] = true
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	replacements, entry := t.heap_fixed_array_view_params(fn_node, [fixed])
	assert replacements.len == 1
	assert entry.len > 0
	assert 'values' in t.heaped_amp_locals
	assert t.var_type('values') == '&[2]int'
	assert t.a.nodes[int(replacements[int(param)])].value != 'values'
	assert t.a.nodes[int(replacements[int(param)])].typ == '[2]int'
	mut b := flat.FlatAst.new()
	mut tc2 := types.TypeChecker.new(&b)
	mut t2 := new_transformer(mut b, &tc2, map[string]bool{})
	param2 := t2.a.add_node(flat.Node{ kind: .param, value: 'values', typ: '[2]int', is_mut: true })
	start2 := t2.a.children.len
	t2.a.children << param2
	fn2 := flat.Node{ kind: .fn_decl, value: 'forward', children_start: start2, children_count: 1 }
	t2.set_var_type('values', '[2]int')
	t2.escaping_fixed_array_view_sources['values'] = true
	replacements2, entry2 := t2.heap_fixed_array_view_params(fn2, [types.Type(types.Pointer{ base_type: fixed })])
	assert replacements2.len == 0
	assert entry2.len == 0
}

fn test_fixed_array_reference_stack_pointer_projections_keep_original_root() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	tc.structs['Holder'] = [types.StructField{ name: 'values', typ: fixed }]
	tc.trust_checked_expr_types = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('p', '&Holder')
	ptr := t.make_ident('p')
	deref := t.make_prefix(.mul, ptr)
	t.set_node_typ(int(deref), 'Holder')
	field := t.make_selector(deref, 'values', '[2]int')
	tc.register_synth_type(field, fixed)
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	t.mark_fixed_array_reference_argument_escape(field, array_ref, {
		'p': true
	}, {
		'p': ['holder']
	}, map[string]string{}, {
		'holder': true
		'p':      true
	})
	assert 'holder' in t.escaping_fixed_array_view_sources
	assert 'p' !in t.escaping_fixed_array_view_sources
	// Dynamic container slots are already separately allocated; their header is not the
	// inline owner of the selected fixed array.
	t.escaping_fixed_array_view_sources.clear()
	t.set_var_type('rows', '[]Holder')
	row := t.make_index(t.make_ident('rows'), t.make_int_literal(0), 'Holder')
	row_field := t.make_selector(row, 'values', '[2]int')
	tc.register_synth_type(row_field, fixed)
	t.mark_fixed_array_reference_argument_escape(row_field, array_ref, map[string]bool{}, map[string][]string{}, map[string]string{}, {
		'rows': true
	})
	assert t.escaping_fixed_array_view_sources.len == 0
}

fn test_fixed_array_reference_zero_argument_method_marks_receiver() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Holder'] = [types.StructField{ name: 'values', typ: types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 }) }]
	tc.fn_param_types['Holder.keep'] = [types.Type(types.Pointer{ base_type: types.Type(types.Struct{ name: 'Holder' }) })]
	tc.fn_ret_types['Holder.keep'] = types.Type(types.Array{ elem_type: types.Type(types.int_) })
	tc.cur_scope.insert('holder', types.Type(types.Struct{ name: 'Holder' }))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('holder', 'Holder')
	call := t.make_method_call(t.make_ident('holder'), 'keep', []flat.NodeId{})
	t.set_resolved_call_entry(int(call), 'Holder.keep')
	t.fast_escape_precheck = true
	t.item_escape_scan_known = true
	t.item_escape_scan_needed = false
	t.mark_escaping_amp_ptrs([call])
	assert 'holder' in t.escaping_fixed_array_view_sources
}
