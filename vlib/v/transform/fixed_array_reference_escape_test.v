module transform

import v.flat
import v.types
import v.token

fn test_fixed_array_reference_iteration_escape_uses_checked_backing_types() {
	for source in ['foreign_const', 'stale_local', 'synthetic'] {
		for sliced in [false, true] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
			mut t := new_transformer(mut a, &tc, map[string]bool{})
			t.cur_module = 'main'
			t.set_var_type('out', flat.source_type_id('[]&int'))
			first := t.make_int_literal(1)
			second := t.make_int_literal(2)
			array_start := a.children.len
			a.children << [first, second]
			array := a.add_node(flat.Node{
				kind:           .array_literal
				typ:            flat.source_type_id('[2]int')
				children_start: array_start
				children_count: 2
			})
			decl := t.make_decl_assign('second', array)
			// The escape prepass runs before declaration lowering registers this local.
			if source == 'synthetic' {
				t.set_var_type('second', flat.source_type_id('[2]int'))
			} else {
				tc.const_types['time.second'] = types.Type(types.int_)
				t.collect_const_suffixes()
				if source == 'stale_local' {
					t.set_var_type('second', flat.source_type_int)
				}
			}
			// Source identifiers have checker types, without a lowered local annotation.
			backing := a.add_val(.ident, 'second')
			if source != 'synthetic' {
				tc.register_synth_type(backing, fixed)
			}
			container := if sliced {
				t.make_range_index(backing, t.make_int_literal(0), flat.empty_node, flat.source_type_id('[]int'))
			} else {
				backing
			}
			if sliced && source != 'synthetic' {
				tc.register_synth_type(container, types.Type(types.Array{ elem_type: types.Type(types.int_) }))
			}
			item := t.make_ident('item')
			append := t.make_infix(.left_shift, t.make_ident('out'), t.make_prefix(.amp, item))
			body := t.make_block([t.make_expr_stmt(append)])
			loop_start := a.children.len
			a.children << [flat.empty_node, item, container, body]
			loop := a.add_node(flat.Node{
				kind:           .for_in_stmt
				op:             .amp
				value:          '3'
				children_start: loop_start
				children_count: 4
			})
			t.mark_escaping_amp_ptrs([decl, loop])
			assert 'item' in t.escaping_amp_sources, '${source}, sliced: ${sliced}'
			assert 'second' in t.escaping_amp_sources, '${source}, sliced: ${sliced}'
			assert 'time.second' !in t.escaping_amp_sources
		}
	}
}

fn test_plain_multi_declaration_copies_promoted_fixed_values_with_their_value_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed_type := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('values', flat.source_type_id('&[2]int'))
	t.heaped_amp_locals['values'] = true
	t.pointer_value_lvalues['values'] = true
	t.pointer_value_rvalues['values'] = true
	value := t.make_ident('values')
	tc.register_synth_type(value, fixed_type)
	start := t.a.children.len
	t.a.children << [t.make_ident('zero'), t.make_int_literal(0), t.make_ident('copied'), value]
	decl := t.a.add_node(flat.Node{
		kind:           .decl_assign
		value:          '2'
		children_start: start
		children_count: 4
		pos:            token.new_span(1, 1, 10)
	})
	lowered := t.transform_decl_assign_stmt(decl, t.a.nodes[int(decl)])
	assert lowered.len == 2
	copied := t.a.nodes[int(lowered[1])]
	assert copied.typ_text() == '[2]int'
	assert flat.source_type_name(t.var_type('copied')) == '[2]int'
	rhs := t.a.child_node(&copied, 1)
	assert rhs.kind == .prefix && rhs.op == .mul
	assert t.heaped_amp_locals['values']
}

fn add_fixed_array_reference_generic_struct(mut t Transformer, name string, field_text string) {
	field := t.a.add_node(flat.Node{ kind: .field_decl, value: 'value', typ: flat.source_type_id(field_text) })
	start := t.a.children.len
	t.a.children << field
	decl := t.a.add_node(flat.Node{ kind: .struct_decl, value: name, children_start: start, children_count: 1 })
	t.ensure_node_context_map_capacity()
	t.mark_node_context(decl, 'worker', '/tmp/worker.v')
	t.mark_node_context(field, 'worker', '/tmp/worker.v')
	t.tc.type_declaration_ids['worker.${name}'] = [int(decl)]
	t.tc.struct_generic_params['worker.${name}'] = ['T']
	// Deliberately mimic cached checker metadata resolving a source T to worker.T.
	t.tc.structs['worker.${name}'] = [types.StructField{ name: 'value', typ: types.Type(types.Struct{ name_id: flat.source_type_id('worker.T') }) }]
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
		mut seen := map[u32]bool{}
		assert t.escape_value_contains_fixed_array(types.Type(types.Struct{ name_id: flat.source_type_id(name) }), mut seen) == expected, name
	}
}

fn test_fixed_array_reference_generic_call_uses_conservative_escape_scan() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Holder'] = [types.StructField{ name: 'values', typ: types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 }) }]
	tc.fn_param_types['forward'] = [types.Type(types.Pointer{ base_type: types.Type(types.Unknown{ reason: 'generic placeholder `T`' }) })]
	tc.fn_ret_types['forward'] = types.Type(types.Array{ elem_type: types.Type(types.int_) })
	tc.cur_scope.insert('holder', types.Type(types.Struct{ name_id: flat.source_type_id('Holder') }))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('holder', flat.source_type_id('Holder'))
	arg := t.make_ident('holder')
	tc.register_synth_type(arg, types.Type(types.Struct{ name_id: flat.source_type_id('Holder') }))
	call := t.make_call_typed('forward', [arg], flat.source_type_id('[]int'))
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
	param := t.a.add_node(flat.Node{ kind: .param, value: 'values', typ: flat.source_type_id('[2]int') })
	start := t.a.children.len
	t.a.children << param
	fn_node := flat.Node{ kind: .fn_decl, value: 'forward', children_start: start, children_count: 1 }
	t.set_var_type('values', flat.source_type_id('[2]int'))
	t.escaping_fixed_array_view_sources['values'] = true
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	replacements, entry := t.heap_fixed_array_view_params(fn_node, [fixed])
	assert replacements.len == 1
	assert entry.len > 0
	assert 'values' in t.heaped_amp_locals
	assert flat.source_type_name(t.var_type('values')) == '&[2]int'
	assert t.a.nodes[int(replacements[int(param)])].value != 'values'
	assert t.a.nodes[int(replacements[int(param)])].typ_text() == '[2]int'
	mut b := flat.FlatAst.new()
	mut tc2 := types.TypeChecker.new(&b)
	mut t2 := new_transformer(mut b, &tc2, map[string]bool{})
	param2 := t2.a.add_node(flat.Node{ kind: .param, value: 'values', typ: flat.source_type_id('[2]int'), is_mut: true })
	start2 := t2.a.children.len
	t2.a.children << param2
	fn2 := flat.Node{ kind: .fn_decl, value: 'forward', children_start: start2, children_count: 1 }
	t2.set_var_type('values', flat.source_type_id('[2]int'))
	t2.escaping_fixed_array_view_sources['values'] = true
	replacements2, entry2 := t2.heap_fixed_array_view_params(fn2, [types.Type(types.Pointer{ base_type: fixed })])
	assert replacements2.len == 0
	assert entry2.len == 0
}

fn test_fixed_array_reference_pointer_field_does_not_promote_its_containing_value() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 9 })
	coordinator := types.Type(types.Struct{ name_id: flat.source_type_id('Coordinator') })
	coordinator_ptr := types.Type(types.Pointer{ base_type: coordinator })
	tc.structs['Coordinator'] = [types.StructField{ name: 'reasons', typ: fixed }]
	tc.structs['Dispatch'] = [types.StructField{ name: 'coordinator', typ: coordinator_ptr }]
	tc.trust_checked_expr_types = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('dispatch', flat.source_type_id('Dispatch'))
	field := t.make_selector(t.make_ident('dispatch'), 'coordinator', flat.source_type_id('&Coordinator'))
	tc.register_synth_type(field, coordinator_ptr)
	t.mark_fixed_array_reference_argument_escape(field, coordinator_ptr, map[string]bool{}, map[string][]string{}, map[string]string{}, {
		'dispatch': true
	})
	assert t.escaping_fixed_array_view_sources.len == 0
	// A pointer explicitly taken to this frame's fixed storage still promotes
	// the original owner, rather than the pointer alias.
	t.set_var_type('p', flat.source_type_id('&Coordinator'))
	p := t.make_ident('p')
	tc.register_synth_type(p, coordinator_ptr)
	t.mark_fixed_array_reference_argument_escape(p, coordinator_ptr, {
		'p': true
	}, {
		'p': ['coordinator']
	}, map[string]string{}, {
		'coordinator': true
		'p':           true
	})
	assert 'coordinator' in t.escaping_fixed_array_view_sources
	assert 'p' !in t.escaping_fixed_array_view_sources
}

fn test_fixed_array_reference_pointer_address_roundtrip_keeps_its_original_owner() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 3 })
	pointer := types.Type(types.Pointer{ base_type: fixed })
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	tc.trust_checked_expr_types = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('p', flat.source_type_id('&[3]int'))
	p := t.make_ident('p')
	tc.register_synth_type(p, pointer)
	address := t.make_prefix(.amp, p)
	tc.register_synth_type(address, types.Type(types.Pointer{ base_type: pointer }))
	deref := t.make_prefix(.mul, t.make_paren(address))
	tc.register_synth_type(deref, pointer)
	t.mark_fixed_array_reference_argument_escape(deref, array_ref, {
		'p': true
	}, {
		'p': ['values']
	}, map[string]string{}, {
		'p':      true
		'values': true
	})
	assert 'values' in t.escaping_fixed_array_view_sources
	assert 'p' !in t.escaping_fixed_array_view_sources
}

fn test_fixed_array_reference_mutable_iteration_pointer_keeps_its_backing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	rows_type := types.Type(types.ArrayFixed{ elem_type: fixed, len: 2 })
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	tc.fn_param_types['retain'] = [array_ref]
	tc.fn_ret_types['retain'] = array_ref
	tc.trust_checked_expr_types = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('rows', flat.source_type_id('[2][2]int'))
	rows := t.make_ident('rows')
	tc.register_synth_type(rows, rows_type)
	row := t.make_ident('row')
	tc.register_synth_type(row, types.Type(types.Pointer{ base_type: fixed }))
	call := t.make_call_typed('retain', [row], flat.source_type_id('&[]int'))
	t.set_resolved_call_entry(int(call), 'retain')
	body := t.make_block([t.make_expr_stmt(call)])
	start := a.children.len
	a.children << [flat.empty_node, row, rows, body]
	loop := a.add_node(flat.Node{
		kind:           .for_in_stmt
		op:             .amp
		value:          '3'
		children_start: start
		children_count: 4
	})
	t.mark_escaping_amp_ptrs([loop])
	assert 'row' in t.escaping_fixed_array_view_sources
	assert 'rows' in t.escaping_fixed_array_view_sources
}

fn test_fixed_array_reference_stack_pointer_projections_keep_original_root() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	tc.structs['Holder'] = [types.StructField{ name: 'values', typ: fixed }]
	tc.trust_checked_expr_types = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('p', flat.source_type_id('&Holder'))
	ptr := t.make_ident('p')
	deref := t.make_prefix(.mul, ptr)
	t.set_node_typ(int(deref), flat.source_type_id('Holder'))
	field := t.make_selector(deref, 'values', flat.source_type_id('[2]int'))
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
	t.set_var_type('rows', flat.source_type_id('[]Holder'))
	row := t.make_index(t.make_ident('rows'), t.make_int_literal(0), flat.source_type_id('Holder'))
	row_field := t.make_selector(row, 'values', flat.source_type_id('[2]int'))
	tc.register_synth_type(row_field, fixed)
	t.mark_fixed_array_reference_argument_escape(row_field, array_ref, map[string]bool{}, map[string][]string{}, map[string]string{}, {
		'rows': true
	})
	assert t.escaping_fixed_array_view_sources.len == 0
}

fn test_fixed_array_direct_address_dereference_marks_the_original_local() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 3 })
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('fixed', flat.source_type_id('[3]int'))
	tc.cur_scope.insert('fixed', fixed)
	tc.type_aliases.set_id('FixedPtr', flat.source_type_id('&[3]int'))
	tc.type_aliases.set_id('FixedPtrAlias', flat.source_type_id('FixedPtr'))
	for parenthesized in [false, true] {
		value := t.make_ident('fixed')
		tc.register_synth_type(value, fixed)
		addr := t.make_prefix(.amp, value)
		t.set_node_typ(int(addr), flat.source_type_id('&[3]int'))
		pointer := if parenthesized { t.make_paren(addr) } else { addr }
		deref := t.make_prefix(.mul, pointer)
		t.set_node_typ(int(deref), flat.source_type_id('[3]int'))
		tc.register_synth_type(deref, fixed)
		range := t.make_range_index(deref, t.make_int_literal(1), flat.empty_node, flat.source_type_id('[]int'))
		tc.register_synth_type(range, types.Type(types.Array{ elem_type: types.Type(types.int_) }))
		t.escaping_fixed_array_view_sources.clear()
		t.mark_fixed_array_reference_argument_escape(range, array_ref, map[string]bool{}, map[string][]string{}, map[string]string{}, {
			'fixed': true
		})
		assert 'fixed' in t.escaping_fixed_array_view_sources
	}
	for cast_type in ['FixedPtr', 'FixedPtrAlias', '&[3]int'] {
		value := t.make_ident('fixed')
		tc.register_synth_type(value, fixed)
		addr := t.make_prefix(.amp, value)
		t.set_node_typ(int(addr), flat.source_type_id('&[3]int'))
		cast := t.make_cast(flat.source_type_id(cast_type), t.make_paren(addr), flat.source_type_id(cast_type))
		deref := t.make_prefix(.mul, cast)
		t.set_node_typ(int(deref), flat.source_type_id('[3]int'))
		tc.register_synth_type(deref, fixed)
		range := t.make_range_index(deref, t.make_int_literal(1), flat.empty_node, flat.source_type_id('[]int'))
		tc.register_synth_type(range, types.Type(types.Array{ elem_type: types.Type(types.int_) }))
		t.escaping_fixed_array_view_sources.clear()
		t.mark_fixed_array_reference_argument_escape(range, array_ref, map[string]bool{}, map[string][]string{}, map[string]string{}, {
			'fixed': true
		})
		assert 'fixed' in t.escaping_fixed_array_view_sources, cast_type
	}
	// A local variable holding a heap pointer is not the owner of its pointed-to storage.
	t.escaping_fixed_array_view_sources.clear()
	t.set_var_type('heap', flat.source_type_id('&[3]int'))
	pointer := t.make_ident('heap')
	deref := t.make_prefix(.mul, pointer)
	t.set_node_typ(int(deref), flat.source_type_id('[3]int'))
	tc.register_synth_type(deref, fixed)
	t.mark_fixed_array_reference_argument_escape(deref, array_ref, map[string]bool{}, map[string][]string{}, map[string]string{}, {
		'heap': true
	})
	assert t.escaping_fixed_array_view_sources.len == 0
}

fn test_generated_fixed_guard_bindings_receive_durable_storage() {
	for source in ['view', 'address', 'capture'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		if source == 'view' {
			t.escaping_fixed_array_view_sources['values'] = true
		} else if source == 'address' {
			t.escaping_amp_sources['values'] = true
		} else {
			t.mut_fixed_array_capture_sources['values'] = true
		}
		value := t.make_ident('payload')
		decls := t.make_guard_value_decls('values', value, flat.source_type_id('[2]int'))
		assert decls.len == 2
		assert flat.source_type_name(t.var_type('values')) == '&[2]int'
		assert t.heaped_amp_locals['values']
		assert t.pointer_value_lvalues['values']
		assert t.pointer_value_rvalues['values']
		assert t.a.child_node(&t.a.nodes[int(decls[0])], 1).kind == .ident
	}
}

fn test_fixed_array_reference_zero_argument_method_marks_receiver() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Holder'] = [types.StructField{ name: 'values', typ: types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 }) }]
	tc.fn_param_types['Holder.keep'] = [types.Type(types.Pointer{ base_type: types.Type(types.Struct{ name_id: flat.source_type_id('Holder') }) })]
	tc.fn_ret_types['Holder.keep'] = types.Type(types.Array{ elem_type: types.Type(types.int_) })
	tc.cur_scope.insert('holder', types.Type(types.Struct{ name_id: flat.source_type_id('Holder') }))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('holder', flat.source_type_id('Holder'))
	call := t.make_method_call(t.make_ident('holder'), 'keep', []flat.NodeId{})
	t.set_resolved_call_entry(int(call), 'Holder.keep')
	t.fast_escape_precheck = true
	t.item_escape_scan_known = true
	t.item_escape_scan_needed = false
	t.mark_escaping_amp_ptrs([call])
	assert 'holder' in t.escaping_fixed_array_view_sources
}

fn test_fixed_array_reference_variadic_arguments_promote_each_tail_source() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	tc.fn_param_types['retain'] = [types.Type(types.int_),
		types.Type(types.Array{ elem_type: array_ref })]
	tc.fn_ret_types['retain'] = array_ref
	tc.fn_variadic['retain'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	mut args := [t.make_int_literal(2)]
	mut locals := map[string]bool{}
	for name in ['first', 'middle', 'last'] {
		t.set_var_type(name, flat.source_type_id('[2]int'))
		tc.cur_scope.insert(name, fixed)
		arg := t.make_ident(name)
		tc.register_synth_type(arg, fixed)
		args << arg
		locals[name] = true
	}
	call_id := t.make_call_typed('retain', args, flat.source_type_id('&[]int'))
	t.set_resolved_call_entry(int(call_id), 'retain')
	t.fast_escape_precheck = true
	t.item_escape_scan_known = true
	t.item_escape_scan_needed = false
	t.mark_escaping_amp_ptrs([call_id])
	for name in locals.keys() {
		assert name in t.escaping_fixed_array_view_sources, name
	}
	t.escaping_fixed_array_view_sources.clear()
	ref_alias := types.Type(types.Alias{ name_id: flat.source_type_id('ArrayRef'), base_type: array_ref })
	tc.fn_param_types['retain_alias'] = [types.Type(types.int_),
		types.Type(types.Array{ elem_type: ref_alias })]
	tc.fn_ret_types['retain_alias'] = array_ref
	tc.fn_variadic['retain_alias'] = true
	alias_call := t.make_call_typed('retain_alias', args, flat.source_type_id('&[]int'))
	t.set_resolved_call_entry(int(alias_call), 'retain_alias')
	t.mark_fixed_array_reference_argument_escapes(alias_call, t.a.nodes[int(alias_call)], map[string]bool{}, map[string][]string{}, map[string]string{}, locals)
	for name in locals.keys() {
		assert name in t.escaping_fixed_array_view_sources, name
	}
	// An ordinary array parameter contains references that were formed earlier;
	// it must not reinterpret its argument as a variadic reference element.
	t.escaping_fixed_array_view_sources.clear()
	tc.fn_param_types['retain_array'] = tc.fn_param_types['retain'].clone()
	tc.fn_ret_types['retain_array'] = array_ref
	ordinary := t.make_call_typed('retain_array', args[..2], flat.source_type_id('&[]int'))
	t.set_resolved_call_entry(int(ordinary), 'retain_array')
	t.mark_fixed_array_reference_argument_escapes(ordinary, t.a.nodes[int(ordinary)], map[string]bool{}, map[string][]string{}, map[string]string{}, locals)
	assert t.escaping_fixed_array_view_sources.len == 0
	t.set_var_type('array_fn', flat.source_type_id('fn (int, []&[]int) &[]int'))
	ordinary_value := t.make_call_typed('array_fn', args[..2], flat.source_type_id('&[]int'))
	t.mark_fixed_array_reference_argument_escapes(ordinary_value, t.a.nodes[int(ordinary_value)], map[string]bool{}, map[string][]string{}, map[string]string{}, locals)
	assert t.escaping_fixed_array_view_sources.len == 0
	t.set_var_type('retain_fn', flat.source_type_id('fn (int, []&[]int) &[]int'))
	fn_value_call := t.make_call_typed('retain_fn', args, flat.source_type_id('&[]int'))
	t.a.nodes[int(fn_value_call)].flags |= flat.node_flag_variadic_call
	assert (flat.clone_node_flags(&t.a.nodes[int(fn_value_call)], false) & flat.node_flag_variadic_call) != 0
	t.mark_fixed_array_reference_argument_escapes(fn_value_call, t.a.nodes[int(fn_value_call)], map[string]bool{}, map[string][]string{}, map[string]string{}, locals)
	for name in locals.keys() {
		assert name in t.escaping_fixed_array_view_sources, name
	}
}

fn test_promoted_fixed_array_storage_preserves_nested_element_alignment() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := a.add_node(flat.Node{ kind: .struct_decl, value: 'Aligned' })
	tc.structs['Aligned'] = []types.StructField{}
	tc.collect(&a)
	tc.declaration_attributes[int(decl)] = ['aligned: 64']
	for typ in ['Aligned', '[2]Aligned', '[2][3]Aligned'] {
		call := t.make_memdup_call_for_type(t.make_ident('source'), flat.source_type_id(typ))
		node := t.a.nodes[int(call)]
		assert t.a.child_node(&node, 0).value == 'v3_aligned_memdup'
		alignment := t.a.child_node(&node, 3)
		assert alignment.kind == .call
		assert t.a.child_node(alignment, 0).value == '__alignof__'
	}
}

fn test_fresh_aligned_fixed_array_addresses_promote_storage_but_existing_values_keep_identity() {
	for entry in ['prefix', 'builtin_addr'] {
		for type_name in ['[2]Choice', '[2][3]Choice', 'ChoicePair', '[2]int'] {
			mut a := flat.FlatAst.new()
			decl := a.add_node(flat.Node{ kind: .struct_decl, value: 'Aligned' })
			mut tc := types.TypeChecker.new(&a)
			tc.first_type_declaration_ids['Aligned'] = int(decl)
			tc.declaration_attributes[int(decl)] = ['aligned: 64']
			tc.structs['Aligned'] = []types.StructField{}
			tc.sum_types.set('Choice', ['Aligned', 'int'])
			tc.type_aliases.set_id('ChoicePair', flat.source_type_id('[2]Choice'))
			mut t := new_transformer(mut a, &tc, map[string]bool{})
			type_id := flat.source_type_id(type_name)
			literal := a.add_node(flat.Node{ kind: .array_literal, typ: type_id })
			mut result := flat.empty_node
			if entry == 'prefix' {
				address := t.make_prefix(.amp, literal)
				result = t.transform_prefix_expr(address, t.a.nodes[int(address)])
			} else {
				call := t.make_call_typed('__addr', [literal], flat.source_type_id('&${type_name}'))
				result = t.transform_builtin_addr_call(t.a.nodes[int(call)]) or { panic('missing addr intrinsic') }
			}
			result_node := t.a.nodes[int(result)]
			if type_name == '[2]int' {
				// Ordinary alignment keeps the existing temporary-address lowering.
				assert result_node.kind == .prefix && result_node.op == .amp
				assert t.a.child_node(&result_node, 0).kind == .ident
			} else {
				assert result_node.kind == .cast_expr, '${entry}: ${type_name}'
				assert result_node.value == '&${type_name}'
				allocation := t.a.child_node(&result_node, 0)
				assert allocation.kind == .call
				assert t.a.child_node(allocation, 0).value == 'v3_aligned_memdup'
				alignment := t.a.child_node(allocation, 3)
				assert alignment.kind == .call
				assert t.a.child_node(alignment, 0).value == '__alignof__'
			}
			// Addressing existing storage must preserve the original reference.
			t.set_var_type('existing', type_id)
			existing := t.make_ident('existing')
			address := t.make_prefix(.amp, existing)
			borrowed := if entry == 'prefix' {
				t.transform_prefix_expr(address, t.a.nodes[int(address)])
			} else {
				t.runtime_addr(existing, type_id)
			}
			borrowed_node := t.a.nodes[int(borrowed)]
			assert borrowed_node.kind == .prefix && borrowed_node.op == .amp
			assert t.a.child_node(&borrowed_node, 0).value == 'existing'
		}
	}
}

fn test_fixed_array_argument_ordering_handles_variadic_tail_and_reference_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Slice', flat.source_type_id('[]int'))
	tc.type_aliases.set_id('SliceRef', flat.source_type_id('&[]int'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('fixed', flat.source_type_id('[2]int'))
	t.set_var_type('dynamic', flat.source_type_id('&[]int'))
	call_id := t.make_call_typed('retain', [t.make_int_literal(0), t.make_ident('dynamic'),
		t.make_ident('fixed')], flat.source_type_id('&[]int'))
	call := t.a.nodes[int(call_id)]
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	params := [types.Type(types.int_), types.Type(types.Array{ elem_type: array_ref })]
	assert t.call_has_mut_fixed_array_args(call, params, 0, 1)
	assert !t.call_has_mut_fixed_array_args(call, params, 0, -1)
	for param in [
		types.Type(types.Pointer{ base_type: types.Type(types.Alias{ name_id: flat.source_type_id('Slice'), base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) }) }),
		types.Type(types.Alias{ name_id: flat.source_type_id('SliceRef'), base_type: array_ref }),
		types.Type(types.OptionType{ base_type: array_ref }),
		types.Type(types.ResultType{ base_type: array_ref }),
	] {
		aliased_call := t.make_call_typed('keep_alias', [t.make_ident('fixed')], flat.source_type_id('&[]int'))
		assert t.call_has_mut_fixed_array_args(t.a.nodes[int(aliased_call)], [param], 0, -1)
	}
}

fn test_fixed_array_optional_reference_arguments_promote_successful_payload_sources() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	ref_alias := types.Type(types.Alias{ name_id: flat.source_type_id('SliceRef'), base_type: array_ref })
	option_ref := types.Type(types.OptionType{ base_type: ref_alias })
	result_ref := types.Type(types.ResultType{ base_type: ref_alias })
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('fixed', flat.source_type_id('[2]int'))
	tc.cur_scope.insert('fixed', fixed)
	value := t.make_ident('fixed')
	tc.register_synth_type(value, fixed)
	for param in [option_ref, result_ref,
		types.Type(types.Alias{ name_id: flat.source_type_id('MaybeSliceRef'), base_type: option_ref }),
		types.Type(types.Alias{ name_id: flat.source_type_id('ResultSliceRef'), base_type: result_ref })] {
		tc.fn_param_types['retain_optional'] = [param]
		tc.fn_ret_types['retain_optional'] = param
		call := t.make_call_typed('retain_optional', [value], flat.source_type_id('?&[]int'))
		t.set_resolved_call_entry(int(call), 'retain_optional')
		t.fast_escape_precheck = true
		t.item_escape_scan_known = true
		t.item_escape_scan_needed = false
		t.escaping_fixed_array_view_sources.clear()
		t.mark_escaping_amp_ptrs([call])
		assert 'fixed' in t.escaping_fixed_array_view_sources
		tc.fn_param_types['retain_optional_tail'] = [types.Type(types.Array{ elem_type: param })]
		tc.fn_ret_types['retain_optional_tail'] = param
		tc.fn_variadic['retain_optional_tail'] = true
		tail_call := t.make_call_typed('retain_optional_tail', [value, value], flat.source_type_id('?&[]int'))
		t.set_resolved_call_entry(int(tail_call), 'retain_optional_tail')
		t.escaping_fixed_array_view_sources.clear()
		t.mark_escaping_amp_ptrs([tail_call])
		assert 'fixed' in t.escaping_fixed_array_view_sources
	}
	t.escaping_fixed_array_view_sources.clear()
	no_value := t.make_optional_none(flat.source_type_id('?&[]int'))
	tc.register_synth_type(no_value, option_ref)
	no_value_call := t.make_call_typed('retain_optional', [no_value], flat.source_type_id('?&[]int'))
	t.set_resolved_call_entry(int(no_value_call), 'retain_optional')
	t.mark_escaping_amp_ptrs([no_value_call])
	assert t.escaping_fixed_array_view_sources.len == 0
}

fn test_fixed_array_range_reference_headers_are_allocated_before_wrapping() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 3 })
	array := types.Type(types.Array{ elem_type: types.Type(types.int_) })
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('fixed', flat.source_type_id('[3]int'))
	tc.cur_scope.insert('fixed', fixed)
	for wrapper in ['?&[]int', '!&[]int'] {
		for explicit in [false, true] {
			value := t.make_ident('fixed')
			tc.register_synth_type(value, fixed)
			range := t.make_range_index(value, t.make_int_literal(1), flat.empty_node, flat.source_type_id('[]int'))
			tc.register_synth_type(range, array)
			arg := if explicit { t.make_prefix(.amp, range) } else { range }
			assert t.fixed_array_reference_arg_needs_durable_header(arg, flat.source_type_id('[]int'))
			wrapped := t.transform_call_arg_for_param(arg, flat.source_type_id(wrapper))
			wrapped_node := t.a.nodes[int(wrapped)]
			assert wrapped_node.kind == .struct_init
			assert wrapped_node.typ_text() == wrapper
			payload_field := t.a.child_node(&wrapped_node, 1)
			assert payload_field.value == 'value'
			header_call := t.a.child_node(payload_field, 0)
			assert header_call.kind == .call
			assert t.a.child_node(header_call, 0).value == 'v3_heap_array'
		}
	}
}

fn test_fixed_array_reference_sums_own_inline_variant_storage() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	tc.structs['FixedPayload'] = [types.StructField{ name: 'values', typ: fixed }]
	tc.structs['Wrapper'] = [types.StructField{ name: 'payload', typ: types.Type(types.SumType{ name_id: flat.source_type_id('Payload') }) }]
	tc.sum_types.set('Payload', ['int', 'FixedPayload'])
	tc.sum_types.set('Generic', ['int', 'T'])
	tc.sum_generic_params['Generic'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for name in ['Payload', 'Generic[FixedPayload]'] {
		mut seen := map[u32]bool{}
		assert t.escape_value_contains_fixed_array(types.Type(types.SumType{ name_id: flat.source_type_id(name) }), mut seen)
	}
	mut seen := map[u32]bool{}
	assert t.escape_value_contains_fixed_array(types.Type(types.Alias{
		name_id:   flat.source_type_id('PayloadAlias')
		base_type: types.Type(types.SumType{ name_id: flat.source_type_id('Payload') })
	}), mut seen)
	seen.clear()
	assert t.escape_value_contains_fixed_array(types.Type(types.Struct{ name_id: flat.source_type_id('Wrapper') }), mut seen)
	seen.clear()
	assert t.escape_value_contains_fixed_array(types.Type(types.Struct{ name_id: flat.source_type_id('FixedPayload') }), mut seen)
}
