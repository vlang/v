module transform

import v.flat
import v.types
import v.token

fn test_plain_multi_declaration_copies_promoted_fixed_values_with_their_value_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed_type := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('values', '&[2]int')
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
	assert copied.typ == '[2]int'
	assert t.var_type('copied') == '[2]int'
	rhs := t.a.child_node(&copied, 1)
	assert rhs.kind == .prefix && rhs.op == .mul
	assert t.heaped_amp_locals['values']
}

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

fn test_fixed_array_direct_address_dereference_marks_the_original_local() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 3 })
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('fixed', '[3]int')
	tc.cur_scope.insert('fixed', fixed)
	tc.type_aliases['FixedPtr'] = '&[3]int'
	tc.type_aliases['FixedPtrAlias'] = 'FixedPtr'
	for parenthesized in [false, true] {
		value := t.make_ident('fixed')
		tc.register_synth_type(value, fixed)
		addr := t.make_prefix(.amp, value)
		t.set_node_typ(int(addr), '&[3]int')
		pointer := if parenthesized { t.make_paren(addr) } else { addr }
		deref := t.make_prefix(.mul, pointer)
		t.set_node_typ(int(deref), '[3]int')
		tc.register_synth_type(deref, fixed)
		range := t.make_range_index(deref, t.make_int_literal(1), flat.empty_node, '[]int')
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
		t.set_node_typ(int(addr), '&[3]int')
		cast := t.make_cast(cast_type, t.make_paren(addr), cast_type)
		deref := t.make_prefix(.mul, cast)
		t.set_node_typ(int(deref), '[3]int')
		tc.register_synth_type(deref, fixed)
		range := t.make_range_index(deref, t.make_int_literal(1), flat.empty_node, '[]int')
		tc.register_synth_type(range, types.Type(types.Array{ elem_type: types.Type(types.int_) }))
		t.escaping_fixed_array_view_sources.clear()
		t.mark_fixed_array_reference_argument_escape(range, array_ref, map[string]bool{}, map[string][]string{}, map[string]string{}, {
			'fixed': true
		})
		assert 'fixed' in t.escaping_fixed_array_view_sources, cast_type
	}
	// A local variable holding a heap pointer is not the owner of its pointed-to storage.
	t.escaping_fixed_array_view_sources.clear()
	t.set_var_type('heap', '&[3]int')
	pointer := t.make_ident('heap')
	deref := t.make_prefix(.mul, pointer)
	t.set_node_typ(int(deref), '[3]int')
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
		decls := t.make_guard_value_decls('values', value, '[2]int')
		assert decls.len == 2
		assert t.var_type('values') == '&[2]int'
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
		t.set_var_type(name, '[2]int')
		tc.cur_scope.insert(name, fixed)
		arg := t.make_ident(name)
		tc.register_synth_type(arg, fixed)
		args << arg
		locals[name] = true
	}
	call_id := t.make_call_typed('retain', args, '&[]int')
	t.set_resolved_call_entry(int(call_id), 'retain')
	t.fast_escape_precheck = true
	t.item_escape_scan_known = true
	t.item_escape_scan_needed = false
	t.mark_escaping_amp_ptrs([call_id])
	for name in locals.keys() {
		assert name in t.escaping_fixed_array_view_sources, name
	}
	t.escaping_fixed_array_view_sources.clear()
	ref_alias := types.Type(types.Alias{ name: 'ArrayRef', base_type: array_ref })
	tc.fn_param_types['retain_alias'] = [types.Type(types.int_),
		types.Type(types.Array{ elem_type: ref_alias })]
	tc.fn_ret_types['retain_alias'] = array_ref
	tc.fn_variadic['retain_alias'] = true
	alias_call := t.make_call_typed('retain_alias', args, '&[]int')
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
	ordinary := t.make_call_typed('retain_array', args[..2], '&[]int')
	t.set_resolved_call_entry(int(ordinary), 'retain_array')
	t.mark_fixed_array_reference_argument_escapes(ordinary, t.a.nodes[int(ordinary)], map[string]bool{}, map[string][]string{}, map[string]string{}, locals)
	assert t.escaping_fixed_array_view_sources.len == 0
	t.set_var_type('array_fn', 'fn (int, []&[]int) &[]int')
	ordinary_value := t.make_call_typed('array_fn', args[..2], '&[]int')
	t.mark_fixed_array_reference_argument_escapes(ordinary_value, t.a.nodes[int(ordinary_value)], map[string]bool{}, map[string][]string{}, map[string]string{}, locals)
	assert t.escaping_fixed_array_view_sources.len == 0
	t.set_var_type('retain_fn', 'fn (int, []&[]int) &[]int')
	fn_value_call := t.make_call_typed('retain_fn', args, '&[]int')
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
	t.structs['Aligned'] = StructInfo{ name: 'Aligned', is_aligned: true, alignment: '64' }
	for typ in ['Aligned', '[2]Aligned', '[2][3]Aligned'] {
		call := t.make_memdup_call_for_type(t.make_ident('source'), typ)
		node := t.a.nodes[int(call)]
		assert t.a.child_node(&node, 0).value == 'v3_aligned_memdup'
		assert t.a.child_node(&node, 3).value == '64'
	}
}

fn test_fixed_array_argument_ordering_handles_variadic_tail_and_reference_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['Slice'] = '[]int'
	tc.type_aliases['SliceRef'] = '&[]int'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('fixed', '[2]int')
	t.set_var_type('dynamic', '&[]int')
	call_id := t.make_call_typed('retain', [t.make_int_literal(0), t.make_ident('dynamic'),
		t.make_ident('fixed')], '&[]int')
	call := t.a.nodes[int(call_id)]
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	params := [types.Type(types.int_), types.Type(types.Array{ elem_type: array_ref })]
	assert t.call_has_mut_fixed_array_args(call, params, 0, 1)
	assert !t.call_has_mut_fixed_array_args(call, params, 0, -1)
	for param in [
		types.Type(types.Pointer{ base_type: types.Type(types.Alias{ name: 'Slice', base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) }) }),
		types.Type(types.Alias{ name: 'SliceRef', base_type: array_ref }),
		types.Type(types.OptionType{ base_type: array_ref }),
		types.Type(types.ResultType{ base_type: array_ref }),
	] {
		aliased_call := t.make_call_typed('keep_alias', [t.make_ident('fixed')], '&[]int')
		assert t.call_has_mut_fixed_array_args(t.a.nodes[int(aliased_call)], [param], 0, -1)
	}
}

fn test_fixed_array_optional_reference_arguments_promote_successful_payload_sources() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	array_ref := types.Type(types.Pointer{ base_type: types.Type(types.Array{ elem_type: types.Type(types.int_) }) })
	ref_alias := types.Type(types.Alias{ name: 'SliceRef', base_type: array_ref })
	option_ref := types.Type(types.OptionType{ base_type: ref_alias })
	result_ref := types.Type(types.ResultType{ base_type: ref_alias })
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('fixed', '[2]int')
	tc.cur_scope.insert('fixed', fixed)
	value := t.make_ident('fixed')
	tc.register_synth_type(value, fixed)
	for param in [option_ref, result_ref,
		types.Type(types.Alias{ name: 'MaybeSliceRef', base_type: option_ref }),
		types.Type(types.Alias{ name: 'ResultSliceRef', base_type: result_ref })] {
		tc.fn_param_types['retain_optional'] = [param]
		tc.fn_ret_types['retain_optional'] = param
		call := t.make_call_typed('retain_optional', [value], '?&[]int')
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
		tail_call := t.make_call_typed('retain_optional_tail', [value, value], '?&[]int')
		t.set_resolved_call_entry(int(tail_call), 'retain_optional_tail')
		t.escaping_fixed_array_view_sources.clear()
		t.mark_escaping_amp_ptrs([tail_call])
		assert 'fixed' in t.escaping_fixed_array_view_sources
	}
	t.escaping_fixed_array_view_sources.clear()
	no_value := t.make_optional_none('?&[]int')
	tc.register_synth_type(no_value, option_ref)
	no_value_call := t.make_call_typed('retain_optional', [no_value], '?&[]int')
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
	t.set_var_type('fixed', '[3]int')
	tc.cur_scope.insert('fixed', fixed)
	for wrapper in ['?&[]int', '!&[]int'] {
		for explicit in [false, true] {
			value := t.make_ident('fixed')
			tc.register_synth_type(value, fixed)
			range := t.make_range_index(value, t.make_int_literal(1), flat.empty_node, '[]int')
			tc.register_synth_type(range, array)
			arg := if explicit { t.make_prefix(.amp, range) } else { range }
			assert t.fixed_array_reference_arg_needs_durable_header(arg, '[]int')
			wrapped := t.transform_call_arg_for_param(arg, wrapper)
			wrapped_node := t.a.nodes[int(wrapped)]
			assert wrapped_node.kind == .struct_init
			assert wrapped_node.typ == wrapper
			payload_field := t.a.child_node(&wrapped_node, 1)
			assert payload_field.value == 'value'
			header_call := t.a.child_node(payload_field, 0)
			assert header_call.kind == .call
			assert t.a.child_node(header_call, 0).value == 'v3_heap_array'
		}
	}
}

fn test_fixed_array_reference_sum_headers_do_not_own_inline_variant_storage() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	fixed := types.Type(types.ArrayFixed{ elem_type: types.Type(types.int_), len: 2 })
	tc.structs['FixedPayload'] = [types.StructField{ name: 'values', typ: fixed }]
	tc.structs['Wrapper'] = [types.StructField{ name: 'payload', typ: types.Type(types.SumType{ name: 'Payload' }) }]
	tc.sum_types['Payload'] = ['int', 'FixedPayload']
	tc.sum_types['Generic'] = ['int', 'T']
	tc.sum_generic_params['Generic'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for name in ['Payload', 'Generic[FixedPayload]'] {
		mut seen := map[string]bool{}
		assert !t.escape_value_contains_fixed_array(types.Type(types.SumType{ name: name }), mut seen)
	}
	mut seen := map[string]bool{}
	assert !t.escape_value_contains_fixed_array(types.Type(types.Alias{
		name:      'PayloadAlias'
		base_type: types.Type(types.SumType{ name: 'Payload' })
	}), mut seen)
	seen.clear()
	assert !t.escape_value_contains_fixed_array(types.Type(types.Struct{ name: 'Wrapper' }), mut seen)
	seen.clear()
	assert t.escape_value_contains_fixed_array(types.Type(types.Struct{ name: 'FixedPayload' }), mut seen)
}
