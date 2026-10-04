module transform

import v.flat
import v.gen.c as cgen
import v.types

fn escape_prescreen_node(mut a flat.FlatAst, node flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		...node
		children_start: start
		children_count: children.len
	})
}

fn ordinary_escape_fixture_c(mode string, prescreen bool) string {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	mut tc := types.TypeChecker.new(&a)
	tc.fn_param_types['compute'] = [types.Type(types.int_)]
	tc.fn_ret_types['compute'] = types.Type(types.int_)
	tc.fn_param_types['identity'] = [types.Type(types.int_)]
	tc.fn_ret_types['identity'] = types.Type(types.int_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.ordinary_escape_precheck = prescreen
	t.cur_module = 'main'
	param := a.add_node(flat.Node{ kind: .param, value: 'input', typ: 'int' })
	value := t.make_ident('input')
	tc.register_synth_type(value, types.Type(types.int_))
	decl := t.make_decl_assign_typed('value', value, 'int')
	mut stmts := [decl]
	if mode == 'call' {
		call := t.make_call_typed('identity', [t.make_ident('value')], 'int')
		stmts << t.make_expr_stmt(call)
	} else if mode == 'aggregate' {
		array := escape_prescreen_node(mut a, flat.Node{ kind: .array_literal, typ: '[]int' }, [
			t.make_ident('value'),
			t.make_int_literal(2),
		])
		stmts << t.make_decl_assign_typed('values', array, '[]int')
	} else if mode == 'branch' {
		cond := a.add_val(.bool_literal, 'true')
		then_block := t.make_block([t.make_return(t.make_int_literal(3), 'int')])
		else_block := t.make_block([t.make_return(t.make_int_literal(4), 'int')])
		stmts << escape_prescreen_node(mut a, flat.Node{ kind: .if_expr }, [cond, then_block,
			else_block])
	}
	stmts << t.make_return(t.make_ident('value'), 'int')
	body := t.make_block(stmts)
	fn_id := escape_prescreen_node(mut a, flat.Node{ kind: .fn_decl, value: 'compute', typ: 'int' }, [
		param,
		body,
	])
	assert t.ordinary_escape_scan_can_be_skipped([body]), mode
	// Both paths must clear state from the previous function, and the prescreen
	// itself must leave source annotations and children untouched.
	nodes_before := a.nodes.clone()
	children_before := a.children.clone()
	t.escaping_amp_sources['previous'] = true
	t.mark_escaping_amp_ptrs([body])
	assert t.escaping_amp_ptrs.len == 0 && t.escaping_amp_sources.len == 0
	assert t.escaping_fixed_array_view_sources.len == 0
	assert t.escaping_interface_box_locals.len == 0
	assert a.nodes == nodes_before && a.children == children_before
	t.transform_fn_body(int(fn_id))
	mut gen := cgen.FlatGen.new()
	source := gen.gen_with_used(&a, {
		'compute':  true
		'identity': true
	}, &tc)
	assert source.contains('compute('), mode
	assert source.contains('return value;'), mode
	if mode == 'call' {
		assert source.contains('identity(value)'), mode
	} else if mode == 'aggregate' {
		assert source.contains('values'), mode
	} else if mode == 'branch' {
		assert source.contains('return 3;') && source.contains('return 4;'), mode
	}
	return source
}

fn test_ordinary_escape_prescreen_matches_full_analysis_and_emitted_c() {
	for mode in ['scalar', 'call', 'aggregate', 'branch'] {
		assert ordinary_escape_fixture_c(mode, true) == ordinary_escape_fixture_c(mode, false), mode
	}
}

fn test_ordinary_escape_prescreen_rejects_every_address_source_category() {
	for mode in ['address', 'selector', 'reference_loop', 'literal', 'lambda', 'spawn',
		'unresolved_call', 'missing_signature', 'pointer_param', 'wrapped_pointer',
		'variadic_alias_pointer', 'unknown_param', 'generic_param'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'
		t.cur_fn_ret_type = 'int'
		value := t.make_ident('value')
		mut source := value
		match mode {
			'address' {
				t.set_var_type('value', 'int')
				t.cur_fn_ret_type = '&int'
				source = t.make_prefix(.amp, value)
				tc.register_synth_type(source, types.Type(types.Pointer{ base_type: types.Type(types.int_) }))
			}
			'selector' {
				// Includes standalone bound methods, even without an explicit &.
				source = t.make_selector(value, 'read', 'fn () int')
			}
			'reference_loop' {
				container := t.make_ident('items')
				a.nodes[int(container)].typ = '[2]int'
				body := t.make_block([]flat.NodeId{})
				source = escape_prescreen_node(mut a, flat.Node{ kind: .for_in_stmt, value: '3', op: .amp }, [
					value,
					flat.empty_node,
					container,
					body,
				])
			}
			'literal', 'lambda' {
				body := t.make_block([]flat.NodeId{})
				source = escape_prescreen_node(mut a, flat.Node{
					kind: if mode == 'literal' {
						flat.NodeKind.fn_literal
					} else {
						flat.NodeKind.lambda_expr
					}
				}, [body])
			}
			'spawn' {
				call := t.make_call_typed('retain', [value], 'int')
				source = escape_prescreen_node(mut a, flat.Node{ kind: .spawn_expr }, [call])
			}
			else {
				pointer := types.Type(types.Pointer{ base_type: types.Type(types.int_) })
				param := match mode {
					'pointer_param' { pointer }
					'wrapped_pointer' {
						types.Type(types.OptionType{ base_type: types.Type(types.Alias{ name: 'Ref', base_type: pointer }) })
					}
					'variadic_alias_pointer' {
						types.Type(types.Array{ elem_type: types.Type(types.Alias{ name: 'Ref', base_type: pointer }) })
					}
					'unknown_param' { types.Type(types.Unknown{ reason: 'unresolved signature' }) }
					else { types.Type(types.int_) }
				}
				if mode != 'missing_signature' {
					tc.fn_param_types['retain'] = [param]
					tc.fn_ret_types['retain'] = types.Type(types.int_)
				}
				if mode == 'variadic_alias_pointer' {
					tc.fn_variadic['retain'] = true
				}
				if mode == 'generic_param' {
					// A declared placeholder can collide with a scalar nominal type,
					// while inference specializes the full walk to a reference type.
					tc.fn_generic_params['retain'] = ['T']
				}
				if mode == 'unresolved_call' {
					source = escape_prescreen_node(mut a, flat.Node{ kind: .call }, [
						t.make_ident('retain'),
						value,
					])
				} else {
					source = t.make_call_typed('retain', [value], 'int')
				}
			}
		}
		body := if mode == 'address' {
			t.make_block([t.make_return(source, '&int')])
		} else {
			t.make_block([t.make_expr_stmt(source)])
		}
		assert !t.ordinary_escape_scan_can_be_skipped([body]), mode
		t.ordinary_escape_precheck = false
		t.mark_escaping_amp_ptrs([body])
		expected_sources := t.escaping_amp_sources.clone()
		expected_ptrs := t.escaping_amp_ptrs.clone()
		expected_fixed := t.escaping_fixed_array_view_sources.clone()
		expected_boxes := t.escaping_interface_box_locals.clone()
		if mode == 'address' {
			assert expected_sources['value'], mode
		}
		t.ordinary_escape_precheck = true
		t.mark_escaping_amp_ptrs([body])
		assert t.escaping_amp_sources == expected_sources, mode
		assert t.escaping_amp_ptrs == expected_ptrs, mode
		assert t.escaping_fixed_array_view_sources == expected_fixed, mode
		assert t.escaping_interface_box_locals == expected_boxes, mode
	}
}

fn test_ordinary_escape_prescreen_rejects_inferred_generic_reference_signature() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	mut tc := types.TypeChecker.new(&a)
	// The source placeholder T resolves to a scalar alias in the declaration
	// parameter view, but the inferred call parameter is a fixed-array pointer.
	tc.file_scope.insert('T', types.Type(types.Alias{ name: 'T', base_type: types.Type(types.int_) }))
	tc.fn_param_types['retain'] = [types.Type(types.int_)]
	tc.fn_ret_types['retain'] = types.Type(types.int_)
	param := a.add_node(flat.Node{ kind: .param, value: 'arg', typ: 'T' })
	fn_body := escape_prescreen_node(mut a, flat.Node{ kind: .block }, []flat.NodeId{})
	_ = escape_prescreen_node(mut a, flat.Node{
		kind:    .fn_decl
		value:   'retain'
		typ:     'int'
		payload: flat.node_payload(['T'])
	}, [param, fn_body])
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.set_var_type('value', '&[2]int')
	value := t.make_ident('value')
	tc.register_synth_type(value, tc.parse_type('&[2]int'))
	call := t.make_call_typed('retain', [value], 'int')
	call_node := a.nodes[int(call)]
	declared := t.call_param_types_for_node('retain', call_node)
	assert declared.len == 1
	assert !ordinary_escape_param_may_borrow_local(declared[0], 0)
	concrete := t.concrete_generic_call_param_types(call, call_node) or { panic('missing inferred signature') }
	assert concrete.len == 1 && concrete[0] is types.Pointer
	body := t.make_block([t.make_expr_stmt(call)])
	assert !t.ordinary_escape_scan_can_be_skipped([body])
	t.mark_escaping_amp_ptrs([body])
	assert t.escaping_fixed_array_view_sources['value']
	expected := t.escaping_fixed_array_view_sources.clone()
	t.ordinary_escape_precheck = true
	t.mark_escaping_amp_ptrs([body])
	assert t.escaping_fixed_array_view_sources == expected
}

fn test_ordinary_escape_proof_clears_stale_closure_cleanup_state() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.ordinary_escape_precheck = true
	body := t.make_block([t.make_expr_stmt(t.make_int_literal(1))])
	proof := t.mark_escaping_amp_ptrs([body])
	assert proof
	t.local_closure_cleanup_decls[81] = 'old_decl'
	t.local_closure_cleanup_values[82] = 'old_value'
	t.local_closure_cleanup_assigns[83] = 'old_assign'
	t.local_closure_field_cleanups[84] = true
	t.mark_local_closure_cleanup_decls_with_proof([body], proof)
	assert t.local_closure_cleanup_decls.len == 0
	assert t.local_closure_cleanup_values.len == 0
	assert t.local_closure_cleanup_assigns.len == 0
	assert t.local_closure_field_cleanups.len == 0
	// The existing self-host precheck has a different proof and must never
	// authorize skipping ordinary closure cleanup.
	t.ordinary_escape_precheck = false
	t.fast_escape_precheck = true
	assert !t.mark_escaping_amp_ptrs([body])
}

fn test_ordinary_escape_proof_keeps_closure_cleanup_fallback_metadata() {
	for mode in ['literal', 'selector', 'wrapped_selector', 'branch_literal'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.fn_param_types['Thing.read'] = [types.Type(types.Struct{ name: 'Thing' })]
		tc.fn_ret_types['Thing.read'] = types.Type(types.int_)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'
		value := t.make_ident('value')
		t.set_var_type('value', 'Thing')
		tc.register_synth_type(value, types.Type(types.Struct{ name: 'Thing' }))
		mut creator := flat.empty_node
		if mode in ['selector', 'wrapped_selector'] {
			creator = t.make_selector(value, 'read', 'fn () int')
			if mode == 'wrapped_selector' {
				creator = escape_prescreen_node(mut a, flat.Node{ kind: .paren }, [creator])
			}
		} else {
			literal_body := t.make_block([]flat.NodeId{})
			creator = escape_prescreen_node(mut a, flat.Node{ kind: .fn_literal, typ: 'fn () int' }, [
				value,
				literal_body,
			])
			if mode == 'branch_literal' {
				cond := a.add_val(.bool_literal, 'true')
				then_body := t.make_block([creator])
				else_body := t.make_block([creator])
				creator = escape_prescreen_node(mut a, flat.Node{ kind: .if_expr }, [
					cond,
					then_body,
					else_body,
				])
			}
		}
		decl := t.make_decl_assign_typed('callback', creator, 'fn () int')
		body := t.make_block([decl])
		assert !t.mark_escaping_amp_ptrs([body]), mode
		// Direct calls retain the full walk, with a real nonescaping creator.
		t.mark_local_closure_cleanup_decls([body])
		assert t.local_closure_cleanup_decls[int(decl)] == 'callback', mode
		expected_decls := t.local_closure_cleanup_decls.clone()
		expected_values := t.local_closure_cleanup_values.clone()
		expected_assigns := t.local_closure_cleanup_assigns.clone()
		expected_fields := t.local_closure_field_cleanups.clone()
		// Analyze an eligible body in between to expose any stale proof leaking
		// into the following lifted/method/literal body's fallback analysis.
		t.ordinary_escape_precheck = true
		scalar_body := t.make_block([t.make_expr_stmt(t.make_int_literal(2))])
		scalar_proof := t.mark_escaping_amp_ptrs([scalar_body])
		assert scalar_proof
		t.mark_local_closure_cleanup_decls_with_proof([scalar_body], scalar_proof)
		proof := t.mark_escaping_amp_ptrs([body])
		assert !proof, mode
		t.mark_local_closure_cleanup_decls_with_proof([body], proof)
		assert t.local_closure_cleanup_decls == expected_decls, mode
		assert t.local_closure_cleanup_values == expected_values, mode
		assert t.local_closure_cleanup_assigns == expected_assigns, mode
		assert t.local_closure_field_cleanups == expected_fields, mode
	}
}
