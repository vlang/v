module transform

import v3.flat
import v3.types

fn test_cloned_worker_merge_replays_relocated_children_and_body_roots() {
	// 'plain' copies and relocates inside the serial merge, 'relocated' relocates
	// the worker region in parallel first, and 'absorbed' also writes it straight
	// into the master's pre-grown arrays. All three must merge identically.
	for mode in ['plain', 'relocated', 'absorbed'] {
		mut a := flat.FlatAst.new()
		leaf := a.add_node(flat.Node{ kind: .int_literal, value: '1' })
		base_slot := a.children.len
		a.add_child(leaf)
		fn_id := a.add_node(flat.Node{
			kind: .fn_decl
			value: 'helper'
			children_start: base_slot
			children_count: 1
		})
		mut tc := types.TypeChecker.new(&a)
		mut master := new_transformer(mut a, &tc, map[string]bool{})
		base_nodes := a.nodes.len
		base_children := a.children.len
		mut worker_ast := master.clone_ast_base(base_nodes, base_children)
		worker_tc := tc.fork_for_parallel_transform(worker_ast)
		mut worker := master.fork_worker(worker_ast, worker_tc)
		new_leaf := worker_ast.add_node(flat.Node{ kind: .int_literal, value: '2' })
		worker_ast.add_child(leaf)
		worker_ast.add_child(new_leaf)
		body := worker_ast.add_node(flat.Node{
			kind: .block
			children_start: base_children
			children_count: 2
		})
		worker_ast.add_child(body)
		worker_ast.nodes[int(fn_id)] = flat.Node{
			kind: .fn_decl
			value: 'helper'
			children_start: base_children + 2
			children_count: 1
		}
		worker_ast.children[base_slot] = new_leaf
		worker.inplace_child_log << InplaceChildRewrite{ slot: base_slot, child: new_leaf }
		a.add_node(flat.Node{ kind: .int_literal, value: '3' })
		a.add_child(leaf)
		merged_leaf := flat.NodeId(a.nodes.len)
		merged_body := flat.NodeId(a.nodes.len + 1)
		node_shift := i32(a.nodes.len - base_nodes)
		child_shift := i32(a.children.len - base_children)
		if mode == 'relocated' {
			worker.relocate_region_in_place(base_nodes, worker_ast.nodes.len, base_children, worker_ast.children.len, node_shift, child_shift)
			master.merge_regions_relocated = true
		} else if mode == 'absorbed' {
			nodes_dst := a.nodes.len
			children_dst := a.children.len
			unsafe {
				a.nodes.grow_len(worker_ast.nodes.len - base_nodes)
				a.children.grow_len(worker_ast.children.len - base_children)
			}
			worker.absorb_region_into(base_nodes, worker_ast.nodes.len, base_children, worker_ast.children.len, node_shift, child_shift, unsafe { voidptr(&a.nodes[nodes_dst]) }, unsafe { voidptr(&a.children[children_dst]) })
			master.merge_regions_relocated = true
			master.merge_regions_absorbed = true
			master.merge_absorbed_node_shift = node_shift
			master.merge_absorbed_child_shift = child_shift
		}
		master.merge_worker(worker, [FnWorkItem{ fn_idx: int(fn_id) }], base_nodes, base_children, false)
		master.merge_regions_absorbed = false
		assert a.children[base_slot] == merged_leaf
		assert a.child(a.node(fn_id), 0) == merged_body
		assert a.child(a.node(merged_body), 0) == leaf
		assert a.child(a.node(merged_body), 1) == merged_leaf
	}
}

fn test_parallel_split_balances_equal_cost_functions() {
	mut items := []FnWorkItem{}
	for i in 0 .. 40 {
		items << FnWorkItem{
			fn_idx: i
			cost: 100
			rank: 100
		}
	}
	chunks := split_work_items(items, 4)
	mut seen := map[int]bool{}
	for chunk in chunks {
		assert chunk.len == 10
		mut previous := -1
		for item in chunk {
			assert item.fn_idx > previous
			assert item.fn_idx !in seen
			seen[item.fn_idx] = true
			previous = item.fn_idx
		}
	}
	assert seen.len == items.len
}

fn test_alias_receiver_index_preserves_integer_fallback_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['Count'] = 'i64'
	tc.type_aliases['OtherCount'] = 'i32'
	tc.fn_param_types['Plain.show'] = [types.Type(types.i64_)]
	tc.fn_param_types['Count.show'] = [types.Type(types.i64_)]
	tc.fn_param_types['OtherCount.show'] = [types.Type(types.i32_)]
	tc.fn_param_types['Count.other'] = [types.Type(types.i64_)]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.resolve_alias_receiver_method('u8', 'show') or { '' } == 'Count.show'
	t.collect_alias_methods()
	t.skip_generics = true
	assert t.resolve_alias_receiver_method('u16', 'show') or { '' } == 'Count.show'
	assert t.resolve_alias_receiver_method('u16', 'missing') == none
	// Generic lowering can add signatures after preparation and uses the live table.
	t.skip_generics = false
	tc.type_aliases['LateCount'] = 'u64'
	tc.fn_param_types['LateCount.late'] = [types.Type(types.u64_)]
	assert t.resolve_alias_receiver_method('u8', 'late') or { '' } == 'LateCount.late'
}

fn test_generic_app_parts_distinguishes_postfix_fixed_arrays() {
	_, _, numeric_fixed := generic_app_parts('C.sg_color_attachment_action[4]')
	assert !numeric_fixed
	_, _, const_fixed := generic_app_parts('http.HeaderKV[max_headers]')
	assert !const_fixed
	base, args, generic := generic_app_parts('json2.StructKeyDecodeResult[Item]')
	assert generic
	assert base == 'json2.StructKeyDecodeResult'
	assert args == ['Item']
	c_base, c_args, c_generic := generic_app_parts('json2.StructKeyDecodeResult[C.sg_pass_action]')
	assert c_generic
	assert c_base == 'json2.StructKeyDecodeResult'
	assert c_args == ['C.sg_pass_action']
}

fn test_normalize_function_type_preserves_mut_parameter() {
	t := Transformer{}
	assert t.normalize_type_in_module('fn (mut Item)', 'main') == 'fn (&Item)'
	assert t.normalize_type_in_module('fn (mut item Item) bool', 'main') == 'fn (&Item) bool'
}

fn test_or_payload_type_qualifies_imported_generic_base() {
	mut t := Transformer{
		cur_module: 'main'
	}
	info := StructInfo{
		name: 'QueryBuilder'
		module: 'orm'
	}
	t.structs['QueryBuilder'] = info
	t.structs['orm.QueryBuilder'] = info
	t.qualified_types['QueryBuilder'] = 'orm.QueryBuilder'
	assert t.normalize_or_expr_value_type('&QueryBuilder[AggregateEntry]') == '&orm.QueryBuilder[AggregateEntry]'
}

fn test_or_payload_type_qualifies_generic_base_in_own_module() {
	mut t := Transformer{
		cur_module: 'orm'
	}
	info := StructInfo{
		name: 'QueryBuilder'
		module: 'orm'
	}
	t.structs['QueryBuilder'] = info
	t.structs['orm.QueryBuilder'] = info
	assert t.normalize_or_expr_value_type('&QueryBuilder[AggregateEntry]') == '&orm.QueryBuilder[AggregateEntry]'
	expr_type, value_type := t.specialized_or_expr_types('!QueryBuilder[sapp.Event]')
	assert expr_type == '!orm.QueryBuilder[sapp.Event]'
	assert value_type == 'orm.QueryBuilder[sapp.Event]'
	tuple_expr_type, tuple_value_type := t.specialized_or_expr_types('!(&ui.QNode, map[string]ui.QmlEvent)')
	assert tuple_expr_type == '!(&ui.QNode, map[string]ui.QmlEvent)'
	assert tuple_value_type == '(&ui.QNode, map[string]ui.QmlEvent)'
}

fn test_specialized_receiver_method_qualifies_imported_generic_base() {
	mut t := Transformer{
		cur_module: 'main'
	}
	info := StructInfo{
		name: 'QueryBuilder'
		module: 'orm'
	}
	t.structs['QueryBuilder'] = info
	t.structs['orm.QueryBuilder'] = info
	t.qualified_types['QueryBuilder'] = 'orm.QueryBuilder'
	t.fn_ret_types['orm.QueryBuilder_Foo.v_sql_insert'] = '!int'
	assert t.resolve_specialized_generic_receiver_method('QueryBuilder[Foo]', 'v_sql_insert') or {
		''
	} == 'orm.QueryBuilder_Foo.v_sql_insert'
	assert t.resolve_receiver_method_for_type('QueryBuilder_Foo', 'v_sql_insert') or { '' } == 'orm.QueryBuilder_Foo.v_sql_insert'
}

fn test_receiver_method_resolution_follows_main_locked_struct_alias() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['AliasApp'] = 'App'
	tc.type_aliases['AliasContext'] = 'Context'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'veb'
	t.structs['App'] = StructInfo{}
	t.structs['Context'] = StructInfo{
		name: 'Context'
		fields: [
			FieldInfo{
				name: 'Context'
				typ: 'veb.Context'
				raw_typ: 'veb.Context'
				is_embedded: true
			},
		]
	}
	t.structs['veb.Context'] = StructInfo{
		name: 'Context'
		module: 'veb'
		fields: [
			FieldInfo{
				name: 'req'
				typ: 'http.Request'
				raw_typ: 'http.Request'
			},
		]
	}
	t.embedded_fields['Context'] = [
		FieldInfo{
			name: 'Context'
			typ: 'veb.Context'
			raw_typ: 'veb.Context'
		},
	]
	t.fn_ret_types['App.before_accept_loop'] = 'void'
	t.fn_ret_types['veb.Context.before_request'] = 'void'

	assert t.alias_target_type_preserving_main_lock('main.AliasApp') or { '' } == 'main.App'
	assert t.resolve_receiver_method_for_type('main.AliasApp', 'before_accept_loop') or { '' } == 'App.before_accept_loop'
	assert t.resolve_embedded_receiver_method('main.AliasContext', 'before_request') or { '' } == 'veb.Context.before_request'
	assert t.receiver_method_matches_type_name('App.before_accept_loop', 'main.AliasApp')
	path := t.embedded_receiver_path('main.AliasContext', 'veb.Context') or {
		assert false, 'expected the embedded veb.Context path through AliasContext'
		return
	}
	assert path.len == 1
	assert path[0].name == 'Context'
	promoted_path := t.struct_field_path_for_field('main.AliasContext', 'req') or {
		assert false, 'expected the promoted req path through AliasContext'
		return
	}
	assert promoted_path.len == 1
	assert promoted_path[0].name == 'Context'
}

fn test_sql_table_name_substitutes_active_generic_parameter() {
	t := Transformer{
		active_generic_params: ['T']
		active_specialization_args: ['User']
	}
	assert t.sql_resolved_table_name('T') == 'User'
	assert t.sql_table_type_names_match('main.User', 'User')
}

fn test_sql_table_name_keeps_main_lock_outside_main_module() {
	t := Transformer{
		cur_module: 'orm'
	}
	assert !t.sql_table_type_names_match('main.User', 'User')
}

fn test_normalize_type_in_module_cache_tracks_current_file() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.file_imports[file_import_key('first.v', 'dep')] = 'alpha'
	tc.file_imports[file_import_key('second.v', 'dep')] = 'beta'
	tc.structs['alpha.Type'] = []
	tc.structs['beta.Type'] = []
	mut t := Transformer{
		tc: &tc
		cur_module: 'shared'
		module_type_cache: &AliasCache{}
	}

	t.cur_file = 'first.v'
	assert t.normalize_type_in_module('dep.Type', 'shared') == 'alpha.Type'
	t.cur_file = 'second.v'
	assert t.normalize_type_in_module('dep.Type', 'shared') == 'beta.Type'
}

fn test_flattened_generic_receiver_short_variants() {
	assert flattened_generic_receiver_short_variants('foo__Bar_baz__Qux') == [
		'Bar_Qux',
	]
	assert flattened_generic_receiver_short_variants('mod.foo__Bar_baz__Qux') == [
		'Bar_Qux',
		'mod.Bar_Qux',
	]
}

fn test_receiver_method_guard_accepts_short_name_for_qualified_type() {
	t := Transformer{}
	assert t.receiver_method_matches_type_name('Thing.str', 'pkg.Thing')
}

fn test_auto_str_helper_call_uses_type_owner_module() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_modules['v.token.Pos'] = 'token'
	mut t := Transformer{
		a: &a
		tc: &tc
		cur_module: 'token'
		cur_file: 'token.v'
	}
	value := t.make_ident('pos')
	t.stringify_stack << 'Wrapper'
	call := t.request_auto_str_helper(value, 'v.token.Pos')
	callee := a.child_node(a.node(call), 0)

	assert callee.value == '__v3_autostr_v__token__Pos'
	assert t.auto_str_types['v.token.Pos'].helper_module == 'token'
}

fn test_default_clone_helper_drops_owned_rvalue_after_saving_clone() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	source := t.make_call_typed('make_entry', []flat.NodeId{}, 'Entry')
	result := t.request_default_clone_helper(source, 'Entry')

	assert a.node(result).kind == .ident
	assert t.pending_stmts.len == 3

	source_decl := a.node(t.pending_stmts[0])
	assert source_decl.kind == .decl_assign
	assert a.child(source_decl, 1) == source

	clone_decl := a.node(t.pending_stmts[1])
	assert clone_decl.kind == .decl_assign
	clone_call := a.child_node(clone_decl, 1)
	assert clone_call.kind == .call
	assert a.child_node(clone_call, 0).value == '__v3_default_clone_Entry'

	drop_stmt := a.node(t.pending_stmts[2])
	assert drop_stmt.kind == .expr_stmt
	drop_call := a.child_node(drop_stmt, 0)
	assert drop_call.kind == .call
	assert a.child_node(drop_call, 0).value == 'drop_owned'
}

fn test_borrowed_clone_stabilizes_nonaddressable_sources() {
	for typ in ['Payload', '[]Payload'] {
		mut a := flat.FlatAst.new()
		mut t := Transformer{
			a: &a
		}
		source := t.make_call_typed('borrowed_projection', []flat.NodeId{}, typ)
		t.make_compiler_default_borrowed_clone_value(source, typ, true)

		assert t.pending_stmts.len > 0
		source_decl := a.node(t.pending_stmts[0])
		assert source_decl.kind == .decl_assign
		assert a.child(source_decl, 1) == source
	}
}

fn test_program_sum_equality_helper_does_not_collide_with_cached_module_helper() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['orm.Primitive'] = ['orm.Null', 'bool']
	t.cur_module = 'orm'
	t.cur_file = 'orm.v'
	lhs := t.make_ident('lhs')
	rhs := t.make_ident('rhs')
	t.set_node_typ(int(lhs), 'orm.Primitive')
	t.set_node_typ(int(rhs), 'orm.Primitive')
	t.make_sum_semantic_eq_expr(lhs, rhs, 'orm.Primitive', []string{})

	t.sum_eq_helper_module = 'main'
	t.make_sum_semantic_eq_expr(lhs, rhs, 'orm.Primitive', []string{})

	module_helper := sum_eq_helper_name('orm.Primitive')
	program_helper := '${module_helper}__v3_program'
	assert module_helper in t.sum_eq_types
	assert program_helper in t.sum_eq_types
	assert t.sum_eq_types[module_helper].helper_module == 'orm'
	assert t.sum_eq_types[program_helper].helper_module == 'main'
}

fn test_large_recursive_pointer_auto_str_stops_before_expanding_back_edge() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	mut large_fields := []FieldInfo{cap: 65}
	for i in 0 .. 64 {
		large_fields << FieldInfo{
			name: 'value_${i}'
			typ: 'int'
		}
	}
	large_fields << FieldInfo{
		name: 'root'
		typ: '&Root'
		raw_typ: '&Root'
	}
	t.structs['Root'] = StructInfo{
		name: 'Root'
		fields: [
			FieldInfo{
				name: 'large'
				typ: '&Large'
				raw_typ: '&Large'
			},
		]
	}
	t.structs['Large'] = StructInfo{
		name: 'Large'
		fields: large_fields
	}
	t.structs['Small'] = StructInfo{
		name: 'Small'
		fields: [
			FieldInfo{
				name: 'root'
				typ: '&SmallRoot'
				raw_typ: '&SmallRoot'
			},
		]
	}
	t.structs['SmallRoot'] = StructInfo{
		name: 'SmallRoot'
		fields: [
			FieldInfo{
				name: 'small'
				typ: '&Small'
				raw_typ: '&Small'
			},
		]
	}
	t.stringify_stack << 'Root'

	assert t.ref_value_str_reaches_large_circular_graph('Large')
	t.stringify_stack.clear()
	t.stringify_stack << 'SmallRoot'
	assert !t.ref_value_str_reaches_large_circular_graph('Small')
}

fn test_if_type_merge_ignores_unresolved_branch_fallbacks() {
	t := Transformer{}
	assert t.merge_if_expr_types('unknown', 'int') == 'int'
	assert t.merge_if_expr_types('int', 'unknown') == 'int'
}

fn test_generic_inference_uses_seeded_mut_param_value_type_while_cloning() {
	mut a := flat.FlatAst.new()
	ident_id := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: '&Concrete'
	})
	mut t := Transformer{
		a: &a
		in_monomorphize_scan: true
		cloning_generic_fn_depth: 1
		var_types: [
			VarTypeBinding{
				name: 'value'
				typ: 'Concrete'
				raw_typ: 'Concrete'
			},
		]
		mut_param_values: {
			'value': true
		}
	}
	assert t.generic_call_arg_type_for_inference(ident_id) == 'Concrete'
}

fn test_lowered_generic_operator_call_records_operator_use() {
	decls := {
		'Box.+': GenericFnDecl{
			node: flat.Node{
				kind: .fn_decl
				value: 'Box[T].+'
			}
			module: 'main'
			key: 'Box.+'
		}
	}
	specs := {
		'Box[int]': 'Box'
	}
	mut indexer := Transformer{}
	lowered_operator_uses := indexer.lowered_generic_struct_operator_uses_for_specs(specs, decls)
	assert 'Box_int__plus' in lowered_operator_uses
	assert lowered_operator_uses['Box_int__plus'] == ['Box[int].+']

	mut a := flat.FlatAst.new()
	callee_id := a.add_node(flat.Node{
		kind: .ident
		value: 'Box_int__plus'
	})
	call_start := a.children.len
	a.children << callee_id
	call_id := a.add_node(flat.Node{
		kind: .call
		children_start: i32(call_start)
		children_count: flat.child_count(1)
	})
	mut t := Transformer{
		a: &a
	}
	assert t.record_lowered_generic_struct_operator_call(a.nodes[int(call_id)], lowered_operator_uses)
	assert t.used_struct_operator_fns['Box[int].+']
	assert t.used_struct_operator_fns['Box_int__plus']
}

fn test_specialized_zero_arg_method_is_not_lowered_as_generic_cast() {
	mut a := flat.FlatAst.new()
	callee_id := a.add_node(flat.Node{
		kind: .ident
		value: 'Tree_f64.min'
	})
	receiver_id := a.add_node(flat.Node{
		kind: .ident
		typ: 'Tree[f64]'
	})
	children_start := a.children.len
	a.children << callee_id
	a.children << receiver_id
	call := flat.Node{
		kind: .call
		children_start: children_start
		children_count: 2
		value: 'f64'
	}
	mut tc := types.TypeChecker.new(&a)
	tc.specialized_generic_fns['Tree_f64.min'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.try_lower_generic_sum_constructor_call(call) == none
	assert t.try_lower_generic_named_type_cast_call(call) == none
}

fn test_typeof_display_canonicalizes_fixed_array_map_values() {
	assert typeof_display_type_text('map[string]int[3]') == 'map[string][3]int'
	assert typeof_display_type_text('int[n]') == '[n]int'
	assert typeof_display_type_text('map[string]int[config.size]') == 'map[string][config.size]int'
	assert typeof_display_type_text('int[n + 1]') == '[n + 1]int'
	assert typeof_display_type_text('int[0x10]') == '[0x10]int'
	assert typeof_display_type_text('Box[T]') == 'Box[T]'
	assert typeof_display_type_text('Box[int]') == 'Box[int]'
	assert typeof_display_type_text('Box[types.Node]') == 'Box[types.Node]'
	assert typeof_display_type_text('Box[fn () int]') == 'Box[fn () int]'
	assert typeof_display_type_text('Box[chan int]') == 'Box[chan int]'
	assert typeof_display_type_text('chan int[3]') == 'chan [3]int'
	assert typeof_display_type_text('Box[int[3]]') == 'Box[[3]int]'
	assert typeof_display_type_text('Pair[int[3], Box[string[2]]]') == 'Pair[[3]int, Box[[2]string]]'
	assert typeof_display_type_text('Box[int][3]') == '[3]Box[int]'
	fixed_maps := types.Type(types.ArrayFixed{
		elem_type: types.Type(types.Map{
			key_type: types.Type(types.String{})
			value_type: types.Type(types.int_)
		})
		len: 3
	})
	assert typeof_display_resolved_type_text(fixed_maps) == '[3]map[string]int'
}

fn test_parallel_worker_reuses_prebuilt_call_param_decl_index() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'signature_index_test.v')
	a.add_val(.module_decl, 'main')
	param_id := a.add_node(flat.Node{
		kind: .param
		value: 'value'
		typ: 'string'
	})
	children_start := a.children.len
	a.children << param_id
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'takes_string'
		children_start: children_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare_parallel_call_param_types()
	assert t.call_param_types_prepared
	mut worker := t.fork_worker(t.a, t.tc)
	assert worker.call_param_types_index_ready
	assert worker.call_param_types_prepared
	assert worker.call_param_types_decl_index.len == t.call_param_types_decl_index.len
	assert worker.call_param_types_decl_cache.len == t.call_param_types_decl_cache.len
	params := worker.call_param_types_from_decl('takes_string') or {
		assert false
		return
	}
	assert params.len == 1
	assert params[0] is types.String
	t.add_call_param_types_decl_key('main.takes_string', a.nodes.len - 1, 'signature_index_test.v', 'main')
	assert !t.call_param_types_prepared
}

fn test_pending_generic_specialization_keys_are_private_initialized_maps() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.pending_generic_fn_spec_keys['master'] = true
	assert master.pending_generic_fn_spec_keys['master']

	mut worker := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))
	worker.pending_generic_fn_spec_keys['worker'] = true
	assert worker.pending_generic_fn_spec_keys['worker']
	assert 'worker' !in master.pending_generic_fn_spec_keys
}

fn test_absorb_scoped_batch_replays_overlay_into_master_checker() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.begin_sparse_transform_node_caches(0)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	mut batch_tc := tc.fork_for_parallel_transform(&a)
	batch_tc.fork_overlay.resolved_call_names[10] = 'main.resolved_call'
	batch_tc.fork_overlay.resolved_fn_values[11] = 'main.resolved_fn_value'
	batch_tc.ensure_private_transform_signatures()
	batch_tc.fn_ret_types['main.generated'] = types.Type(types.bool_)
	batch_tc.fn_param_types['main.generated'] = [types.Type(types.int_)]
	batch_tc.fn_variadic['main.generated'] = false
	mut batch := master.fork_scoped_batch_worker(&a, batch_tc)
	batch.set_fn_ret_type('main.generated', 'bool')
	assert 'main.generated' !in master.fn_ret_types
	assert 'main.generated' !in tc.fn_ret_types

	master.absorb_scoped_batch(batch, unsafe { nil }, batch.a.nodes.len)
	assert tc.sparse_resolved_call_names[10] == 'main.resolved_call'
	assert tc.sparse_resolved_fn_values[11] == 'main.resolved_fn_value'
	assert master.fn_ret_types['main.generated'] == 'bool'
	generated_ret := tc.fn_ret_types['main.generated'] or { types.Type(types.void_) }
	assert generated_ret == types.Type(types.bool_)
	assert tc.fn_param_types['main.generated'][0] == types.Type(types.int_)
}

fn test_frozen_interface_boxed_types_are_read_only_in_skip_generics_workers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.skip_generics = true
	master.interface_boxed_types['main.Reader\nmain.Source'] = true
	master.interface_boxed_types_done = true
	master.interface_boxed_types_frozen = true
	mut worker := master.fork_worker(&a, tc.fork_for_parallel_transform(&a))

	worker.mark_interface_boxed_type('main.Reader', 'main.Other')
	assert 'main.Reader\nmain.Other' !in master.interface_boxed_types
	assert 'main.Reader\nmain.Other' !in worker.interface_boxed_types
	assert worker.interface_boxed_types_late['main.Reader\nmain.Other']
	assert worker.interface_boxed_type_marked('main.Reader', 'main.Other')

	master.interface_boxed_types_frozen = false
	master.mark_interface_boxed_type('main.Reader', 'main.Other')
	assert master.interface_boxed_types['main.Reader\nmain.Other']
}

fn test_multi_return_selector_suffix_does_not_match_free_fn() {
	mut a := flat.FlatAst.new()
	receiver_id := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
	})
	selector_children_start := a.children.len
	a.children << receiver_id
	selector_id := a.add_node(flat.Node{
		kind: .selector
		value: 'pair'
		children_start: i32(selector_children_start)
		children_count: 1
	})
	call_children_start := a.children.len
	a.children << selector_id
	call_id := a.add_node(flat.Node{
		kind: .call
		children_start: i32(call_children_start)
		children_count: 1
	})
	multi_return := types.Type(types.MultiReturn{
		types: [types.Type(types.int_), types.Type(types.string_)]
	})
	mut tc := types.TypeChecker.new(a)
	tc.fn_ret_types['pair'] = multi_return
	mut t := Transformer{
		a: &a
		tc: &tc
		receiver_method_suffix_index: {
			'pair': 'pair'
		}
	}
	call := a.nodes[int(call_id)]
	if _ := t.find_multi_return_call_types(call, 2) {
		assert false, 'selector suffix lookup matched the free pair function'
	}

	tc.fn_ret_types['Container.pair'] = multi_return
	t.receiver_method_suffix_index['pair'] = 'Container.pair'
	items := t.find_multi_return_call_types(call, 2) or {
		assert false, 'selector suffix lookup did not match the receiver method'
		return
	}
	assert items.len == 2
}

fn test_qualify_or_storage_type_resolves_imported_generic_base_only() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['orm.QueryBuilder'] = []types.StructField{}
	tc.struct_generic_params['orm.QueryBuilder'] = ['T']
	tc.struct_generic_params['QueryBuilder'] = ['T']
	tc.struct_modules['orm.QueryBuilder'] = 'orm'
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.qualify_or_storage_type('&QueryBuilder[main.User]') == '&orm.QueryBuilder[main.User]'
	assert t.qualify_or_storage_type('(string, []string)') == '(string, []string)'
}

fn test_immediate_closure_generic_sum_pointer_result_may_alias_capture() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['Maybe'] = ['T', 'IError']
	tc.sum_generic_params['Maybe'] = ['T']
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.immediate_closure_result_may_alias_capture('Maybe[&int]')
}

fn test_immediate_closure_generic_struct_pointer_result_may_alias_capture() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Box'] = [types.StructField{
		name: 'value'
		typ: tc.parse_type('T')
	}]
	tc.struct_generic_params['Box'] = ['T']
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.immediate_closure_result_may_alias_capture('Box[&int]')
}

fn test_immediate_closure_result_error_may_alias_capture() {
	fallback := Transformer{}
	assert fallback.immediate_closure_result_may_alias_capture('!int')
	assert fallback.immediate_closure_result_may_alias_capture('[]int')
	assert fallback.immediate_closure_result_may_alias_capture('map[string]int')
	assert fallback.immediate_closure_result_may_alias_capture('chan int')
	assert fallback.immediate_closure_result_may_alias_capture('string')
	assert fallback.immediate_closure_result_may_alias_capture('?string')

	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['TextBox'] = [
		types.StructField{
			name: 'text'
			typ: types.Type(types.String{})
		},
	]
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.immediate_closure_result_may_alias_capture('!int')
	assert t.immediate_closure_result_may_alias_capture('[]int')
	assert t.immediate_closure_result_may_alias_capture('map[string]int')
	assert t.immediate_closure_result_may_alias_capture('chan int')
	assert t.immediate_closure_result_may_alias_capture('string')
	assert t.immediate_closure_result_may_alias_capture('?string')
	assert t.immediate_closure_result_may_alias_capture('TextBox')
}

fn test_immediate_closure_thread_result_may_alias_capture() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Worker'] = [
		types.StructField{
			name: 'handle'
			typ: tc.parse_type('thread int')
		},
	]
	with_checker := Transformer{
		a: &a
		tc: &tc
	}
	assert with_checker.immediate_closure_result_may_alias_capture('thread int')
	assert with_checker.immediate_closure_result_may_alias_capture('Worker')

	without_checker := Transformer{}
	assert without_checker.immediate_closure_result_may_alias_capture('thread int')
}

fn test_transform_lane_budget_bounds_base_ast_clones() {
	// A self-host sized base AST (~95 MiB per clone) keeps every runtime lane.
	assert transform_clone_budget_jobs(18, 100_000_000) == 18
	// Ten times that program size only affords two helper clones.
	assert transform_clone_budget_jobs(18, 1_000_000_000) == 3
	// Oversized inputs fall back to the master without a helper clone.
	assert transform_clone_budget_jobs(2, 9_000_000_000) == 1
	assert transform_clone_budget_jobs(8, 9_000_000_000) == 1
	assert transform_clone_budget_jobs(18, 1) == 18
	assert transform_clone_budget_jobs(1, 9_000_000_000) == 1
}
