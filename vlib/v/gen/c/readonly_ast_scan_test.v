module c

import v.flat
import v.types

fn readonly_scan_test_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.begin_children()
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn test_prelude_scan_preserves_nested_scope_boundaries_and_invalid_id_guards() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'C.outer_call')
	assert callee == 0
	call := readonly_scan_test_node(mut a, .call, '', [callee])
	locked_label := a.add_val(.label_stmt, 'locked')
	inner_label := a.add_val(.label_stmt, 'inner')
	inner_lock := readonly_scan_test_node(mut a, .lock_expr, '', [inner_label])
	outer_lock := readonly_scan_test_node(mut a, .lock_expr, '', [locked_label, inner_lock, call])
	outside_label := a.add_val(.label_stmt, 'outside')
	nested_defer := readonly_scan_test_node(mut a, .defer_stmt, 'function', [call])
	nested_fn := readonly_scan_test_node(mut a, .fn_literal, '', [nested_defer, call])
	defer_body_defer := readonly_scan_test_node(mut a, .defer_stmt, 'function', [call])
	outer_defer := readonly_scan_test_node(mut a, .defer_stmt, 'function', [
		defer_body_defer,
		call,
	])
	c_namespace := a.add_val(.ident, 'C')
	selector := readonly_scan_test_node(mut a, .selector, 'selector_call', [c_namespace])
	selector_call := readonly_scan_test_node(mut a, .call, '', [selector])
	empty_base := a.add(.empty)
	implicit_selector := readonly_scan_test_node(mut a, .selector, 'implicit_call', [empty_base])
	implicit_call := readonly_scan_test_node(mut a, .call, '', [implicit_selector])
	bad_range := a.add_node(flat.Node{
		kind:           .block
		children_start: i32(a.children.len + 100)
		children_count: 1
	})
	root := readonly_scan_test_node(mut a, .fn_decl, 'main', [outer_lock, outside_label, nested_fn,
		outer_defer, selector_call, implicit_call, bad_range, flat.NodeId(-1),
		flat.NodeId(a.nodes.len + 2)])
	mut tc := types.TypeChecker.new(&a)
	tc.fn_param_types['C.implicit_call'] = []types.Type{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	nodes_before := a.nodes.clone()
	children_before := a.children.clone()
	scan := g.collect_fn_prelude_scan(a.nodes[int(root)])
	assert scan.defer_ids == [outer_defer]
	assert scan.goto_label_lock_scopes['locked'] == [int(outer_lock)]
	assert scan.goto_label_lock_scopes['inner'] == [int(outer_lock), int(inner_lock)]
	assert scan.goto_label_lock_scopes['outside'] == []int{}
	assert scan.lock_scopes.len == 0
	assert scan.c_fn_calls.len == 3
	assert scan.c_fn_calls['outer_call']
	assert scan.c_fn_calls['selector_call']
	assert scan.c_fn_calls['implicit_call']
	assert a.nodes == nodes_before
	assert a.children == children_before
	mut empty_scan := new_fn_prelude_scan()
	g.collect_prelude_scan_from(flat.NodeId(-1), mut empty_scan, true)
	g.collect_prelude_scan_from(flat.NodeId(a.nodes.len), mut empty_scan, true)
	assert empty_scan.defer_ids.len == 0
	assert empty_scan.c_fn_calls.len == 0
}

fn test_serial_gen_info_scan_preserves_metadata_and_incremental_counts() {
	mut a := flat.FlatAst.new()
	file := a.add_val(.file, 'main.v')
	mod := a.add_val(.module_decl, 'main')
	import_decl := a.add_val(.import_decl, 'time')
	first_fn := a.add_val(.fn_decl, 'first')
	second_fn := a.add_val(.fn_decl, 'second')
	struct_decl := a.add_val(.struct_decl, 'Item')
	global_decl := a.add_node(flat.Node{ kind: .global_decl, children_count: 2 })
	const_decl := a.add_node(flat.Node{ kind: .const_decl, children_count: 3 })
	enum_decl := a.add_node(flat.Node{ kind: .enum_decl, children_count: 4 })
	interface_decl := a.add_val(.interface_decl, 'Reader')
	a.add_val(.string_literal, 'text')
	a.add_node(flat.Node{
		kind:  .string_literal
		value: 'embedded bytes'
		flags: flat.node_flag_embed_payload
	})
	fixed_array := a.add_node(flat.Node{ kind: .ident, typ: '[2]int', value: 'values' })
	optional_sizeof := a.add_val(.sizeof_expr, '?int')
	shared_decl := a.add_val(.decl_assign, 'shared:Item')
	a.add_val(.ident, 'ordinary')
	mut g := FlatGen.new()
	g.a = &a
	nodes_before := a.nodes.clone()
	counts := g.scan_collect_gen_info_serial()
	assert counts.fn_count == 2
	assert counts.struct_count == 1
	assert counts.global_count == 2
	assert counts.const_count == 3
	assert counts.enum_field_count == 4
	assert counts.interface_count == 1
	assert counts.import_count == 1
	assert g.ast_string_literals == ['text']
	assert g.top_level_node_ids == [file, mod, import_decl, first_fn, second_fn, struct_decl,
		global_decl, const_decl, enum_decl, interface_decl]
	assert g.type_metadata_nodes_ready
	assert g.type_metadata_node_ids == [file, mod, first_fn, second_fn, global_decl, fixed_array,
		optional_sizeof, shared_decl]
	g.incremental_fn_names['second'] = true
	incremental_counts := g.scan_collect_gen_info_serial()
	assert incremental_counts.fn_count == 1
	assert incremental_counts.struct_count == counts.struct_count
	assert g.ast_string_literals == ['text']
	assert a.nodes == nodes_before
}

fn test_usable_expr_type_preserves_builtin_string_and_byte_results() {
	mut a := flat.FlatAst.new()
	literal := a.add_val(.string_literal, 'text')
	interpolation := a.add(.string_interp)
	index_value := a.add_val(.int_literal, '0')
	index := readonly_scan_test_node(mut a, .index, '', [literal, index_value])
	slice := readonly_scan_test_node(mut a, .index, 'range', [literal, index_value])
	mut g := FlatGen.new()
	g.a = &a
	assert g.usable_expr_type(literal) == types.builtin_type_value('string')
	assert g.usable_expr_type(interpolation) == types.builtin_type_value('string')
	assert g.usable_expr_type(slice) == types.builtin_type_value('string')
	assert g.usable_expr_type(index) == types.builtin_type_value('u8')
}
