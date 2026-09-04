module transform

import v3.flat
import v3.token
import v3.types

fn test_monomorph_job_count_does_not_start_empty_workers() {
	$if !v3_no_parallel ? {
		assert monomorph_job_count(16, 1) == 1
		assert monomorph_job_count(16, 3) == 3
		assert monomorph_job_count(2, 8) == 2
	}
}

fn test_monomorph_job_limit_caps_large_programs() {
	$if !v3_no_parallel ? {
		assert monomorph_job_limit(12, 499_999, 0) == 4
		assert monomorph_job_limit(12, 500_000, 0) == 4
		assert monomorph_job_limit(1, 500_000, 0) == 1
		assert monomorph_job_limit(12, 500_000, 6) == 6
		assert monomorph_job_limit(4, 500_000, 8) == 4
	}
}

fn test_generated_calls_publish_exact_resolution_except_cgen_intrinsics() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	call_id := t.make_call('main.helper', []flat.NodeId{})
	assert tc.resolved_call_name(call_id)? == 'main.helper'
	intrinsic_id := t.make_call('__v3_clone_owned_ierror', []flat.NodeId{})
	assert tc.resolved_call_name(intrinsic_id) == none
}

fn test_forwarded_optional_conversion_propagates_borrowed_clone() {
	mut a := flat.FlatAst.new()
	source := a.add_node(flat.Node{
		kind: .ident
		value: 'source'
		typ: '?string'
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	payload := types.Type(types.string_)
	optional := types.Type(types.OptionType{
		base_type: types.Type(types.string_)
	})

	result := t.convert_forwarded_optional_result(source, optional, payload, optional, optional, payload, true)

	assert result != source
	mut saw_clone := false
	mut saw_error_clone := false
	for i, node in a.nodes {
		if node.kind == .call && tc.resolved_call_name(flat.NodeId(i)) or { '' } == 'string__clone' {
			saw_clone = true
		}
		if node.kind == .call && node.children_count > 0 {
			callee := a.child_node(&node, 0)
			if callee.kind == .ident && callee.value == '__v3_clone_owned_ierror' {
				saw_error_clone = true
			}
		}
	}
	assert saw_clone
	assert saw_error_clone
}

fn test_const_map_expansion_estimate_ignores_shadowing_local() {
	mut a := flat.FlatAst.new()
	const_id := a.add_node(flat.Node{
		kind: .map_init
	})
	param := a.add_node(flat.Node{
		kind: .param
		value: 'lookup'
		typ: 'map[string]int'
	})
	ident := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	block_start := a.children.len
	a.children << ident
	block := a.add_node(flat.Node{
		kind: .block
		children_start: block_start
		children_count: 1
	})
	fn_start := a.children.len
	a.children << param
	a.children << block
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'shadowed'
		children_start: fn_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.const_exprs['lookup'] = const_id
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()
	assert t.collection_const_expr_for_ident(ident) == none
}

fn test_const_map_expansion_estimate_ignores_stale_transformer_local() {
	mut a := flat.FlatAst.new()
	const_id := a.add_node(flat.Node{
		kind: .map_init
	})
	ident := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	block_start := a.children.len
	a.children << ident
	block := a.add_node(flat.Node{
		kind: .block
		children_start: block_start
		children_count: 1
	})
	fn_start := a.children.len
	a.children << block
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'uses_const'
		children_start: fn_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.const_exprs['lookup'] = const_id
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()
	t.set_var_type('lookup', 'map[string]int')
	assert t.collection_const_expr_for_ident(ident)? == const_id
}

fn test_const_map_expansion_estimate_recognizes_for_in_bindings_only_in_body() {
	mut a := flat.FlatAst.new()
	const_id := a.add_node(flat.Node{
		kind: .map_init
	})
	empty := a.add_node(flat.Node{
		kind: .empty
	})
	binding := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	container := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	body_ident := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	loop_start := a.children.len
	a.children << empty
	a.children << binding
	a.children << container
	a.children << body_ident
	loop := a.add_node(flat.Node{
		kind: .for_in_stmt
		value: '3'
		children_start: loop_start
		children_count: 4
	})
	fn_start := a.children.len
	a.children << loop
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'shadowed_in_loop'
		children_start: fn_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.const_exprs['lookup'] = const_id
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()

	assert t.collection_const_expr_for_ident(container)? == const_id
	assert t.collection_const_expr_for_ident(body_ident) == none
}

fn test_const_map_expansion_estimate_keeps_if_guard_shadow_out_of_else_branch() {
	mut a := flat.FlatAst.new()
	const_id := a.add_node(flat.Node{
		kind: .map_init
	})
	guard_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	guard_rhs := a.add_node(flat.Node{
		kind: .call
	})
	guard_start := a.children.len
	a.children << guard_lhs
	a.children << guard_rhs
	guard := a.add_node(flat.Node{
		kind: .decl_assign
		children_start: guard_start
		children_count: 2
	})
	then_ident := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	then_start := a.children.len
	a.children << then_ident
	then_block := a.add_node(flat.Node{
		kind: .block
		children_start: then_start
		children_count: 1
	})
	else_ident := a.add_node(flat.Node{
		kind: .ident
		value: 'lookup'
	})
	else_start := a.children.len
	a.children << else_ident
	else_block := a.add_node(flat.Node{
		kind: .block
		children_start: else_start
		children_count: 1
	})
	if_start := a.children.len
	a.children << guard
	a.children << then_block
	a.children << else_block
	if_expr := a.add_node(flat.Node{
		kind: .if_expr
		children_start: if_start
		children_count: 3
	})
	fn_start := a.children.len
	a.children << if_expr
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'guarded_lookup'
		children_start: fn_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.const_exprs['lookup'] = const_id
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()

	assert t.collection_const_expr_for_ident(then_ident) == none
	assert t.collection_const_expr_for_ident(else_ident)? == const_id
}

fn test_map_expansion_estimate_includes_owned_value_cleanup() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	plain_id := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[int]int'
		children_count: 254
	})
	owned_id := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[int]string'
		children_count: 254
	})
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.map_init_expansion_estimate(plain_id, a.nodes[int(plain_id)]) < deferred_map_expansion_threshold
	assert t.map_init_expansion_estimate(owned_id, a.nodes[int(owned_id)]) > deferred_map_expansion_threshold
}

fn test_map_expansion_estimate_defers_metadata_driven_spread_clone() {
	mut a := flat.FlatAst.new()
	spread_source := a.add_node(flat.Node{
		kind: .ident
		typ: 'map[int][]string'
	})
	spread_start := a.children.len
	a.children << spread_source
	spread := a.add_node(flat.Node{
		kind: .prefix
		value: '...'
		children_start: spread_start
		children_count: 1
	})
	map_start := a.children.len
	a.children << spread
	a.children << a.add_node(flat.Node{
		kind: .empty
	})
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[int][]string'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.map_init_expansion_estimate(root, a.nodes[int(root)]) > deferred_map_expansion_threshold
}

fn test_array_literal_expansion_estimate_defers_metadata_driven_spread_clone() {
	mut a := flat.FlatAst.new()
	spread_source := a.add_node(flat.Node{
		kind: .ident
		typ: '[][]string'
	})
	spread_start := a.children.len
	a.children << spread_source
	spread := a.add_node(flat.Node{
		kind: .prefix
		value: '...'
		children_start: spread_start
		children_count: 1
	})
	array_start := a.children.len
	a.children << spread
	root := a.add_node(flat.Node{
		kind: .array_literal
		typ: '[][]string'
		children_start: array_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.array_literal_expansion_estimate(root, a.nodes[int(root)], false) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_nested_array_lowering() {
	mut a := flat.FlatAst.new()
	mut inner_arrays := []flat.NodeId{cap: 130}
	for i in 0 .. 130 {
		value := a.add_node(flat.Node{
			kind: .int_literal
			value: i.str()
		})
		inner_start := a.children.len
		a.children << value
		inner_arrays << a.add_node(flat.Node{
			kind: .array_literal
			children_start: inner_start
			children_count: 1
		})
	}
	outer_start := a.children.len
	for inner in inner_arrays {
		a.children << inner
	}
	outer := a.add_node(flat.Node{
		kind: .array_literal
		children_start: outer_start
		children_count: flat.child_count(inner_arrays.len)
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << outer
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string][][]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_follows_nested_const_index() {
	mut a := flat.FlatAst.new()
	entry := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
	})
	large_start := a.children.len
	for _ in 0 .. 256 {
		a.children << entry
	}
	large := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[int]int'
		children_start: large_start
		children_count: 256
	})
	large_ident := a.add_node(flat.Node{
		kind: .ident
		value: 'large'
		typ: 'map[int]int'
	})
	large_key := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
	})
	index_start := a.children.len
	a.children << large_ident
	a.children << large_key
	large_index := a.add_node(flat.Node{
		kind: .index
		children_start: index_start
		children_count: 2
	})
	outer_key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'item'
	})
	outer_start := a.children.len
	a.children << outer_key
	a.children << large_index
	outer := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: outer_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.const_exprs['large'] = large
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(outer, 0, 0) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_ignores_const_array_index() {
	mut a := flat.FlatAst.new()
	entry := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
	})
	array_start := a.children.len
	for _ in 0 .. deferred_map_expansion_threshold + 1 {
		a.children << entry
	}
	large := a.add_node(flat.Node{
		kind: .array_literal
		typ: '[]int'
		children_start: array_start
		children_count: flat.child_count(deferred_map_expansion_threshold + 1)
	})
	large_ident := a.add_node(flat.Node{
		kind: .ident
		value: 'large'
		typ: '[]int'
	})
	large_key := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
	})
	index_start := a.children.len
	a.children << large_ident
	a.children << large_key
	large_index := a.add_node(flat.Node{
		kind: .index
		typ: 'int'
		children_start: index_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.const_exprs['large'] = large
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate := t.fn_span_map_expansion_estimate(int(large_index), int(large_index) + 1)
	assert estimate < deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_direct_array_copy() {
	mut a := flat.FlatAst.new()
	children_start := a.children.len
	for i in 0 .. deferred_map_expansion_threshold + 1 {
		a.children << a.add_node(flat.Node{
			kind: .int_literal
			value: i.str()
		})
	}
	array := a.add_node(flat.Node{
		kind: .array_literal
		children_start: children_start
		children_count: flat.child_count(deferred_map_expansion_threshold + 1)
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << array
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string][]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.array_literal_can_emit_direct(a.nodes[int(array)])
	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_struct_reconstruction() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .struct_init
		typ: 'LargeValue'
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'item'
	})
	map_start := a.children.len
	a.children << key
	a.children << value
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]LargeValue'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_fixed_array_init() {
	mut a := flat.FlatAst.new()
	index := a.add_node(flat.Node{
		kind: .ident
		value: 'index'
		typ: 'int'
	})
	field_start := a.children.len
	a.children << index
	init_field := a.add_node(flat.Node{
		kind: .field_init
		value: 'init'
		children_start: field_start
		children_count: 1
	})
	array_start := a.children.len
	a.children << init_field
	root := a.add_node(flat.Node{
		kind: .array_init
		typ: 'int[4096]'
		children_start: array_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_empty_fixed_array_runtime_init() {
	mut a := flat.FlatAst.new()
	root := a.add_node(flat.Node{
		kind: .array_init
		typ: '[4096][]int'
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate := t.external_map_tree_expansion_estimate(root, 0, 0)
	assert estimate > deferred_map_expansion_threshold

	plain := a.add_node(flat.Node{
		kind: .array_init
		typ: 'int[4096]'
	})
	assert t.fixed_array_init_expansion_estimate(plain, a.nodes[int(plain)]) == 0
}

fn test_fn_span_map_expansion_estimate_includes_fixed_array_runtime_init() {
	mut a := flat.FlatAst.new()
	root := a.add_node(flat.Node{
		kind: .array_init
		typ: '[4096][]int'
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_includes_comptime_zero_value_markers() {
	mut a := flat.FlatAst.new()
	mut markers := []flat.NodeId{}
	for marker_name in ['__v3_comptime_zero', '__v3_comptime_new'] {
		target := a.add_node(flat.Node{
			kind: .ident
			value: '[4096][]int'
		})
		start := a.children.len
		a.children << target
		markers << a.add_node(flat.Node{
			kind: .string_literal
			value: marker_name
			typ: 'string'
			children_start: start
			children_count: 1
		})
	}
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	for marker in markers {
		assert t.fn_span_map_expansion_estimate(int(marker), int(marker) + 1) > deferred_map_expansion_threshold
	}
}

fn test_fn_span_map_expansion_estimate_includes_map_index_zero_value() {
	mut a := flat.FlatAst.new()
	base := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[string][4096][]int'
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'missing'
		typ: 'string'
	})
	index_start := a.children.len
	a.children << base
	a.children << key
	root := a.add_node(flat.Node{
		kind: .index
		typ: '[4096][]int'
		children_start: index_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_owned_array_index_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	base := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[][4096][]int'
	})
	key := a.add_node(flat.Node{
		kind: .int_literal
		value: '0'
		typ: 'int'
	})
	index_start := a.children.len
	a.children << base
	a.children << key
	root := a.add_node(flat.Node{
		kind: .index
		typ: '[4096][]int'
		children_start: index_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.owned_array_index_zero_value_expansion_estimate(a.nodes[int(root)], true) > deferred_map_expansion_threshold
	assert t.owned_array_index_zero_value_expansion_estimate(a.nodes[int(root)], false) == 0
}

fn test_if_expr_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	condition := a.add_node(flat.Node{
		kind: .bool_literal
		value: 'true'
		typ: 'bool'
	})
	then_value := a.add_node(flat.Node{
		kind: .call
		value: 'first'
		typ: '[4096][]int'
	})
	then_start := a.children.len
	a.children << then_value
	then_block := a.add_node(flat.Node{
		kind: .block
		children_start: then_start
		children_count: 1
	})
	else_value := a.add_node(flat.Node{
		kind: .call
		value: 'second'
		typ: '[4096][]int'
	})
	else_start := a.children.len
	a.children << else_value
	else_block := a.add_node(flat.Node{
		kind: .block
		children_start: else_start
		children_count: 1
	})
	if_start := a.children.len
	a.children << condition
	a.children << then_block
	a.children << else_block
	root := a.add_node(flat.Node{
		kind: .if_expr
		typ: '[4096][]int'
		children_start: if_start
		children_count: 3
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_channel_receive_if_guard_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	channel := a.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: 'chan [4096][]int'
	})
	receive_start := a.children.len
	a.children << channel
	receive := a.add_node(flat.Node{
		kind: .prefix
		op: .arrow
		typ: '[4096][]int'
		children_start: receive_start
		children_count: 1
	})
	lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
	})
	guard_start := a.children.len
	a.children << lhs
	a.children << receive
	guard := a.add_node(flat.Node{
		kind: .decl_assign
		children_start: guard_start
		children_count: 2
	})
	body := a.add_node(flat.Node{
		kind: .block
	})
	if_start := a.children.len
	a.children << guard
	a.children << body
	root := a.add_node(flat.Node{
		kind: .if_expr
		children_start: if_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_multi_return_if_zero_value_expansion_is_reserved_per_slot() {
	mut a := flat.FlatAst.new()
	condition := a.add_node(flat.Node{
		kind: .bool_literal
		value: 'true'
		typ: 'bool'
	})
	mut branches := []flat.NodeId{}
	for name in ['first', 'second'] {
		large := a.add_node(flat.Node{
			kind: .call
			value: '${name}_large'
			typ: '[4096][]int'
		})
		number := a.add_node(flat.Node{
			kind: .call
			value: '${name}_number'
			typ: 'int'
		})
		expr_start := a.children.len
		a.children << large
		a.children << number
		expr := a.add_node(flat.Node{
			kind: .expr_stmt
			children_start: expr_start
			children_count: 2
		})
		block_start := a.children.len
		a.children << expr
		branches << a.add_node(flat.Node{
			kind: .block
			children_start: block_start
			children_count: 1
		})
	}
	if_start := a.children.len
	a.children << condition
	a.children << branches[0]
	a.children << branches[1]
	root := a.add_node(flat.Node{
		kind: .if_expr
		typ: '([4096][]int, int)'
		children_start: if_start
		children_count: 3
	})
	large_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'large'
	})
	number_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'number'
	})
	decl_start := a.children.len
	a.children << large_lhs
	a.children << root
	a.children << number_lhs
	a.add_node(flat.Node{
		kind: .decl_assign
		value: '2'
		children_start: decl_start
		children_count: 3
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_multi_return_match_zero_value_expansion_is_reserved_per_slot() {
	mut a := flat.FlatAst.new()
	subject := a.add_node(flat.Node{
		kind: .bool_literal
		value: 'true'
		typ: 'bool'
	})
	mut branches := []flat.NodeId{}
	for i, name in ['first', 'second'] {
		large := a.add_node(flat.Node{
			kind: .call
			value: '${name}_large'
			typ: '[4096][]int'
		})
		number := a.add_node(flat.Node{
			kind: .call
			value: '${name}_number'
			typ: 'int'
		})
		expr_start := a.children.len
		a.children << large
		a.children << number
		expr := a.add_node(flat.Node{
			kind: .expr_stmt
			children_start: expr_start
			children_count: 2
		})
		body_start := a.children.len
		a.children << expr
		body := a.add_node(flat.Node{
			kind: .block
			value: 'comma_exprs'
			children_start: body_start
			children_count: 1
		})
		branch_start := a.children.len
		if i == 0 {
			condition := a.add_node(flat.Node{
				kind: .bool_literal
				value: 'true'
				typ: 'bool'
			})
			a.children << condition
		}
		a.children << body
		branches << a.add_node(flat.Node{
			kind: .match_branch
			value: if i == 0 { '1' } else { 'else' }
			children_start: branch_start
			children_count: if i == 0 { 2 } else { 1 }
		})
	}
	match_start := a.children.len
	a.children << subject
	a.children << branches[0]
	a.children << branches[1]
	root := a.add_node(flat.Node{
		kind: .match_stmt
		typ: '([4096][]int, int)'
		children_start: match_start
		children_count: 3
	})
	large_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'large'
	})
	number_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'number'
	})
	decl_start := a.children.len
	a.children << large_lhs
	a.children << root
	a.children << number_lhs
	a.add_node(flat.Node{
		kind: .decl_assign
		value: '2'
		children_start: decl_start
		children_count: 3
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_match_expr_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	subject := a.add_node(flat.Node{
		kind: .bool_literal
		value: 'true'
		typ: 'bool'
	})
	condition := a.add_node(flat.Node{
		kind: .bool_literal
		value: 'true'
		typ: 'bool'
	})
	value := a.add_node(flat.Node{
		kind: .call
		value: 'make_big'
		typ: '[4096][]int'
	})
	branch_start := a.children.len
	a.children << condition
	a.children << value
	branch := a.add_node(flat.Node{
		kind: .match_branch
		children_start: branch_start
		children_count: 2
	})
	root_start := a.children.len
	a.children << subject
	a.children << branch
	root := a.add_node(flat.Node{
		kind: .match_stmt
		typ: '[4096][]int'
		children_start: root_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_nested_optional_leaf_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	optional := a.add_node(flat.Node{
		kind: .call
		value: 'maybe_big'
		typ: '?[4096][]int'
	})
	expected := a.add_node(flat.Node{
		kind: .ident
		value: 'big'
		typ: '[4096][]int'
	})
	comparison_start := a.children.len
	a.children << optional
	a.children << expected
	comparison := a.add_node(flat.Node{
		kind: .infix
		op: .eq
		typ: 'bool'
		children_start: comparison_start
		children_count: 2
	})
	fallback := a.add_node(flat.Node{
		kind: .bool_literal
		value: 'false'
		typ: 'bool'
	})
	fallback_start := a.children.len
	a.children << fallback
	fallback_block := a.add_node(flat.Node{
		kind: .block
		children_start: fallback_start
		children_count: 1
	})
	or_start := a.children.len
	a.children << comparison
	a.children << fallback_block
	root := a.add_node(flat.Node{
		kind: .or_expr
		typ: 'bool'
		children_start: or_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(optional), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_forwarded_fixed_array_return_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: '[4096]int'
	})
	return_start := a.children.len
	a.children << value
	return_stmt := a.add_node(flat.Node{
		kind: .return_stmt
		typ: '[4096]i64'
		children_start: return_start
		children_count: 1
	})
	fn_start := a.children.len
	a.children << return_stmt
	fn_decl := a.add_node(flat.Node{
		kind: .fn_decl
		value: 'forward'
		typ: '[4096]i64'
		children_start: fn_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(value), int(fn_decl)) > deferred_map_expansion_threshold
}

fn test_forwarded_wrapped_fixed_array_return_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: '?[4096]int'
	})
	return_start := a.children.len
	a.children << value
	return_stmt := a.add_node(flat.Node{
		kind: .return_stmt
		typ: '?[4096]i64'
		children_start: return_start
		children_count: 1
	})
	fn_start := a.children.len
	a.children << return_stmt
	fn_decl := a.add_node(flat.Node{
		kind: .fn_decl
		value: 'forward_optional'
		typ: '?[4096]i64'
		children_start: fn_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(value), int(fn_decl)) > deferred_map_expansion_threshold
}

fn test_forwarded_container_fixed_array_return_expansion_is_reserved() {
	mut array_ast := flat.FlatAst.new()
	array_value := array_ast.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: '[][4096]int'
	})
	array_return_start := array_ast.children.len
	array_ast.children << array_value
	array_return := array_ast.add_node(flat.Node{
		kind: .return_stmt
		typ: '[][4096]i64'
		children_start: array_return_start
		children_count: 1
	})
	array_fn_start := array_ast.children.len
	array_ast.children << array_return
	array_fn := array_ast.add_node(flat.Node{
		kind: .fn_decl
		value: 'promote_array'
		typ: '[][4096]i64'
		children_start: array_fn_start
		children_count: 1
	})
	mut array_tc := types.TypeChecker.new(&array_ast)
	mut array_transformer := new_transformer(mut array_ast, &array_tc, map[string]bool{})
	assert array_transformer.fn_span_map_expansion_estimate(int(array_value), int(array_fn)) > deferred_map_expansion_threshold

	mut map_ast := flat.FlatAst.new()
	map_value := map_ast.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: 'map[string][4096]int'
	})
	map_return_start := map_ast.children.len
	map_ast.children << map_value
	map_return := map_ast.add_node(flat.Node{
		kind: .return_stmt
		typ: 'map[string][4096]i64'
		children_start: map_return_start
		children_count: 1
	})
	map_fn_start := map_ast.children.len
	map_ast.children << map_return
	map_fn := map_ast.add_node(flat.Node{
		kind: .fn_decl
		value: 'promote_map'
		typ: 'map[string][4096]i64'
		children_start: map_fn_start
		children_count: 1
	})
	mut map_tc := types.TypeChecker.new(&map_ast)
	mut map_transformer := new_transformer(mut map_ast, &map_tc, map[string]bool{})
	assert map_transformer.fn_span_map_expansion_estimate(int(map_value), int(map_fn)) > deferred_map_expansion_threshold

	mut lookup_ast := flat.FlatAst.new()
	lookup_value := lookup_ast.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: 'map[int][4096][]int'
	})
	lookup_return_start := lookup_ast.children.len
	lookup_ast.children << lookup_value
	lookup_return := lookup_ast.add_node(flat.Node{
		kind: .return_stmt
		typ: 'map[i64][4096][]int'
		children_start: lookup_return_start
		children_count: 1
	})
	lookup_fn_start := lookup_ast.children.len
	lookup_ast.children << lookup_return
	lookup_fn := lookup_ast.add_node(flat.Node{
		kind: .fn_decl
		value: 'promote_map_key'
		typ: 'map[i64][4096][]int'
		children_start: lookup_fn_start
		children_count: 1
	})
	mut lookup_tc := types.TypeChecker.new(&lookup_ast)
	mut lookup_transformer := new_transformer(mut lookup_ast, &lookup_tc, map[string]bool{})
	assert lookup_transformer.fn_span_map_expansion_estimate(int(lookup_value), int(lookup_fn)) > deferred_map_expansion_threshold
}

fn test_disabled_call_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind: .ident
		value: 'disabled_big'
	})
	call_start := a.children.len
	a.children << callee
	call := a.add_node(flat.Node{
		kind: .call
		typ: '[4096][]int'
		children_start: call_start
		children_count: 1
	})
	a.disabled_fns['disabled_big'] = true
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(callee), int(call) + 1) > deferred_map_expansion_threshold
}

fn test_disabled_struct_operator_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'lhs'
		typ: 'Box'
	})
	rhs := a.add_node(flat.Node{
		kind: .ident
		value: 'rhs'
		typ: 'Box'
	})
	infix_start := a.children.len
	a.children << lhs
	a.children << rhs
	infix := a.add_node(flat.Node{
		kind: .infix
		typ: '[4096][]int'
		op: .plus
		children_start: infix_start
		children_count: 2
	})
	a.disabled_fns['Box.+'] = true
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Box'] = []types.StructField{}
	box_type := tc.parse_type('Box')
	tc.fn_param_types['Box.+'] = [box_type, box_type]
	tc.fn_ret_types['Box.+'] = tc.parse_type('[4096][]int')
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Box'] = StructInfo{
		name: 'Box'
	}

	assert t.fn_span_map_expansion_estimate(int(lhs), int(infix) + 1) > deferred_map_expansion_threshold
}

fn test_variant_types_metadata_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	type_value := a.add_node(flat.Node{
		kind: .typeof_expr
		value: 'WideSum'
		typ: 'TypeInfo'
	})
	selector_start := a.children.len
	a.children << type_value
	variant_types := a.add_node(flat.Node{
		kind: .selector
		value: 'variant_types'
		typ: '[]int'
		children_start: selector_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['WideSum'] = ['First', 'Second']

	assert t.external_selector_expands_from_type_metadata(a.nodes[int(variant_types)])
	assert t.fn_span_map_expansion_estimate(int(type_value), int(variant_types) + 1) > deferred_map_expansion_threshold
}

fn test_sql_expr_metadata_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	sql_expr := a.add_node(flat.Node{
		kind: .sql_expr
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.fn_span_map_expansion_estimate(int(sql_expr), int(sql_expr) + 1) > deferred_map_expansion_threshold
}

fn test_interface_cast_metadata_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'Concrete'
	})
	cast_start := a.children.len
	a.children << value
	cast := a.add_node(flat.Node{
		kind: .cast_expr
		value: 'WideInterface'
		typ: 'WideInterface'
		children_start: cast_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['WideInterface'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.interface_cast_expands_from_type_metadata(a.nodes[int(cast)])
	assert t.fn_span_map_expansion_estimate(int(value), int(cast) + 1) > deferred_map_expansion_threshold
	assert t.external_map_tree_expansion_estimate(cast, 0, 0) > deferred_map_expansion_threshold
}

fn test_or_expr_zero_value_expansion_is_reserved() {
	mut a := flat.FlatAst.new()
	optional_value := a.add_node(flat.Node{
		kind: .call
		value: 'make_big'
		typ: '?[4096][]int'
	})
	fallback_value := a.add_node(flat.Node{
		kind: .call
		value: 'fallback'
		typ: '[4096][]int'
	})
	fallback_start := a.children.len
	a.children << fallback_value
	fallback_block := a.add_node(flat.Node{
		kind: .block
		children_start: fallback_start
		children_count: 1
	})
	root_start := a.children.len
	a.children << optional_value
	a.children << fallback_block
	root := a.add_node(flat.Node{
		kind: .or_expr
		typ: '[4096][]int'
		children_start: root_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_sum_type_map_index_zero_value() {
	mut a := flat.FlatAst.new()
	base := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[string]Item'
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'missing'
		typ: 'string'
	})
	index_start := a.children.len
	a.children << base
	a.children << key
	root := a.add_node(flat.Node{
		kind: .index
		typ: 'Item'
		children_start: index_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Item'] = ['Wide', 'int']

	assert t.fn_span_map_expansion_estimate(int(root), int(root) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_dynamic_array_initialization() {
	mut a := flat.FlatAst.new()
	length := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	len_field_start := a.children.len
	a.children << length
	len_field := a.add_node(flat.Node{
		kind: .field_init
		value: 'len'
		children_start: len_field_start
		children_count: 1
	})
	default_start := a.children.len
	a.children << len_field
	default_root := a.add_node(flat.Node{
		kind: .array_init
		typ: '[]Wide'
		children_start: default_start
		children_count: 1
	})
	initial := a.add_node(flat.Node{
		kind: .ident
		value: 'make_wide()'
		typ: 'Wide'
	})
	init_field_start := a.children.len
	a.children << initial
	init_field := a.add_node(flat.Node{
		kind: .field_init
		value: 'init'
		children_start: init_field_start
		children_count: 1
	})
	explicit_start := a.children.len
	a.children << len_field
	a.children << init_field
	explicit_root := a.add_node(flat.Node{
		kind: .array_init
		typ: '[]Wide'
		children_start: explicit_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Wide'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(length), int(default_root) + 1) > deferred_map_expansion_threshold
	assert t.fn_span_map_expansion_estimate(int(initial), int(explicit_root) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_compiler_default_clone_calls() {
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'wide'
		typ: 'Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'clone'
		typ: 'fn () Wide'
		children_start: selector_start
		children_count: 1
	})
	call_start := a.children.len
	a.children << selector
	clone_call := a.add_node(flat.Node{
		kind: .call
		typ: 'Wide'
		children_start: call_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Wide'] = []types.StructField{}
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(int(receiver), int(clone_call) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_struct_defaults() {
	mut a := flat.FlatAst.new()
	default_value := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	init := a.add_node(flat.Node{
		kind: .struct_init
		value: 'Wide'
		typ: 'Wide'
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Wide'] = StructInfo{
		name: 'Wide'
		fields: [
			FieldInfo{
				name: 'value'
				typ: 'int'
				default_expr: default_value
			},
		]
	}

	assert t.fn_span_map_expansion_estimate(int(init), int(init) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_metadata_driven_calls_and_equality() {
	mut metadata_ast := flat.FlatAst.new()
	type_idx_call := add_runtime_metadata_call(mut metadata_ast, 'item', 'Item', 'type_idx', 'int')
	mut metadata_tc := types.TypeChecker.new(&metadata_ast)
	mut metadata_transformer := new_transformer(mut metadata_ast, &metadata_tc, map[string]bool{})
	metadata_transformer.sum_types['Item'] = ['First', 'Second']
	assert metadata_transformer.fn_span_map_expansion_estimate(0, int(type_idx_call) + 1) > deferred_map_expansion_threshold

	mut equality_ast := flat.FlatAst.new()
	left := equality_ast.add_node(flat.Node{
		kind: .ident
		value: 'left'
		typ: 'Wide'
	})
	right := equality_ast.add_node(flat.Node{
		kind: .ident
		value: 'right'
		typ: 'Wide'
	})
	equality_start := equality_ast.children.len
	equality_ast.children << left
	equality_ast.children << right
	equality := equality_ast.add_node(flat.Node{
		kind: .infix
		op: .eq
		typ: 'bool'
		children_start: equality_start
		children_count: 2
	})
	mut equality_tc := types.TypeChecker.new(&equality_ast)
	mut equality_transformer := new_transformer(mut equality_ast, &equality_tc, map[string]bool{})
	equality_transformer.structs['Wide'] = StructInfo{
		name: 'Wide'
	}
	assert equality_transformer.fn_span_map_expansion_estimate(0, int(equality) + 1) > deferred_map_expansion_threshold

	mut predicate_ast := flat.FlatAst.new()
	predicate_value := predicate_ast.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'View'
	})
	predicate_start := predicate_ast.children.len
	predicate_ast.children << predicate_value
	predicate := predicate_ast.add_node(flat.Node{
		kind: .is_expr
		value: 'Target'
		typ: 'bool'
		children_start: predicate_start
		children_count: 1
	})
	mut predicate_tc := types.TypeChecker.new(&predicate_ast)
	predicate_tc.interface_names['View'] = true
	mut predicate_transformer := new_transformer(mut predicate_ast, &predicate_tc, map[string]bool{})
	assert predicate_transformer.fn_span_map_expansion_estimate(0, int(predicate) + 1) > deferred_map_expansion_threshold

	mut selector_ast := flat.FlatAst.new()
	selector_base := selector_ast.add_node(flat.Node{
		kind: .ident
		value: 'view'
		typ: 'View'
	})
	selector_start := selector_ast.children.len
	selector_ast.children << selector_base
	selector := selector_ast.add_node(flat.Node{
		kind: .selector
		value: 'value'
		typ: 'int'
		children_start: selector_start
		children_count: 1
	})
	mut selector_tc := types.TypeChecker.new(&selector_ast)
	selector_tc.interface_names['View'] = true
	selector_tc.interface_fields['View'] = [types.StructField{
		name: 'value'
		typ: types.Type(types.int_)
	}]
	mut selector_transformer := new_transformer(mut selector_ast, &selector_tc, map[string]bool{})
	assert selector_transformer.fn_span_map_expansion_estimate(0, int(selector) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_ownership_collection_clones() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[]Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'reverse'
		typ: 'fn () []Wide'
		children_start: selector_start
		children_count: 1
	})
	call_start := a.children.len
	a.children << selector
	reverse_call := a.add_node(flat.Node{
		kind: .call
		typ: '[]Wide'
		children_start: call_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(0, int(reverse_call) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_ownership_for_in_binding_clones() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	index := a.add_node(flat.Node{
		kind: .ident
		value: '_'
		typ: 'int'
	})
	item := a.add_node(flat.Node{
		kind: .ident
		value: 'item'
		typ: 'Wide'
	})
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[]Wide'
	})
	loop_start := a.children.len
	a.children << index
	a.children << item
	a.children << items
	loop := a.add_node(flat.Node{
		kind: .for_in_stmt
		value: '3'
		children_start: loop_start
		children_count: 3
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_for_in_binding_clone_expands(a.nodes[int(loop)])
	assert t.fn_span_map_expansion_estimate(0, int(loop) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_owned_map_delete_snapshot_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	key := a.add_node(flat.Node{
		kind: .ident
		value: 'key'
		typ: 'string'
	})
	ignored_value := a.add_node(flat.Node{
		kind: .ident
		value: '_'
		typ: 'Wide'
	})
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[string]Wide'
	})
	selector_start := a.children.len
	a.children << items
	delete_selector := a.add_node(flat.Node{
		kind: .selector
		value: 'delete'
		children_start: selector_start
		children_count: 1
	})
	call_start := a.children.len
	a.children << delete_selector
	a.children << key
	delete_call := a.add_node(flat.Node{
		kind: .call
		children_start: call_start
		children_count: 2
	})
	loop_start := a.children.len
	a.children << key
	a.children << ignored_value
	a.children << items
	a.children << delete_call
	loop := a.add_node(flat.Node{
		kind: .for_in_stmt
		value: '3'
		children_start: loop_start
		children_count: 4
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert !t.ownership_for_in_binding_clone_expands(a.nodes[int(loop)])
	assert t.ownership_for_in_map_snapshot_clone_expands(a.nodes[int(loop)])
	assert t.fn_span_map_expansion_estimate(0, int(loop) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_nested_map_delete_key_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[Wide]map[string]int'
	})
	outer_key := a.add_node(flat.Node{
		kind: .ident
		value: 'outer_key'
		typ: 'Wide'
	})
	outer_index_start := a.children.len
	a.children << items
	a.children << outer_key
	outer_index := a.add_node(flat.Node{
		kind: .index
		typ: 'map[string]int'
		children_start: outer_index_start
		children_count: 2
	})
	selector_start := a.children.len
	a.children << outer_index
	delete_selector := a.add_node(flat.Node{
		kind: .selector
		value: 'delete'
		children_start: selector_start
		children_count: 1
	})
	inner_key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'inner'
		typ: 'string'
	})
	call_start := a.children.len
	a.children << delete_selector
	a.children << inner_key
	delete_call := a.add_node(flat.Node{
		kind: .call
		children_start: call_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_nested_map_delete_key_clone_expands(a.nodes[int(delete_call)])
	assert t.fn_span_map_expansion_estimate(0, int(delete_call) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_overlapping_owned_map_assignment() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[string]Wide'
	})
	left_key := a.add_node(flat.Node{
		kind: .ident
		value: 'left_key'
		typ: 'string'
	})
	right_key := a.add_node(flat.Node{
		kind: .ident
		value: 'right_key'
		typ: 'string'
	})
	lhs_start := a.children.len
	a.children << items
	a.children << left_key
	lhs := a.add_node(flat.Node{
		kind: .index
		typ: 'Wide'
		children_start: lhs_start
		children_count: 2
	})
	rhs_start := a.children.len
	a.children << items
	a.children << right_key
	rhs := a.add_node(flat.Node{
		kind: .index
		typ: 'Wide'
		children_start: rhs_start
		children_count: 2
	})
	assignment_start := a.children.len
	a.children << lhs
	a.children << rhs
	assignment := a.add_node(flat.Node{
		kind: .index_assign
		op: .assign
		children_start: assignment_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_map_assignment_clone_expands(a.nodes[int(assignment)])
	assert t.fn_span_map_expansion_estimate(0, int(assignment) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_borrowed_owned_map_key_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[Wide]int'
	})
	key := a.add_node(flat.Node{
		kind: .ident
		value: 'key'
		typ: 'Wide'
	})
	lhs_start := a.children.len
	a.children << items
	a.children << key
	lhs := a.add_node(flat.Node{
		kind: .index
		typ: 'int'
		children_start: lhs_start
		children_count: 2
	})
	value := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	assignment_start := a.children.len
	a.children << lhs
	a.children << value
	assignment := a.add_node(flat.Node{
		kind: .index_assign
		op: .assign
		children_start: assignment_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_map_assignment_clone_expands(a.nodes[int(assignment)])
	assert t.fn_span_map_expansion_estimate(0, int(assignment) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_nested_map_lvalue_key_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[Wide]Entry'
	})
	key := a.add_node(flat.Node{
		kind: .ident
		value: 'key'
		typ: 'Wide'
	})
	index_start := a.children.len
	a.children << items
	a.children << key
	index := a.add_node(flat.Node{
		kind: .index
		typ: 'Entry'
		children_start: index_start
		children_count: 2
	})
	selector_start := a.children.len
	a.children << index
	field := a.add_node(flat.Node{
		kind: .selector
		value: 'value'
		typ: 'int'
		children_start: selector_start
		children_count: 1
	})
	rhs := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	assignment_start := a.children.len
	a.children << field
	a.children << rhs
	assignment := a.add_node(flat.Node{
		kind: .selector_assign
		op: .assign
		children_start: assignment_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_map_assignment_clone_expands(a.nodes[int(assignment)])
	assert t.fn_span_map_expansion_estimate(0, int(assignment) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_outer_nested_map_assignment_key_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: 'map[Wide]map[string]int'
	})
	outer_key := a.add_node(flat.Node{
		kind: .ident
		value: 'outer_key'
		typ: 'Wide'
	})
	outer_start := a.children.len
	a.children << items
	a.children << outer_key
	outer_index := a.add_node(flat.Node{
		kind: .index
		typ: 'map[string]int'
		children_start: outer_start
		children_count: 2
	})
	inner_key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'inner'
		typ: 'string'
	})
	inner_start := a.children.len
	a.children << outer_index
	a.children << inner_key
	inner_index := a.add_node(flat.Node{
		kind: .index
		typ: 'int'
		children_start: inner_start
		children_count: 2
	})
	rhs := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	assignment_start := a.children.len
	a.children << inner_index
	a.children << rhs
	assignment := a.add_node(flat.Node{
		kind: .index_assign
		op: .assign
		children_start: assignment_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.map_assignment_lvalue_key_clone_expands(inner_index)
	assert t.ownership_map_assignment_clone_expands(a.nodes[int(assignment)])
	assert t.fn_span_map_expansion_estimate(0, int(assignment) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_owned_method_value_receiver_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'wide'
		typ: 'Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	method_value := a.add_node(flat.Node{
		kind: .selector
		value: 'consume'
		typ: 'fn () int'
		children_start: selector_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	wide_type := tc.parse_type('Wide')
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	tc.fn_param_types['Wide.consume'] = [wide_type]
	tc.fn_ret_types['Wide.consume'] = tc.parse_type('int')
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_method_value_clone_expands(method_value, a.nodes[int(method_value)])
	assert t.fn_span_map_expansion_estimate(0, int(method_value) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_reflected_comptime_loops() {
	for kind in ['fields', 'values', 'variants', 'methods', 'params', 'attributes'] {
		mut a := flat.FlatAst.new()
		comptime_loop := a.add_node(flat.Node{
			kind: .comptime_for
			value: 'item|${kind}'
			typ: 'Wide'
		})
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		assert t.fn_span_map_expansion_estimate(0, int(comptime_loop) + 1) > deferred_map_expansion_threshold
	}
}

fn test_fn_span_map_expansion_estimate_defers_ownership_array_append_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[]Wide'
	})
	borrowed := a.add_node(flat.Node{
		kind: .ident
		value: 'borrowed'
		typ: 'Wide'
	})
	append_start := a.children.len
	a.children << items
	a.children << borrowed
	append := a.add_node(flat.Node{
		kind: .infix
		op: .left_shift
		typ: '[]Wide'
		children_start: append_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_array_append_expands(a.nodes[int(append)])
	assert t.fn_span_map_expansion_estimate(0, int(append) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_map_expansion_estimate_defers_ownership_map_index_append_clone() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	values := a.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: 'map[string][]Wide'
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'key'
		typ: 'string'
	})
	index_start := a.children.len
	a.children << values
	a.children << key
	index := a.add_node(flat.Node{
		kind: .index
		typ: '[]Wide'
		children_start: index_start
		children_count: 2
	})
	item := a.add_node(flat.Node{
		kind: .ident
		value: 'item'
		typ: 'Wide'
	})
	append_start := a.children.len
	a.children << index
	a.children << item
	append := a.add_node(flat.Node{
		kind: .infix
		op: .left_shift
		typ: '[]Wide'
		children_start: append_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_array_append_expands(a.nodes[int(append)])
	assert t.fn_span_map_expansion_estimate(0, int(append) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_expansion_defers_builtin_auto_stringification() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind: .ident
		value: 'println'
	})
	wide := a.add_node(flat.Node{
		kind: .ident
		value: 'wide'
		typ: 'Wide'
	})
	call_start := a.children.len
	a.children << callee
	a.children << wide
	call := a.add_node(flat.Node{
		kind: .call
		children_start: call_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Wide'] = StructInfo{
		name: 'Wide'
		fields: [FieldInfo{
			name: 'value'
			typ: 'int'
		}]
	}

	assert t.builtin_call_auto_stringify_expands(call, a.nodes[int(call)])
	assert t.fn_span_map_expansion_estimate(0, int(call) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_expansion_defers_dump_auto_stringification() {
	mut a := flat.FlatAst.new()
	wide := a.add_node(flat.Node{
		kind: .ident
		value: 'wide'
		typ: 'Wide'
	})
	dump_start := a.children.len
	a.children << wide
	dump_expr := a.add_node(flat.Node{
		kind: .dump_expr
		value: 'wide'
		typ: 'Wide'
		children_start: dump_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.fn_span_map_expansion_estimate(0, int(dump_expr) + 1) > deferred_map_expansion_threshold
}

fn test_fn_span_expansion_defers_direct_aggregate_membership() {
	mut a := flat.FlatAst.new()
	needle := a.add_node(flat.Node{
		kind: .ident
		value: 'needle'
		typ: 'Wide'
	})
	items := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[]Wide'
	})
	membership_start := a.children.len
	a.children << needle
	a.children << items
	membership := a.add_node(flat.Node{
		kind: .in_expr
		typ: 'bool'
		children_start: membership_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Wide'] = StructInfo{
		name: 'Wide'
	}

	assert t.array_membership_equality_expands(a.nodes[int(membership)])
	assert t.fn_span_map_expansion_estimate(0, int(membership) + 1) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_dynamic_array_struct_defaults() {
	mut a := flat.FlatAst.new()
	length := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	field_start := a.children.len
	a.children << length
	len_field := a.add_node(flat.Node{
		kind: .field_init
		value: 'len'
		children_start: field_start
		children_count: 1
	})
	array_start := a.children.len
	a.children << len_field
	root := a.add_node(flat.Node{
		kind: .array_init
		value: 'Wide'
		typ: '[]Wide'
		children_start: array_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Wide'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_dynamic_array_explicit_init() {
	mut a := flat.FlatAst.new()
	length := a.add_node(flat.Node{
		kind: .int_literal
		value: '4'
		typ: 'int'
	})
	len_field_start := a.children.len
	a.children << length
	len_field := a.add_node(flat.Node{
		kind: .field_init
		value: 'len'
		children_start: len_field_start
		children_count: 1
	})
	initial := a.add_node(flat.Node{
		kind: .ident
		value: 'index'
		typ: 'int'
	})
	init_field_start := a.children.len
	a.children << initial
	init_field := a.add_node(flat.Node{
		kind: .field_init
		value: 'init'
		children_start: init_field_start
		children_count: 1
	})
	array_start := a.children.len
	a.children << len_field
	a.children << init_field
	root := a.add_node(flat.Node{
		kind: .array_init
		value: 'int'
		typ: '[]int'
		children_start: array_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_nested_empty_fixed_array_runtime_init() {
	mut a := flat.FlatAst.new()
	root := a.add_node(flat.Node{
		kind: .array_init
		typ: '[64][64][]int'
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_string_interpolation() {
	mut a := flat.FlatAst.new()
	wide_value := a.add_node(flat.Node{
		kind: .ident
		value: 'wide'
		typ: 'Wide'
	})
	interp_start := a.children.len
	a.children << wide_value
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'wide'
	})
	map_start := a.children.len
	a.children << key
	a.children << interp
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]string'
		children_start: map_start
		children_count: 2
	})
	mut fields := []FieldInfo{cap: 256}
	for i in 0 .. 256 {
		fields << FieldInfo{
			name: 'value_${i}'
			typ: 'int'
		}
	}
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Wide'] = StructInfo{
		name: 'Wide'
		fields: fields
	}

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_small_aggregate_interpolation() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'Small'
	})
	interp_start := a.children.len
	a.children << value
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << interp
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]string'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Small'] = StructInfo{
		name: 'Small'
		fields: [
			FieldInfo{
				name: 'number'
				typ: 'int'
			},
		]
	}

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_or_lowering() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind: .ident
		value: 'decode'
	})
	call_start := a.children.len
	a.children << callee
	call := a.add_node(flat.Node{
		kind: .call
		typ: '!int'
		children_start: call_start
		children_count: 1
	})
	fallback_value := a.add_node(flat.Node{
		kind: .int_literal
		value: '0'
		typ: 'int'
	})
	fallback_start := a.children.len
	a.children << fallback_value
	fallback := a.add_node(flat.Node{
		kind: .block
		children_start: fallback_start
		children_count: 1
	})
	or_start := a.children.len
	a.children << call
	a.children << fallback
	or_expr := a.add_node(flat.Node{
		kind: .or_expr
		typ: 'int'
		children_start: or_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << or_expr
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_dump_lowering() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	inner_start := a.children.len
	a.children << value
	inner := a.add_node(flat.Node{
		kind: .dump_expr
		value: '1'
		typ: 'int'
		children_start: inner_start
		children_count: 1
	})
	outer_start := a.children.len
	a.children << inner
	outer := a.add_node(flat.Node{
		kind: .dump_expr
		value: 'dump(1)'
		typ: 'int'
		children_start: outer_start
		children_count: 1
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << outer
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_membership_lowering() {
	mut a := flat.FlatAst.new()
	needle := a.add_node(flat.Node{
		kind: .ident
		value: 'needle'
		typ: 'Wide'
	})
	haystack := a.add_node(flat.Node{
		kind: .ident
		value: 'haystack'
		typ: '[]Wide'
	})
	membership_start := a.children.len
	a.children << needle
	a.children << haystack
	root := a.add_node(flat.Node{
		kind: .in_expr
		typ: 'bool'
		children_start: membership_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn external_map_metadata_expr_expansion_estimate(kind flat.NodeKind) int {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'source'
	})
	expr_start := a.children.len
	a.children << value
	expr := a.add_node(flat.Node{
		kind: kind
		typ: 'Target'
		children_start: expr_start
		children_count: 1
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << expr
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]bool'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	return t.external_map_tree_expansion_estimate(root, 0, 0)
}

fn test_external_map_expansion_estimate_defers_interface_metadata_lowering() {
	assert external_map_metadata_expr_expansion_estimate(.is_expr) > deferred_map_expansion_threshold
	assert external_map_metadata_expr_expansion_estimate(.as_expr) > deferred_map_expansion_threshold
}

fn add_runtime_metadata_call(mut a flat.FlatAst, base_name string, base_type string, method string, result_type string) flat.NodeId {
	base := a.add_node(flat.Node{
		kind: .ident
		value: base_name
		typ: base_type
	})
	selector_start := a.children.len
	a.children << base
	selector := a.add_node(flat.Node{
		kind: .selector
		value: method
		typ: 'fn () ${result_type}'
		children_start: selector_start
		children_count: 1
	})
	call_start := a.children.len
	a.children << selector
	return a.add_node(flat.Node{
		kind: .call
		typ: result_type
		children_start: call_start
		children_count: 1
	})
}

fn test_external_map_expansion_estimate_defers_runtime_type_metadata_calls() {
	mut a := flat.FlatAst.new()
	type_idx_call := add_runtime_metadata_call(mut a, 'item', 'Item', 'type_idx', 'int')
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << type_idx_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Item'] = ['First', 'Second']

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_enum_from_string_calls() {
	mut a := flat.FlatAst.new()
	enum_type := a.add_node(flat.Node{
		kind: .ident
		value: 'Wide'
		typ: 'Wide'
	})
	selector_start := a.children.len
	a.children << enum_type
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'from_string'
		typ: 'fn (string) ?Wide'
		children_start: selector_start
		children_count: 1
	})
	argument := a.add_node(flat.Node{
		kind: .string_literal
		value: 'v0'
		typ: 'string'
	})
	call_start := a.children.len
	a.children << selector
	a.children << argument
	call := a.add_node(flat.Node{
		kind: .call
		typ: '?Wide'
		children_start: call_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]?Wide'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.enum_types['Wide'] = ['v0', 'v1']

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn external_map_equality_expansion_estimate(operand_type string, metadata_type string, is_interface bool) int {
	mut a := flat.FlatAst.new()
	mut operands := []flat.NodeId{}
	for name in ['left', 'right'] {
		callee := a.add_node(flat.Node{
			kind: .ident
			value: name
		})
		call_start := a.children.len
		a.children << callee
		operands << a.add_node(flat.Node{
			kind: .call
			typ: operand_type
			children_start: call_start
			children_count: 1
		})
	}
	equality_start := a.children.len
	a.children << operands[0]
	a.children << operands[1]
	equality := a.add_node(flat.Node{
		kind: .infix
		op: .eq
		typ: 'bool'
		children_start: equality_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << equality
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]bool'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	if is_interface {
		tc.interface_names[metadata_type] = true
	}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	if !is_interface {
		t.structs[metadata_type] = StructInfo{
			name: metadata_type
		}
	}
	return t.external_map_tree_expansion_estimate(root, 0, 0)
}

fn test_external_map_expansion_estimate_defers_metadata_driven_equality() {
	assert external_map_equality_expansion_estimate('WideRecord', 'WideRecord', false) > deferred_map_expansion_threshold
	assert external_map_equality_expansion_estimate('Value', 'Value', true) > deferred_map_expansion_threshold
	assert external_map_equality_expansion_estimate('[]WideRecord', 'WideRecord', false) > deferred_map_expansion_threshold
	assert external_map_equality_expansion_estimate('map[string]WideRecord', 'WideRecord', false) > deferred_map_expansion_threshold
}

fn test_map_fixed_array_equality_reserves_missing_value_zeroing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.equality_type_expands_from_metadata('map[string][4096][]int', 0)
}

fn test_external_map_expansion_estimate_includes_index_reconstruction() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind: .ident
		value: 'make_items'
	})
	call_start := a.children.len
	a.children << callee
	base := a.add_node(flat.Node{
		kind: .call
		children_start: call_start
		children_count: 1
	})
	index_value := a.add_node(flat.Node{
		kind: .ident
		value: 'i'
	})
	index_start := a.children.len
	a.children << base
	a.children << index_value
	root := a.add_node(flat.Node{
		kind: .index
		children_start: index_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) == 5
}

fn test_external_map_expansion_estimate_includes_range_reconstruction() {
	mut a := flat.FlatAst.new()
	mut value := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
	})
	for i in 0 .. deferred_map_expansion_threshold / 8 + 1 {
		low_callee := a.add_node(flat.Node{
			kind: .ident
			value: 'low_${i}'
		})
		low_start := a.children.len
		a.children << low_callee
		low := a.add_node(flat.Node{
			kind: .call
			children_start: low_start
			children_count: 1
		})
		high_callee := a.add_node(flat.Node{
			kind: .ident
			value: 'high_${i}'
		})
		high_start := a.children.len
		a.children << high_callee
		high := a.add_node(flat.Node{
			kind: .call
			children_start: high_start
			children_count: 1
		})
		range_start := a.children.len
		a.children << low
		a.children << high
		range := a.add_node(flat.Node{
			kind: .range
			children_start: range_start
			children_count: 2
		})
		index_start := a.children.len
		a.children << value
		a.children << range
		value = a.add_node(flat.Node{
			kind: .index
			children_start: index_start
			children_count: 2
		})
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << value
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string][]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_selector_reconstruction() {
	mut a := flat.FlatAst.new()
	callee := a.add_node(flat.Node{
		kind: .ident
		value: 'make_node'
	})
	call_start := a.children.len
	a.children << callee
	mut value := a.add_node(flat.Node{
		kind: .call
		children_start: call_start
		children_count: 1
	})
	for _ in 0 .. deferred_map_expansion_threshold / 2 + 1 {
		selector_start := a.children.len
		a.children << value
		value = a.add_node(flat.Node{
			kind: .selector
			value: 'next'
			children_start: selector_start
			children_count: 1
		})
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << value
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_sum_shared_field_selector() {
	mut a := flat.FlatAst.new()
	base := a.add_node(flat.Node{
		kind: .ident
		value: 'item'
		typ: 'Item'
	})
	selector_start := a.children.len
	a.children << base
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'value'
		typ: 'int'
		children_start: selector_start
		children_count: 1
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'item'
	})
	map_start := a.children.len
	a.children << key
	a.children << selector
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Item'] = ['First', 'Second']
	t.structs['First'] = StructInfo{
		name: 'First'
		fields: [
			FieldInfo{
				name: 'value'
				typ: 'int'
			},
		]
	}
	t.structs['Second'] = StructInfo{
		name: 'Second'
		fields: [
			FieldInfo{
				name: 'value'
				typ: 'int'
			},
		]
	}

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_interpolation_concatenation() {
	mut a := flat.FlatAst.new()
	part := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
	})
	part_count := deferred_map_expansion_threshold / 2 + 2
	interp_start := a.children.len
	for _ in 0 .. part_count {
		a.children << part
	}
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: flat.child_count(part_count)
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'wide'
	})
	map_start := a.children.len
	a.children << key
	a.children << interp
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]string'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_string_interp_expansion_estimate_includes_possible_temp_hoisting() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
		typ: 'string'
	})
	base := a.add_node(flat.Node{
		kind: .ident
		value: 'values'
		typ: 'map[string]string'
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'key'
		typ: 'string'
	})
	index_start := a.children.len
	a.children << base
	a.children << key
	index := a.add_node(flat.Node{
		kind: .index
		typ: 'string'
		children_start: index_start
		children_count: 2
	})
	part_count := 600
	interp_start := a.children.len
	for _ in 0 .. part_count - 1 {
		a.children << literal
	}
	a.children << index
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: flat.child_count(part_count)
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	join_estimate := 3 * (part_count - 1)
	assert join_estimate < deferred_map_expansion_threshold
	assert estimate == join_estimate + part_count * string_interp_hoisted_part_expansion_estimate
	assert estimate > deferred_map_expansion_threshold
	assert !needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_includes_array_ident_hoisting() {
	for array_type in ['[]int', '[4]int'] {
		mut a := flat.FlatAst.new()
		items := a.add_node(flat.Node{
			kind: .ident
			value: 'items'
			typ: array_type
		})
		interp_start := a.children.len
		a.children << items
		interp := a.add_node(flat.Node{
			kind: .string_interp
			typ: 'string'
			children_start: interp_start
			children_count: 1
		})
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
		assert estimate >= string_interp_hoisted_part_expansion_estimate
		assert needs_deferred_lowering
	}
}

fn test_reflected_comptime_for_interpolation_defers_with_bounded_join_estimate() {
	mut a := flat.FlatAst.new()
	comptime_loop := a.add_node(flat.Node{
		kind: .comptime_for
	})
	literal := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
		typ: 'string'
	})
	metadata := a.add_node(flat.Node{
		kind: .ident
		value: 'field_name'
		typ: 'string'
	})
	interp_start := a.children.len
	a.children << literal
	a.children << metadata
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	part_estimate, part_needs_deferred := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert part_estimate == 3
	assert !part_needs_deferred
	span_estimate, span_needs_deferred := t.fn_span_interp_estimate(int(comptime_loop), int(interp) + 1)
	assert span_estimate == part_estimate
	assert span_needs_deferred
}

fn test_string_interp_expansion_estimate_includes_container_cast_hoisting() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
		typ: 'string'
	})
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'Any'
	})
	cast_start := a.children.len
	a.children << value
	cast := a.add_node(flat.Node{
		kind: .as_expr
		value: '[]int'
		typ: '[]int'
		children_start: cast_start
		children_count: 1
	})
	interp_start := a.children.len
	a.children << literal
	a.children << cast
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate, _ := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert t.string_interp_expr_may_hoist(cast)
	assert estimate >= 3 + 2 * string_interp_hoisted_part_expansion_estimate
}

fn test_string_interp_expansion_estimate_defers_interface_conversion_hoisting() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'Source'
	})
	cast_start := a.children.len
	a.children << value
	cast := a.add_node(flat.Node{
		kind: .as_expr
		value: 'Target'
		typ: 'Target'
		children_start: cast_start
		children_count: 1
	})
	interp_start := a.children.len
	a.children << cast
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Source'] = true
	tc.interface_names['Target'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.string_interp_expr_needs_deferred_lowering(cast)
	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == string_interp_hoisted_part_expansion_estimate
	assert needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_defers_metadata_driven_selectors() {
	mut a := flat.FlatAst.new()
	interface_base := a.add_node(flat.Node{
		kind: .ident
		value: 'view'
		typ: 'View'
	})
	interface_selector_start := a.children.len
	a.children << interface_base
	interface_selector := a.add_node(flat.Node{
		kind: .selector
		value: 'value'
		typ: 'int'
		children_start: interface_selector_start
		children_count: 1
	})
	sum_base := a.add_node(flat.Node{
		kind: .ident
		value: 'item'
		typ: 'Item'
	})
	sum_selector_start := a.children.len
	a.children << sum_base
	sum_selector := a.add_node(flat.Node{
		kind: .selector
		value: 'value'
		typ: 'int'
		children_start: sum_selector_start
		children_count: 1
	})
	interp_start := a.children.len
	a.children << interface_selector
	a.children << sum_selector
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['View'] = true
	tc.interface_fields['View'] = [
		types.StructField{
			name: 'value'
			typ: types.Type(types.int_)
		},
	]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Item'] = ['First', 'Second']
	for variant in ['First', 'Second'] {
		t.structs[variant] = StructInfo{
			name: variant
			fields: [
				FieldInfo{
					name: 'value'
					typ: 'int'
				},
			]
		}
	}

	assert t.string_interp_expr_needs_deferred_lowering(interface_selector)
	assert t.string_interp_expr_needs_deferred_lowering(sum_selector)
	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == 3 + 2 * string_interp_hoisted_part_expansion_estimate
	assert needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_defers_metadata_driven_predicates() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'Value'
	})
	is_start := a.children.len
	a.children << value
	is_expr := a.add_node(flat.Node{
		kind: .is_expr
		value: 'Target'
		typ: 'bool'
		children_start: is_start
		children_count: 1
	})
	left := a.add_node(flat.Node{
		kind: .ident
		value: 'left'
		typ: 'WideRecord'
	})
	right := a.add_node(flat.Node{
		kind: .ident
		value: 'right'
		typ: 'WideRecord'
	})
	equality_start := a.children.len
	a.children << left
	a.children << right
	equality := a.add_node(flat.Node{
		kind: .infix
		op: .eq
		typ: 'bool'
		children_start: equality_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['Value'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['WideRecord'] = StructInfo{
		name: 'WideRecord'
	}

	assert t.string_interp_expr_needs_deferred_lowering(is_expr)
	assert t.string_interp_expr_needs_deferred_lowering(equality)
}

fn test_string_interp_expansion_estimate_defers_runtime_type_metadata_calls() {
	mut a := flat.FlatAst.new()
	sum_call := add_runtime_metadata_call(mut a, 'item', 'Item', 'type_name', 'string')
	interface_call := add_runtime_metadata_call(mut a, 'view', 'View', 'type_idx', 'int')
	interp_start := a.children.len
	a.children << sum_call
	a.children << interface_call
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['View'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Item'] = ['First', 'Second']

	assert t.string_interp_expr_needs_deferred_lowering(sum_call)
	assert t.string_interp_expr_needs_deferred_lowering(interface_call)
	_, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_defers_compiler_clone_calls() {
	for clone_type in ['Wide', '[]Wide', '[4]Wide', 'map[string]Wide'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_node(flat.Node{
			kind: .ident
			value: 'value'
			typ: clone_type
		})
		selector_start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind: .selector
			value: 'clone'
			typ: 'fn () ${clone_type}'
			children_start: selector_start
			children_count: 1
		})
		call_start := a.children.len
		a.children << selector
		clone_call := a.add_node(flat.Node{
			kind: .call
			typ: clone_type
			children_start: call_start
			children_count: 1
		})
		interp_start := a.children.len
		a.children << clone_call
		interp := a.add_node(flat.Node{
			kind: .string_interp
			typ: 'string'
			children_start: interp_start
			children_count: 1
		})
		mut tc := types.TypeChecker.new(&a)
		tc.structs['Wide'] = []
		tc.struct_implements['Wide'] = ['IClone']
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		assert t.string_interp_expr_needs_deferred_lowering(clone_call)
		_, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
		assert needs_deferred_lowering
	}
}

fn test_string_interp_expansion_estimate_includes_shared_ident_hoisting() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
		typ: 'string'
	})
	shared_value := a.add_node(flat.Node{
		kind: .ident
		value: 'counter'
		typ: 'int'
	})
	interp_start := a.children.len
	a.children << literal
	a.children << shared_value
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('counter', 'shared int')

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == 3 + 2 * string_interp_hoisted_part_expansion_estimate
	assert !needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_defers_optional_ident_hoisting() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
		typ: 'string'
	})
	optional_value := a.add_node(flat.Node{
		kind: .ident
		value: 'maybe_value'
		typ: '?int'
	})
	interp_start := a.children.len
	a.children << literal
	a.children << optional_value
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert t.string_interp_expr_may_hoist(optional_value)
	assert estimate == 3 + 2 * string_interp_hoisted_part_expansion_estimate
	assert needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_includes_shared_param_hoisting() {
	mut a := flat.FlatAst.new()
	param := a.add_node(flat.Node{
		kind: .param
		value: 'counter'
		typ: 'shared int'
	})
	literal := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
		typ: 'string'
	})
	shared_value := a.add_node(flat.Node{
		kind: .ident
		value: 'counter'
		typ: 'int'
	})
	interp_start := a.children.len
	a.children << literal
	a.children << shared_value
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	block_start := a.children.len
	a.children << interp
	block := a.add_node(flat.Node{
		kind: .block
		children_start: block_start
		children_count: 1
	})
	fn_start := a.children.len
	a.children << param
	a.children << block
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'show'
		children_start: fn_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == 3 + 2 * string_interp_hoisted_part_expansion_estimate
	assert !needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_ignores_stale_shared_binding() {
	mut a := flat.FlatAst.new()
	stale_lhs := a.add_node(flat.Node{
		kind: .ident
		value: 'counter'
	})
	stale_rhs := a.add_node(flat.Node{
		kind: .int_literal
		value: '0'
		typ: 'int'
	})
	stale_decl_start := a.children.len
	a.children << stale_lhs
	a.children << stale_rhs
	stale_decl := a.add_node(flat.Node{
		kind: .decl_assign
		value: 'shared'
		children_start: stale_decl_start
		children_count: 2
	})
	stale_fn_start := a.children.len
	a.children << stale_decl
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'stale'
		children_start: stale_fn_start
		children_count: 1
	})

	param := a.add_node(flat.Node{
		kind: .param
		value: 'counter'
		typ: 'int'
	})
	literal := a.add_node(flat.Node{
		kind: .string_literal
		value: 'part'
		typ: 'string'
	})
	plain_value := a.add_node(flat.Node{
		kind: .ident
		value: 'counter'
		typ: 'int'
	})
	interp_start := a.children.len
	a.children << literal
	a.children << plain_value
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 2
	})
	block_start := a.children.len
	a.children << interp
	block := a.add_node(flat.Node{
		kind: .block
		children_start: block_start
		children_count: 1
	})
	current_fn_start := a.children.len
	a.children << param
	a.children << block
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'current'
		children_start: current_fn_start
		children_count: 2
	})

	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_source_parent_index()
	t.set_var_type('counter', 'shared int')

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == 3
	assert !needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_defers_unresolved_values() {
	mut a := flat.FlatAst.new()
	unresolved := a.add_node(flat.Node{
		kind: .ident
		value: 'unknown_value'
	})
	interp_start := a.children.len
	a.children << unresolved
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == unresolved_interp_expansion_estimate
	assert needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_defers_nested_literal_interpolation() {
	for value in [
		'prefix \${if true { "yes" } else { "no" }}',
		'prefix \${match 1 { 1 { "yes" } else { "no" } }}',
	] {
		mut a := flat.FlatAst.new()
		nested := a.add_node(flat.Node{
			kind: .string_literal
			value: value
			typ: 'string'
		})
		interp_start := a.children.len
		a.children << nested
		interp := a.add_node(flat.Node{
			kind: .string_interp
			typ: 'string'
			children_start: interp_start
			children_count: 1
		})
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		_, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
		assert t.string_interp_expr_needs_deferred_lowering(nested)
		assert needs_deferred_lowering
	}
}

fn test_string_interp_expansion_estimate_defers_interface_auto_stringification() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'View'
	})
	interp_start := a.children.len
	a.children << value
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['View'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.string_interp_expr_needs_deferred_lowering(value)
	_, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert needs_deferred_lowering
}

fn test_string_interp_expansion_estimate_defers_typed_map_stringification() {
	for typ in ['map[string]?int', 'map[string][]int', 'map[string]&int'] {
		mut a := flat.FlatAst.new()
		value := a.add_node(flat.Node{
			kind: .ident
			value: 'value'
			typ: typ
		})
		interp_start := a.children.len
		a.children << value
		interp := a.add_node(flat.Node{
			kind: .string_interp
			typ: 'string'
			children_start: interp_start
			children_count: 1
		})
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		assert t.string_interp_expr_needs_deferred_lowering(value), typ
		_, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
		assert needs_deferred_lowering, typ
	}
}

fn test_pointer_formatted_interp_skips_aggregate_stringify_expansion() {
	mut a := flat.FlatAst.new()
	pointer := a.add_node(flat.Node{
		kind: .ident
		value: 'pointer'
		typ: '&Large'
	})
	format_start := a.children.len
	a.children << pointer
	formatted := a.add_node(flat.Node{
		kind: .directive
		value: 'string_interp_format'
		typ: 'p'
		children_start: format_start
		children_count: 1
	})
	interp_start := a.children.len
	a.children << formatted
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Large'] = StructInfo{
		name: 'Large'
		fields: [
			FieldInfo{
				name: 'value'
				typ: 'string'
			},
		]
	}

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == 0
	assert !needs_deferred_lowering
}

fn test_pointer_formatted_interp_still_accounts_for_temp_hoisting() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'Any'
	})
	cast_start := a.children.len
	a.children << value
	cast := a.add_node(flat.Node{
		kind: .as_expr
		value: '[]int'
		typ: '[]int'
		children_start: cast_start
		children_count: 1
	})
	format_start := a.children.len
	a.children << cast
	formatted := a.add_node(flat.Node{
		kind: .directive
		value: 'string_interp_format'
		typ: 'p'
		children_start: format_start
		children_count: 1
	})
	interp_start := a.children.len
	a.children << formatted
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert estimate == string_interp_hoisted_part_expansion_estimate
	assert !needs_deferred_lowering
}

fn test_pointer_formatted_interp_accounts_for_pointer_to_sum_cast_hoisting() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind: .ident
		value: 'value'
		typ: 'First'
	})
	cast_start := a.children.len
	a.children << value
	cast := a.add_node(flat.Node{
		kind: .cast_expr
		value: '&Item'
		typ: '&Item'
		children_start: cast_start
		children_count: 1
	})
	format_start := a.children.len
	a.children << cast
	formatted := a.add_node(flat.Node{
		kind: .directive
		value: 'string_interp_format'
		typ: 'p'
		children_start: format_start
		children_count: 1
	})
	interp_start := a.children.len
	a.children << formatted
	interp := a.add_node(flat.Node{
		kind: .string_interp
		typ: 'string'
		children_start: interp_start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Item'] = ['First', 'Second']

	estimate, needs_deferred_lowering := t.string_interp_expansion_estimates(a.nodes[int(interp)])
	assert t.string_interp_expr_may_hoist(cast)
	assert estimate == string_interp_hoisted_part_expansion_estimate
	assert !needs_deferred_lowering
}

fn test_shared_map_expansion_is_bounded_in_aggregate() {
	mut t := Transformer{}
	items := [
		FnWorkItem{ fn_idx: 10, map_expansion_estimate: 300 },
		FnWorkItem{ fn_idx: 20, map_expansion_estimate: 0 },
		FnWorkItem{ fn_idx: 30, map_expansion_estimate: 300 },
		FnWorkItem{ fn_idx: 40, map_expansion_estimate: 300 },
	]

	bounded := t.bound_shared_expansion(items, 1600, 2000)
	assert bounded.map(it.fn_idx) == [10, 20, 30]
	assert t.deferred_expansion_items.map(it.fn_idx) == [40]
}

fn test_shared_interpolation_expansion_is_bounded_in_aggregate() {
	mut t := Transformer{}
	items := [
		FnWorkItem{ fn_idx: 10, interp_expansion_estimate: 300 },
		FnWorkItem{ fn_idx: 20, map_expansion_estimate: 100, interp_expansion_estimate: 200 },
		FnWorkItem{ fn_idx: 30, interp_expansion_estimate: 300 },
	]

	bounded := t.bound_shared_expansion(items, 1600, 2000)
	assert bounded.map(it.fn_idx) == [10, 20]
	assert t.deferred_expansion_items.map(it.fn_idx) == [30]
}

fn test_external_map_expansion_estimate_includes_string_concatenation() {
	mut a := flat.FlatAst.new()
	mut concat := a.add_node(flat.Node{
		kind: .string_literal
		value: 'a'
	})
	for _ in 0 .. deferred_map_expansion_threshold / external_string_infix_expansion_estimate + 1 {
		rhs := a.add_node(flat.Node{
			kind: .string_literal
			value: 'b'
		})
		infix_start := a.children.len
		a.children << concat
		a.children << rhs
		concat = a.add_node(flat.Node{
			kind: .infix
			op: .plus
			typ: 'string'
			children_start: infix_start
			children_count: 2
		})
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << concat
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]string'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_string_comparisons() {
	mut a := flat.FlatAst.new()
	lhs := a.add_node(flat.Node{
		kind: .string_literal
		value: 'a'
	})
	rhs := a.add_node(flat.Node{
		kind: .string_literal
		value: 'b'
	})
	mut comparison_tree := flat.empty_node
	for i in 0 .. deferred_map_expansion_threshold / external_string_infix_expansion_estimate + 1 {
		comparison_start := a.children.len
		a.children << lhs
		a.children << rhs
		comparison := a.add_node(flat.Node{
			kind: .infix
			op: .eq
			typ: 'bool'
			children_start: comparison_start
			children_count: 2
		})
		if i == 0 {
			comparison_tree = comparison
			continue
		}
		and_start := a.children.len
		a.children << comparison_tree
		a.children << comparison
		comparison_tree = a.add_node(flat.Node{
			kind: .infix
			op: .logical_and
			typ: 'bool'
			children_start: and_start
			children_count: 2
		})
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << comparison_tree
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]bool'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_logical_rewrites() {
	mut a := flat.FlatAst.new()
	mut logical_tree := a.add_node(flat.Node{
		kind: .bool_literal
		value: 'true'
	})
	for i in 0 .. deferred_map_expansion_threshold / 2 + 1 {
		rhs := a.add_node(flat.Node{
			kind: .bool_literal
			value: 'false'
		})
		infix_start := a.children.len
		a.children << logical_tree
		a.children << rhs
		logical_tree = a.add_node(flat.Node{
			kind: .infix
			op: if i % 2 == 0 { .logical_and } else { .logical_or }
			typ: 'bool'
			children_start: infix_start
			children_count: 2
		})
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << logical_tree
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]bool'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_call_reconstruction() {
	mut a := flat.FlatAst.new()
	mut level := []flat.NodeId{cap: 4096}
	for i in 0 .. 4096 {
		level << a.add_node(flat.Node{
			kind: .int_literal
			value: i.str()
			typ: 'int'
		})
	}
	for level.len > 1 {
		mut next := []flat.NodeId{cap: level.len / 2}
		for i := 0; i < level.len; i += 2 {
			callee := a.add_node(flat.Node{
				kind: .ident
				value: 'combine'
			})
			call_start := a.children.len
			a.children << callee
			a.children << level[i]
			a.children << level[i + 1]
			next << a.add_node(flat.Node{
				kind: .call
				typ: 'int'
				children_start: call_start
				children_count: 3
			})
		}
		level = next.clone()
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << level[0]
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_compiler_default_clone_calls() {
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'wide'
		typ: 'Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'clone'
		typ: 'fn () Wide'
		children_start: selector_start
		children_count: 1
	})
	call_start := a.children.len
	a.children << selector
	clone_call := a.add_node(flat.Node{
		kind: .call
		typ: 'Wide'
		children_start: call_start
		children_count: 1
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << clone_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]Wide'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Wide'] = []
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_ownership_collection_clone_calls() {
	for collection_type in ['[]Wide', '[4]Wide', 'map[string]Wide'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_node(flat.Node{
			kind: .ident
			value: 'items'
			typ: collection_type
		})
		selector_start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind: .selector
			value: 'clone'
			typ: 'fn () ${collection_type}'
			children_start: selector_start
			children_count: 1
		})
		call_start := a.children.len
		a.children << selector
		clone_call := a.add_node(flat.Node{
			kind: .call
			typ: collection_type
			children_start: call_start
			children_count: 1
		})
		key := a.add_node(flat.Node{
			kind: .string_literal
			value: 'value'
		})
		map_start := a.children.len
		a.children << key
		a.children << clone_call
		root := a.add_node(flat.Node{
			kind: .map_init
			typ: 'map[string]${collection_type}'
			children_start: map_start
			children_count: 2
		})
		mut tc := types.TypeChecker.new(&a)
		tc.structs['Wide'] = []
		tc.struct_implements['Wide'] = ['IClone']
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
	}
}

fn test_external_map_expansion_estimate_defers_ownership_array_reverse_calls() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[]Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'reverse'
		typ: 'fn () []Wide'
		children_start: selector_start
		children_count: 1
	})
	call_start := a.children.len
	a.children << selector
	reverse_call := a.add_node(flat.Node{
		kind: .call
		typ: '[]Wide'
		children_start: call_start
		children_count: 1
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << reverse_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string][]Wide'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.compiler_collection_clone_call_expands(a.nodes[int(reverse_call)])
	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_ownership_array_sorted_calls() {
	$if !ownership ? {
		return
	}
	for method in ['sorted', 'sorted_with_compare'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_node(flat.Node{
			kind: .ident
			value: 'items'
			typ: '[]Wide'
		})
		selector_start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind: .selector
			value: method
			typ: if method == 'sorted' { 'fn () []Wide' } else { 'fn (fn (Wide, Wide) int) []Wide' }
			children_start: selector_start
			children_count: 1
		})
		call_start := a.children.len
		a.children << selector
		mut call_children := 1
		if method == 'sorted_with_compare' {
			a.children << a.add_node(flat.Node{
				kind: .ident
				value: 'compare'
				typ: 'fn (Wide, Wide) int'
			})
			call_children++
		}
		sorted_call := a.add_node(flat.Node{
			kind: .call
			typ: '[]Wide'
			children_start: call_start
			children_count: u16(call_children)
		})
		key := a.add_node(flat.Node{
			kind: .string_literal
			value: method
		})
		map_start := a.children.len
		a.children << key
		a.children << sorted_call
		root := a.add_node(flat.Node{
			kind: .map_init
			typ: 'map[string][]Wide'
			children_start: map_start
			children_count: 2
		})
		mut tc := types.TypeChecker.new(&a)
		tc.collect(&a)
		tc.structs['Wide'] = [types.StructField{
			name: 'text'
			typ: tc.parse_type('string')
		}]
		tc.struct_implements['Wide'] = ['IClone']
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		assert t.compiler_collection_clone_call_expands(a.nodes[int(sorted_call)]), method
		assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold, method
	}
}

fn test_external_map_expansion_estimate_defers_owned_map_item_calls() {
	$if !ownership ? {
		return
	}
	for method in ['keys', 'values'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_node(flat.Node{
			kind: .ident
			value: 'items'
			typ: 'map[Wide]Wide'
		})
		selector_start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind: .selector
			value: method
			typ: 'fn () []Wide'
			children_start: selector_start
			children_count: 1
		})
		call_start := a.children.len
		a.children << selector
		items_call := a.add_node(flat.Node{
			kind: .call
			typ: '[]Wide'
			children_start: call_start
			children_count: 1
		})
		key := a.add_node(flat.Node{
			kind: .string_literal
			value: 'items'
		})
		map_start := a.children.len
		a.children << key
		a.children << items_call
		root := a.add_node(flat.Node{
			kind: .map_init
			typ: 'map[string][]Wide'
			children_start: map_start
			children_count: 2
		})
		mut tc := types.TypeChecker.new(&a)
		tc.collect(&a)
		tc.structs['Wide'] = [types.StructField{
			name: 'text'
			typ: tc.parse_type('string')
		}]
		tc.struct_implements['Wide'] = ['IClone']
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		assert t.compiler_owned_map_items_call_expands(a.nodes[int(items_call)]), method
		assert t.fn_span_map_expansion_estimate(0, int(items_call) + 1) > deferred_map_expansion_threshold, method
		assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold, method
	}
}

fn test_external_map_expansion_estimate_defers_array_equality_calls() {
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'left'
		typ: '[]Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'equals'
		typ: 'fn ([]Wide) bool'
		children_start: selector_start
		children_count: 1
	})
	right := a.add_node(flat.Node{
		kind: .ident
		value: 'right'
		typ: '[]Wide'
	})
	call_start := a.children.len
	a.children << selector
	a.children << right
	equals_call := a.add_node(flat.Node{
		kind: .call
		typ: 'bool'
		children_start: call_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'same'
	})
	map_start := a.children.len
	a.children << key
	a.children << equals_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]bool'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Wide'] = StructInfo{
		name: 'Wide'
	}

	assert t.compiler_array_search_call_expands(a.nodes[int(equals_call)])
	assert t.fn_span_map_expansion_estimate(0, int(equals_call) + 1) > deferred_map_expansion_threshold
	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn external_map_array_search_expansion_estimate(method string) int {
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[]Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: method
		typ: 'fn (Wide) int'
		children_start: selector_start
		children_count: 1
	})
	needle := a.add_node(flat.Node{
		kind: .ident
		value: 'needle'
		typ: 'Wide'
	})
	call_start := a.children.len
	a.children << selector
	a.children << needle
	search_call := a.add_node(flat.Node{
		kind: .call
		typ: if method == 'contains' { 'bool' } else { 'int' }
		children_start: call_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: method
	})
	map_start := a.children.len
	a.children << key
	a.children << search_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: if method == 'contains' { 'map[string]bool' } else { 'map[string]int' }
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['Wide'] = StructInfo{
		name: 'Wide'
	}
	assert t.compiler_array_search_call_expands(a.nodes[int(search_call)]), method
	assert t.fn_span_map_expansion_estimate(0, int(search_call) + 1) > deferred_map_expansion_threshold, method
	return t.external_map_tree_expansion_estimate(root, 0, 0)
}

fn test_external_map_expansion_estimate_defers_array_search_calls() {
	for method in ['contains', 'index', 'last_index'] {
		assert external_map_array_search_expansion_estimate(method) > deferred_map_expansion_threshold, method
	}
}

fn test_external_map_expansion_estimate_defers_owned_array_accessor_calls() {
	$if !ownership ? {
		return
	}
	for method in ['first', 'last'] {
		mut a := flat.FlatAst.new()
		receiver := a.add_node(flat.Node{
			kind: .ident
			value: 'items'
			typ: '[]Wide'
		})
		selector_start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind: .selector
			value: method
			typ: 'fn () Wide'
			children_start: selector_start
			children_count: 1
		})
		call_start := a.children.len
		a.children << selector
		accessor_call := a.add_node(flat.Node{
			kind: .call
			typ: 'Wide'
			children_start: call_start
			children_count: 1
		})
		key := a.add_node(flat.Node{
			kind: .string_literal
			value: 'item'
		})
		map_start := a.children.len
		a.children << key
		a.children << accessor_call
		root := a.add_node(flat.Node{
			kind: .map_init
			typ: 'map[string]Wide'
			children_start: map_start
			children_count: 2
		})
		mut tc := types.TypeChecker.new(&a)
		tc.collect(&a)
		tc.structs['Wide'] = [types.StructField{
			name: 'text'
			typ: tc.parse_type('string')
		}]
		tc.struct_implements['Wide'] = ['IClone']
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		assert t.compiler_owned_array_accessor_call_expands(a.nodes[int(accessor_call)]), method
		assert t.fn_span_map_expansion_estimate(0, int(accessor_call) + 1) > deferred_map_expansion_threshold, method
		assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold, method
	}
}

fn test_external_map_expansion_estimate_defers_owned_array_filter_calls() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	receiver := a.add_node(flat.Node{
		kind: .ident
		value: 'items'
		typ: '[]Wide'
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'filter'
		typ: 'fn (fn (Wide) bool) []Wide'
		children_start: selector_start
		children_count: 1
	})
	predicate := a.add_node(flat.Node{
		kind: .ident
		value: 'keep'
		typ: 'fn (Wide) bool'
	})
	call_start := a.children.len
	a.children << selector
	a.children << predicate
	filter_call := a.add_node(flat.Node{
		kind: .call
		typ: '[]Wide'
		children_start: call_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << filter_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string][]Wide'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.compiler_owned_array_filter_call_expands(a.nodes[int(filter_call)])
	assert t.fn_span_map_expansion_estimate(0, int(filter_call) + 1) > deferred_map_expansion_threshold
	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_owned_array_map_calls() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	make_items := a.add_node(flat.Node{
		kind: .ident
		value: 'make_items'
	})
	make_start := a.children.len
	a.children << make_items
	receiver := a.add_node(flat.Node{
		kind: .call
		typ: '[]Wide'
		children_start: make_start
		children_count: 1
	})
	selector_start := a.children.len
	a.children << receiver
	selector := a.add_node(flat.Node{
		kind: .selector
		value: 'map'
		typ: 'fn (fn (Wide) Wide) []Wide'
		children_start: selector_start
		children_count: 1
	})
	mapper := a.add_node(flat.Node{
		kind: .ident
		value: 'it'
		typ: 'Wide'
	})
	call_start := a.children.len
	a.children << selector
	a.children << mapper
	map_call := a.add_node(flat.Node{
		kind: .call
		typ: '[]Wide'
		children_start: call_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << map_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string][]Wide'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.compiler_owned_array_map_call_expands(a.nodes[int(map_call)])
	assert t.fn_span_map_expansion_estimate(0, int(map_call) + 1) > deferred_map_expansion_threshold
	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_compiler_collection_str_calls() {
	for collection_type in ['[]Wide', '[4]Wide', 'map[string]Wide'] {
		mut a := flat.FlatAst.new()
		make_items := a.add_node(flat.Node{
			kind: .ident
			value: 'make_items'
		})
		make_start := a.children.len
		a.children << make_items
		receiver := a.add_node(flat.Node{
			kind: .call
			typ: collection_type
			children_start: make_start
			children_count: 1
		})
		selector_start := a.children.len
		a.children << receiver
		selector := a.add_node(flat.Node{
			kind: .selector
			value: 'str'
			typ: 'fn () string'
			children_start: selector_start
			children_count: 1
		})
		call_start := a.children.len
		a.children << selector
		str_call := a.add_node(flat.Node{
			kind: .call
			typ: 'string'
			children_start: call_start
			children_count: 1
		})
		key := a.add_node(flat.Node{
			kind: .string_literal
			value: 'value'
		})
		map_start := a.children.len
		a.children << key
		a.children << str_call
		root := a.add_node(flat.Node{
			kind: .map_init
			typ: 'map[string]string'
			children_start: map_start
			children_count: 2
		})
		mut tc := types.TypeChecker.new(&a)
		tc.structs['Wide'] = []
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.structs['Wide'] = StructInfo{
			name: 'Wide'
		}

		assert t.compiler_collection_str_call_expands(a.nodes[int(str_call)]), collection_type
		assert t.fn_span_map_expansion_estimate(0, int(str_call) + 1) > deferred_map_expansion_threshold, collection_type
		assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold, collection_type
	}
}

fn test_external_map_expansion_estimate_defers_ownership_array_repeat_calls() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	make_items := a.add_node(flat.Node{
		kind: .ident
		value: 'make_items'
	})
	make_start := a.children.len
	a.children << make_items
	items := a.add_node(flat.Node{
		kind: .call
		typ: '[]Wide'
		children_start: make_start
		children_count: 1
	})
	selector_start := a.children.len
	a.children << items
	repeat_selector := a.add_node(flat.Node{
		kind: .selector
		value: 'repeat'
		typ: 'fn (int) []Wide'
		children_start: selector_start
		children_count: 1
	})
	count := a.add_node(flat.Node{
		kind: .int_literal
		value: '2'
		typ: 'int'
	})
	repeat_start := a.children.len
	a.children << repeat_selector
	a.children << count
	repeat_call := a.add_node(flat.Node{
		kind: .call
		typ: '[]Wide'
		children_start: repeat_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << repeat_call
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string][]Wide'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.collect(&a)
	tc.structs['Wide'] = [types.StructField{
		name: 'text'
		typ: tc.parse_type('string')
	}]
	tc.struct_implements['Wide'] = ['IClone']
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.ownership_array_repeat_call_expands(a.nodes[int(repeat_call)])
	assert t.fn_span_map_expansion_estimate(0, int(repeat_call) + 1) > deferred_map_expansion_threshold
	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_expansion_estimate_defers_interface_array_literal_repeat_calls_without_ownership() {
	mut a := flat.FlatAst.new()
	item := a.add_node(flat.Node{
		kind: .ident
		value: 'item'
		typ: 'IValue'
	})
	literal_start := a.children.len
	for _ in 0 .. 8 {
		a.children << item
	}
	literal := a.add_node(flat.Node{
		kind: .array_literal
		typ: '[]IValue'
		children_start: literal_start
		children_count: 8
	})
	selector_start := a.children.len
	a.children << literal
	repeat_selector := a.add_node(flat.Node{
		kind: .selector
		value: 'repeat'
		typ: 'fn (int) []IValue'
		children_start: selector_start
		children_count: 1
	})
	count := a.add_node(flat.Node{
		kind: .int_literal
		value: '32'
		typ: 'int'
	})
	repeat_start := a.children.len
	a.children << repeat_selector
	a.children << count
	repeat_call := a.add_node(flat.Node{
		kind: .call
		typ: '[]IValue'
		children_start: repeat_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	tc.interface_names['IValue'] = true
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.interface_array_literal_repeat_call_expands(a.nodes[int(repeat_call)])
	assert t.fn_span_map_expansion_estimate(0, int(repeat_call) + 1) > deferred_map_expansion_threshold
	assert t.external_map_tree_expansion_estimate(repeat_call, 0, 0) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_cast_and_arithmetic_reconstruction() {
	mut a := flat.FlatAst.new()
	mut level := []flat.NodeId{cap: 1024}
	for i in 0 .. 1024 {
		value := a.add_node(flat.Node{
			kind: .int_literal
			value: i.str()
			typ: 'int'
		})
		cast_start := a.children.len
		a.children << value
		level << a.add_node(flat.Node{
			kind: .cast_expr
			value: 'int'
			typ: 'int'
			children_start: cast_start
			children_count: 1
		})
	}
	for level.len > 1 {
		mut next := []flat.NodeId{cap: level.len / 2}
		for i := 0; i < level.len; i += 2 {
			infix_start := a.children.len
			a.children << level[i]
			a.children << level[i + 1]
			next << a.add_node(flat.Node{
				kind: .infix
				op: .plus
				typ: 'int'
				children_start: infix_start
				children_count: 2
			})
		}
		level = next.clone()
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << level[0]
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	base := t.map_init_expansion_estimate(root, a.nodes[int(root)])
	estimate := t.external_map_tree_expansion_estimate(root, 0, 0)
	assert estimate == base + 1024 * 2 + 1023 * 3
	assert estimate > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_unary_wrapper_reconstruction() {
	mut a := flat.FlatAst.new()
	mut wrapped := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	for i in 0 .. 1024 {
		wrapper_start := a.children.len
		a.children << wrapped
		wrapped = if i % 2 == 0 {
			a.add_node(flat.Node{
				kind: .paren
				typ: 'int'
				children_start: wrapper_start
				children_count: 1
			})
		} else {
			a.add_node(flat.Node{
				kind: .prefix
				op: .bit_not
				typ: 'int'
				children_start: wrapper_start
				children_count: 1
			})
		}
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << wrapped
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	base := t.map_init_expansion_estimate(root, a.nodes[int(root)])
	estimate := t.external_map_tree_expansion_estimate(root, 0, 0)
	assert estimate == base + 1024 * 2
	assert estimate > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_conditional_reconstruction() {
	mut a := flat.FlatAst.new()
	mut conditional := a.add_node(flat.Node{
		kind: .int_literal
		value: '0'
		typ: 'int'
	})
	level_count := deferred_map_expansion_threshold / 8 + 1
	for i in 0 .. level_count {
		guard := a.add_node(flat.Node{
			kind: .bool_literal
			value: 'false'
			typ: 'bool'
		})
		then_value := a.add_node(flat.Node{
			kind: .int_literal
			value: i.str()
			typ: 'int'
		})
		then_start := a.children.len
		a.children << then_value
		then_block := a.add_node(flat.Node{
			kind: .block
			children_start: then_start
			children_count: 1
		})
		else_start := a.children.len
		a.children << conditional
		else_block := a.add_node(flat.Node{
			kind: .block
			children_start: else_start
			children_count: 1
		})
		if_start := a.children.len
		a.children << guard
		a.children << then_block
		a.children << else_block
		conditional = a.add_node(flat.Node{
			kind: .if_expr
			typ: 'int'
			children_start: if_start
			children_count: 3
		})
	}
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << conditional
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	base := t.map_init_expansion_estimate(root, a.nodes[int(root)])
	estimate := t.external_map_tree_expansion_estimate(root, 0, 0)
	assert estimate == base + level_count * 8
	assert estimate > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_match_reconstruction() {
	mut a := flat.FlatAst.new()
	subject := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	condition := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		typ: 'int'
	})
	value := a.add_node(flat.Node{
		kind: .int_literal
		value: '42'
		typ: 'int'
	})
	branch_start := a.children.len
	a.children << condition
	a.children << value
	branch := a.add_node(flat.Node{
		kind: .match_branch
		children_start: branch_start
		children_count: 2
	})
	match_start := a.children.len
	a.children << subject
	a.children << branch
	match_expr := a.add_node(flat.Node{
		kind: .match_stmt
		typ: 'int'
		children_start: match_start
		children_count: 2
	})
	key := a.add_node(flat.Node{
		kind: .string_literal
		value: 'value'
	})
	map_start := a.children.len
	a.children << key
	a.children << match_expr
	root := a.add_node(flat.Node{
		kind: .map_init
		typ: 'map[string]int'
		children_start: map_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	base := t.map_init_expansion_estimate(root, a.nodes[int(root)])
	estimate := t.external_map_tree_expansion_estimate(root, 0, 0)
	assert estimate == base + deferred_map_expansion_threshold + 1
	assert estimate > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_defers_function_literal_lifting() {
	mut a := flat.FlatAst.new()
	root := a.add_node(flat.Node{
		kind: .fn_literal
	})
	mut t := Transformer{
		a: &a
	}
	assert t.external_map_tree_expansion_estimate(root, 0, 0) > deferred_map_expansion_threshold
}

fn test_deferred_worker_node_clone_preserves_skip_ownership_drops() {
	$if !v3_no_parallel ? {
		mut t := Transformer{
			deferred_base_writes: [
				DeferredBaseWrite{
					idx: 7
					kind: 2
					node: flat.Node{
						kind: .for_stmt
						skip_ownership_drops: true
					}
				},
			]
			scoped_promoted_texts: map[string]string{}
		}
		t.clone_deferred_worker_writes_from(0)
		cloned := t.deferred_base_writes[0].node
		assert cloned.kind == .for_stmt
		assert cloned.skip_ownership_drops
	}
}

fn test_merge_worker_shifts_private_specialization_metadata() {
	mut a := flat.FlatAst.new()
	base_id := a.add_node(flat.Node{
		kind: .fn_decl
		value: 'base_specialization'
	})
	a.specialized_fn_nodes[int(base_id)] = true
	a.specialized_fn_modules[int(base_id)] = 'base_module'
	a.specialized_fn_files[int(base_id)] = 'base.v'
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	base_nodes := master.a.nodes.len
	base_children := master.a.children.len

	mut worker_ast := master.clone_ast_base(base_nodes, base_children)
	mut worker_tc := tc.fork_for_parallel_transform(worker_ast)
	mut worker := master.fork_worker(worker_ast, worker_tc)
	assert worker.a.specialized_fn_modules[int(base_id)] == 'base_module'
	worker_id := worker_ast.add_node(flat.Node{
		kind: .fn_decl
		value: 'worker_specialization'
	})
	worker_ast.specialized_fn_nodes[int(worker_id)] = true
	worker_ast.specialized_fn_modules[int(worker_id)] = 'worker_module'
	worker_ast.specialized_fn_files[int(worker_id)] = 'worker.v'
	assert int(worker_id) == base_nodes
	assert int(worker_id) !in master.a.specialized_fn_nodes

	master.a.add_node(flat.Node{
		kind: .fn_decl
		value: 'earlier_master_append'
	})
	shifted_id := master.a.nodes.len
	master.merge_worker(worker, []FnWorkItem{}, base_nodes, base_children, false)

	assert master.a.nodes[shifted_id].value == 'worker_specialization'
	assert master.a.specialized_fn_nodes[shifted_id]
	assert master.a.specialized_fn_modules[shifted_id] == 'worker_module'
	assert master.a.specialized_fn_files[shifted_id] == 'worker.v'
	assert int(worker_id) !in master.a.specialized_fn_nodes
	assert master.a.specialized_fn_modules[int(base_id)] == 'base_module'
	assert master.a.specialized_fn_files[int(base_id)] == 'base.v'
}

fn test_merge_worker_signatures_updates_checker_method_suffix_index() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})

	mut worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	mut worker_tc := tc.fork_for_parallel_transform(worker_ast)
	worker_tc.ensure_private_transform_signatures()
	worker_tc.fn_ret_types['widgets.Box.open'] = types.Type(types.bool_)
	params := [types.Type(types.int_)]
	worker_tc.register_generated_fn_param_types('widgets.Box.open', params)
	worker := master.fork_worker(worker_ast, worker_tc)

	assert 'widgets.Box.open' !in master.tc.fn_ret_types
	assert 'widgets.Box.open' !in master.tc.fn_param_types
	assert 'Box.open' !in master.tc.receiver_method_suffix_index
	assert worker.tc.receiver_method_suffix_index['Box.open'] == 'widgets.Box.open'
	master.merge_worker_signatures(worker)

	assert master.tc.fn_param_types_for_name('widgets.Box.open') == params
	assert master.tc.receiver_method_suffix_index['Box.open'] == 'widgets.Box.open'
	assert master.tc.fn_param_types_for_name('Box.open') == params
	assert master.tc.fn_param_types_for_name('open') == params
}

fn test_parallel_master_detaches_metadata_maps_before_writing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.fn_ret_types['main.base'] = 'int'
	tc.fn_param_types['main.base'] = [types.Type(types.int_)]
	master.structs['main.Base'] = StructInfo{
		name: 'main.Base'
	}
	tc.structs['main.Base'] = []types.StructField{}

	mut worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	worker_tc := tc.fork_for_parallel_transform(worker_ast)
	worker := master.fork_worker(worker_ast, worker_tc)
	master.mark_parallel_worker_maps_shared()

	master.set_fn_ret_type('main.master_generated', 'bool')
	mut master_tc := unsafe { &types.TypeChecker(voidptr(master.tc)) }
	master_tc.ensure_private_transform_signatures()
	master_tc.register_generated_fn_param_types('main.master_generated', [
		types.Type(types.bool_),
	])

	assert master.fn_ret_types['main.master_generated'] == 'bool'
	assert 'main.master_generated' !in worker.fn_ret_types
	assert 'main.master_generated' in master.tc.fn_param_types
	assert 'main.master_generated' !in worker.tc.fn_param_types

	master.add_fn_literal_capture_context('CaptureContext', 'main', []string{}, map[string]string{})
	assert 'CaptureContext' in master.structs
	assert 'CaptureContext' !in worker.structs
	assert 'CaptureContext' in master.tc.structs
	assert 'CaptureContext' !in worker.tc.structs
}

fn test_transform_ast_clone_preserves_template_metadata() {
	mut a := flat.FlatAst.new()
	a.template_call_sites[7] = token.new_pos(3, 11)
	a.template_actions[7] = 'render_page'
	mut tc := types.TypeChecker.new(&a)
	master := new_transformer(mut a, &tc, map[string]bool{})
	worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	assert worker_ast.template_call_sites[7] == master.a.template_call_sites[7]
	assert worker_ast.template_actions[7] == 'render_page'
}

fn test_transform_worker_records_struct_operators_in_private_map() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut master := new_transformer(mut a, &tc, map[string]bool{})
	master.used_struct_operator_fns['main.Box.+'] = true

	mut worker_ast := master.clone_ast_base(master.a.nodes.len, master.a.children.len)
	mut worker_tc := tc.fork_for_parallel_transform(worker_ast)
	mut worker := master.fork_worker(worker_ast, worker_tc)
	worker.mark_struct_operator_used_name('main.Point.==')

	assert 'main.Point.==' !in master.used_struct_operator_fns
	master.merge_worker_used_fns(worker)
	assert master.used_struct_operator_fns['main.Box.+']
	assert master.used_struct_operator_fns['main.Point.==']
}

fn test_skipped_literal_decl_does_not_hide_later_closure() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{
		kind: .fn_literal
	})
	a.add_node(flat.Node{
		kind: .fn_decl
		value: 'dead'
	})
	a.add_node(flat.Node{
		kind: .fn_literal
	})
	main_idx := int(a.add_node(flat.Node{
		kind: .fn_decl
		value: 'main'
	}))
	helper_idx := int(a.add_node(flat.Node{
		kind: .fn_decl
		value: 'helper'
	}))
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, {
		'main':   true
		'helper': true
	})
	t.skip_generics = true
	t.transformed_fns = []bool{len: t.a.nodes.len}

	literal_decls := t.collect_literal_fn_decls(t.a.nodes.len)
	assert literal_decls == [1, main_idx]
	pure := t.transform_serial_then_collect_pure(literal_decls)
	assert t.transformed_fns[main_idx]
	assert !t.transformed_fns[helper_idx]
	assert pure.len == 1
	assert pure[0].fn_idx == helper_idx
}

fn test_parallel_escape_precheck_preserves_candidate_across_local_type_decl() {
	$if !v3_no_parallel ? {
		mut a := flat.FlatAst.new()
		a.nodes = []flat.Node{len: 65536}
		a.nodes[1] = flat.Node{
			kind: .prefix
			op: .amp
		}
		a.nodes[2] = flat.Node{
			kind: .struct_decl
			value: 'Local@local@first'
		}
		a.nodes[3] = flat.Node{
			kind: .fn_decl
			value: 'first'
		}
		a.nodes[4] = flat.Node{
			kind: .prefix
			op: .amp
		}
		a.nodes[5] = flat.Node{
			kind: .struct_decl
			value: 'TopLevel'
		}
		a.nodes[6] = flat.Node{
			kind: .fn_decl
			value: 'second'
		}
		a.ensure_workers(2)
		defer {
			a.close_workers()
		}
		mut tc := types.TypeChecker.new(&a)
		tc.top_level_idx = [0, 2, 3, 5, 6]
		tc.top_level_idx_nodes_len = a.nodes.len
		tc.synthetic_top_level_type_ids = [2]
		mut t := new_transformer(mut a, &tc, map[string]bool{})

		t.collect_literal_fn_decls(t.a.nodes.len)

		assert t.fn_escape_scan_flags[3] == 3
		assert t.fn_escape_scan_flags[6] == 1
	}
}
