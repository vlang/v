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
		assert monomorph_job_limit(12, 500_000, 0) == 2
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

fn test_const_map_expansion_estimate_ignores_shadowing_local() {
	mut a := flat.FlatAst.new()
	const_id := a.add_node(flat.Node{
		kind: .map_init
	})
	param := a.add_node(flat.Node{
		kind:  .param
		value: 'lookup'
		typ:   'map[string]int'
	})
	ident := a.add_node(flat.Node{
		kind:  .ident
		value: 'lookup'
	})
	block_start := a.children.len
	a.children << ident
	block := a.add_node(flat.Node{
		kind:           .block
		children_start: block_start
		children_count: 1
	})
	fn_start := a.children.len
	a.children << param
	a.children << block
	a.add_node(flat.Node{
		kind:           .fn_decl
		value:          'shadowed'
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
		kind:  .ident
		value: 'lookup'
	})
	block_start := a.children.len
	a.children << ident
	block := a.add_node(flat.Node{
		kind:           .block
		children_start: block_start
		children_count: 1
	})
	fn_start := a.children.len
	a.children << block
	a.add_node(flat.Node{
		kind:           .fn_decl
		value:          'uses_const'
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
		kind:           .map_init
		typ:            'map[int]int'
		children_count: 254
	})
	owned_id := a.add_node(flat.Node{
		kind:           .map_init
		typ:            'map[int]string'
		children_count: 254
	})
	t := new_transformer(mut a, &tc, map[string]bool{})

	assert t.map_init_expansion_estimate(plain_id, a.nodes[int(plain_id)]) < deferred_map_expansion_threshold
	assert t.map_init_expansion_estimate(owned_id, a.nodes[int(owned_id)]) > deferred_map_expansion_threshold
}

fn test_external_map_expansion_estimate_includes_nested_array_lowering() {
	mut a := flat.FlatAst.new()
	mut inner_arrays := []flat.NodeId{cap: 130}
	for i in 0 .. 130 {
		value := a.add_node(flat.Node{
			kind:  .int_literal
			value: i.str()
		})
		inner_start := a.children.len
		a.children << value
		inner_arrays << a.add_node(flat.Node{
			kind:           .array_literal
			children_start: inner_start
			children_count: 1
		})
	}
	outer_start := a.children.len
	for inner in inner_arrays {
		a.children << inner
	}
	outer := a.add_node(flat.Node{
		kind:           .array_literal
		children_start: outer_start
		children_count: flat.child_count(inner_arrays.len)
	})
	key := a.add_node(flat.Node{
		kind:  .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << outer
	root := a.add_node(flat.Node{
		kind:           .map_init
		typ:            'map[string][][]int'
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
			kind:  .int_literal
			value: i.str()
		})
	}
	array := a.add_node(flat.Node{
		kind:           .array_literal
		children_start: children_start
		children_count: flat.child_count(deferred_map_expansion_threshold + 1)
	})
	key := a.add_node(flat.Node{
		kind:  .string_literal
		value: 'items'
	})
	map_start := a.children.len
	a.children << key
	a.children << array
	root := a.add_node(flat.Node{
		kind:           .map_init
		typ:            'map[string][]int'
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
		typ:  'LargeValue'
	})
	key := a.add_node(flat.Node{
		kind:  .string_literal
		value: 'item'
	})
	map_start := a.children.len
	a.children << key
	a.children << value
	root := a.add_node(flat.Node{
		kind:           .map_init
		typ:            'map[string]LargeValue'
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
		kind:  .ident
		value: 'index'
		typ:   'int'
	})
	field_start := a.children.len
	a.children << index
	init_field := a.add_node(flat.Node{
		kind:           .field_init
		value:          'init'
		children_start: field_start
		children_count: 1
	})
	array_start := a.children.len
	a.children << init_field
	root := a.add_node(flat.Node{
		kind:           .array_init
		typ:            'int[4096]'
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
		typ:  '[4096][]int'
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	estimate := t.external_map_tree_expansion_estimate(root, 0, 0)
	assert estimate > deferred_map_expansion_threshold

	plain := a.add_node(flat.Node{
		kind: .array_init
		typ:  'int[4096]'
	})
	assert t.fixed_array_init_expansion_estimate(plain, a.nodes[int(plain)]) == 0
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
	part_count := 1001
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

	estimate := t.string_interp_expansion_estimate(a.nodes[int(interp)])
	join_estimate := 2 * (part_count - 1)
	assert join_estimate < deferred_map_expansion_threshold
	assert estimate == join_estimate + part_count * string_interp_hoisted_part_expansion_estimate
	assert estimate > deferred_map_expansion_threshold
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
	for i in 0 .. deferred_map_expansion_threshold + 1 {
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

fn test_deferred_worker_node_clone_preserves_skip_ownership_drops() {
	$if !v3_no_parallel ? {
		mut t := Transformer{
			deferred_base_writes:  [
				DeferredBaseWrite{
					idx:  7
					kind: 2
					node: flat.Node{
						kind:                 .for_stmt
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
		kind:  .fn_decl
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
		kind:  .fn_decl
		value: 'worker_specialization'
	})
	worker_ast.specialized_fn_nodes[int(worker_id)] = true
	worker_ast.specialized_fn_modules[int(worker_id)] = 'worker_module'
	worker_ast.specialized_fn_files[int(worker_id)] = 'worker.v'
	assert int(worker_id) == base_nodes
	assert int(worker_id) !in master.a.specialized_fn_nodes

	master.a.add_node(flat.Node{
		kind:  .fn_decl
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
		kind:  .fn_decl
		value: 'dead'
	})
	a.add_node(flat.Node{
		kind: .fn_literal
	})
	main_idx := int(a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'main'
	}))
	helper_idx := int(a.add_node(flat.Node{
		kind:  .fn_decl
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
			op:   .amp
		}
		a.nodes[2] = flat.Node{
			kind:  .struct_decl
			value: 'Local@local@first'
		}
		a.nodes[3] = flat.Node{
			kind:  .fn_decl
			value: 'first'
		}
		a.nodes[4] = flat.Node{
			kind: .prefix
			op:   .amp
		}
		a.nodes[5] = flat.Node{
			kind:  .struct_decl
			value: 'TopLevel'
		}
		a.nodes[6] = flat.Node{
			kind:  .fn_decl
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
