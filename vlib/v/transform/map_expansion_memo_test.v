module transform

import os
import v.flat
import v.types

fn test_expansion_type_memo_preserves_estimates_and_does_not_survive_context_changes() {
	saved_disable := os.getenv('V3_NO_NODE_TYPE_MEMO')
	os.unsetenv('V3_NO_NODE_TYPE_MEMO')
	defer {
		if saved_disable.len > 0 {
			os.setenv('V3_NO_NODE_TYPE_MEMO', saved_disable, true)
		}
	}
	mut a := flat.FlatAst.new()
	values := a.add_node(flat.Node{ kind: .ident, value: 'values' })
	key := a.add_node(flat.Node{ kind: .int_literal, value: '0' })
	start := a.children.len
	a.children << values
	a.children << key
	lookup := a.add_node(flat.Node{
		kind:           .index
		children_start: start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	hi := int(lookup) + 1
	large_map := 'map[int][4096][]int'
	for typ in [large_map, '[]int', 'map[int]int', large_map] {
		t.set_var_type('values', typ)
		uncached := t.fn_span_map_expansion_estimate_uncached(0, hi)
		cached := t.fn_span_map_expansion_estimate(0, hi)
		assert cached == uncached
		assert (cached > deferred_map_expansion_threshold) == (typ == large_map)
		assert isnil(t.node_type_memo)
		assert !t.expansion_node_type_memo.active
		assert t.node_type(values) == typ
	}
	// The read-only analysis also runs while a body memo is active. Its scratch
	// tags must not clear that memo or change the range that the body owns.
	t.begin_node_type_memo(0, hi - 1)
	assert t.node_type(values) == large_map
	outer_memo := t.node_type_memo
	assert outer_memo.filled[int(values)] != 0
	assert t.fn_span_map_expansion_estimate(0, hi) == t.fn_span_map_expansion_estimate_uncached(0,
		hi)
	assert t.node_type_memo == outer_memo
	assert outer_memo.active
	assert outer_memo.lo == 0 && outer_memo.hi == hi - 1
	assert outer_memo.filled[int(values)] != 0
	assert t.node_type(values) == large_map
	assert t.fn_span_map_expansion_estimate(hi, hi) == 0
	assert t.node_type_memo == outer_memo && outer_memo.active
	t.end_node_type_memo()
	t.node_type_memo = unsafe { nil }
	// Changing a smartcast after the scan must resolve against the new context,
	// both in ordinary type queries and in the next expansion estimate.
	tc.sum_types['Collection'] = [large_map, '[]int']
	t.sum_types['Collection'] = [large_map, '[]int']
	t.set_var_type('values', 'Collection')
	for variant in [large_map, '[]int', large_map] {
		t.push_smartcast('values', variant, 'Collection')
		uncached := t.fn_span_map_expansion_estimate_uncached(0, hi)
		cached := t.fn_span_map_expansion_estimate(0, hi)
		assert cached == uncached
		assert (cached > deferred_map_expansion_threshold) == (variant == large_map)
		assert t.node_type(values) == variant
		t.pop_smartcast()
		assert t.node_type(values) == 'Collection'
	}
	// Forks carry the declaration view, while each worker owns its scratch cache.
	worker := t.fork_scan_worker(&tc)
	assert isnil(worker.expansion_node_type_memo)
}

fn test_empty_disabled_function_map_skips_call_and_operator_resolution() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert !t.is_disabled_fn_call(flat.NodeId(-1), flat.Node{ kind: .call })
	assert t.disabled_struct_operator_zero_value_expansion_estimate(flat.NodeId(-1),
		flat.Node{ kind: .infix, children_count: 2, op: .plus }) == 0
}

fn test_expansion_memo_setting_is_shared_by_compilation_workers() {
	previous := os.getenv('V3_NO_NODE_TYPE_MEMO')
	os.setenv('V3_NO_NODE_TYPE_MEMO', '1', true)
	defer {
		if previous == '' {
			os.unsetenv('V3_NO_NODE_TYPE_MEMO')
		} else {
			os.setenv('V3_NO_NODE_TYPE_MEMO', previous, true)
		}
	}
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{ kind: .int_literal, value: '0' })
	mut tc := types.TypeChecker.new(&a)
	mut disabled := new_transformer(mut a, &tc, map[string]bool{})
	os.unsetenv('V3_NO_NODE_TYPE_MEMO')
	worker := disabled.fork_scan_worker(&tc)
	assert !worker.memo_expansion_node_types
	assert disabled.fn_span_map_expansion_estimate(0, 1) == 0
	assert isnil(disabled.expansion_node_type_memo)
	mut enabled := new_transformer(mut a, &tc, map[string]bool{})
	assert enabled.memo_expansion_node_types
	assert enabled.fn_span_map_expansion_estimate(0, 1) == 0
	assert !isnil(enabled.expansion_node_type_memo)
}
