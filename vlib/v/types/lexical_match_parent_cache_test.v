module types

import v.flat

fn match_parent_cache_wrap(mut a flat.FlatAst, kind flat.NodeKind, child flat.NodeId) flat.NodeId {
	start := a.begin_children()
	a.add_child(child)
	return a.add_node(flat.Node{
		kind:           kind
		children_start: start
		children_count: 1
	})
}

fn match_parent_cache_match(mut a flat.FlatAst, subject string, pattern string, body flat.NodeId) flat.NodeId {
	pattern_id := a.add_val(.ident, pattern)
	branch_start := a.begin_children()
	a.add_child(pattern_id)
	a.add_child(body)
	branch := a.add_node(flat.Node{
		kind:           .match_branch
		value:          '1'
		children_start: branch_start
		children_count: 2
	})
	subject_id := a.add_val(.ident, subject)
	start := a.begin_children()
	a.add_child(subject_id)
	a.add_child(branch)
	return a.add_node(flat.Node{
		kind:           .match_stmt
		children_start: start
		children_count: 2
	})
}

fn test_match_parent_cache_keeps_outer_matches_and_function_boundaries() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	inner_body := match_parent_cache_wrap(mut a, .block, value)
	inner := match_parent_cache_match(mut a, 'other', 'string', inner_body)
	closure_value := a.add_val(.ident, 'value')
	closure := match_parent_cache_wrap(mut a, .fn_literal, closure_value)
	body_start := a.begin_children()
	a.add_child(inner)
	a.add_child(closure)
	body := a.add_node(flat.Node{
		kind:           .block
		children_start: body_start
		children_count: 2
	})
	outer := match_parent_cache_match(mut a, 'value', 'int', body)
	match_parent_cache_wrap(mut a, .fn_decl, outer)
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.sum_types['Value'] = ['int', 'string']
	tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
	tc.cur_scope.insert('other', Type(SumType{ name: 'Value' }))
	for _ in 0 .. 3 {
		tc.arm_body_resolve_memo(0, a.nodes.len - 1)
		assert tc.lexical_match_smartcast_type(value)? == Type(int_)
		assert tc.lexical_match_smartcast_type(closure_value) == none
		// Parent links work even outside a checked body, as in reachability
		// analysis. Disable only the topology memo for the reference walk.
		assert tc.lexical_match_smartcast_type(value)? == Type(int_)
		tc.disarm_body_resolve_memo()
		tc.cache_lexical_parents = false
		assert tc.lexical_match_smartcast_type(value)? == Type(int_)
		assert tc.lexical_match_smartcast_type(closure_value) == none
		tc.cache_lexical_parents = true
	}
}

fn test_match_parent_cache_resolves_types_after_a_cached_topology_miss() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	body := match_parent_cache_wrap(mut a, .block, value)
	match_node := match_parent_cache_match(mut a, 'value', 'int', body)
	match_parent_cache_wrap(mut a, .fn_decl, match_node)
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.sum_types['Value'] = ['string']
	tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	assert tc.lexical_match_smartcast_type(value) == none
	// A miss in type resolution is not cached: declaration metadata can acquire
	// a variant while the parsed parent links remain unchanged.
	tc.sum_types['Value'] = ['int', 'string']
	tc.type_cache.sum_variant_pattern_entries.clear()
	assert tc.lexical_match_smartcast_type(value)? == Type(int_)
}

fn test_match_parent_cache_preserves_the_64_parent_limit_for_shorter_suffixes() {
	for wrappers in [0, 60, 61, 62, 80] {
		mut a := flat.FlatAst.new()
		value := a.add_val(.ident, 'value')
		mut current := value
		mut suffix := value
		for i in 0 .. wrappers {
			current = match_parent_cache_wrap(mut a, .paren, current)
			if i == 19 {
				suffix = current
			}
		}
		body := match_parent_cache_wrap(mut a, .block, current)
		match_node := match_parent_cache_match(mut a, 'value', 'int', body)
		match_parent_cache_wrap(mut a, .fn_decl, match_node)
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		tc.sum_types['Value'] = ['int', 'string']
		tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
		tc.arm_body_resolve_memo(0, a.nodes.len - 1)
		if wrappers <= 61 {
			assert tc.lexical_match_smartcast_type(value)? == Type(int_)
		} else {
			assert tc.lexical_match_smartcast_type(value) == none
		}
		// A long query must preserve the exact distance for its shorter suffixes.
		assert tc.lexical_smartcast_type_in_parents(suffix, 'value', true)? == Type(int_)
		tc.disarm_body_resolve_memo()
		tc.cache_lexical_parents = false
		assert tc.lexical_smartcast_type_in_parents(suffix, 'value', true)? == Type(int_)
	}
}

fn test_match_parent_cache_revalidates_slot_collisions_and_parent_generations() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.ident, 'value')
	first_parent := match_parent_cache_wrap(mut a, .fn_literal, first)
	for a.nodes.len < 2048 {
		a.add(.int_literal)
	}
	second := a.add_val(.ident, 'value')
	second_parent := match_parent_cache_wrap(mut a, .match_branch, second)
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	for _ in 0 .. 3 {
		parent, child, depth := tc.lexical_parent_link(first)
		assert child == first
		assert parent == first_parent
		assert depth == 1
		other_parent, other_child, other_depth := tc.lexical_parent_link(second)
		assert other_child == second
		assert other_parent == second_parent
		assert other_depth == 1
	}
	a.nodes[int(first_parent)].kind = .paren
	new_parent := match_parent_cache_wrap(mut a, .fn_literal, first_parent)
	tc.build_direct_parent_index(&a)
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	parent, child, depth := tc.lexical_parent_link(first)
	assert child == first_parent
	assert parent == new_parent
	assert depth == 2
	// Reusing a long-lived checker after generation rollover must also discard
	// entries left by the first generation of the bounded cache.
	tc.lexical_parent_memo.generations[int(first) & 2047] = 1
	tc.lexical_parent_memo.parents[int(first) & 2047] = first_parent
	tc.lexical_parent_memo.generation = u32(0xffff_ffff)
	tc.invalidate_lexical_parent_memo()
	rolled_parent, rolled_child, rolled_depth := tc.lexical_parent_link(first)
	assert rolled_child == first_parent
	assert rolled_parent == new_parent
	assert rolled_depth == 2
}

fn test_match_parent_cache_refreshes_rewritten_ancestors_without_a_new_body() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	body := match_parent_cache_wrap(mut a, .block, value)
	match_node := match_parent_cache_match(mut a, 'value', 'int', body)
	match_parent_cache_wrap(mut a, .fn_decl, match_node)
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.sum_types['Value'] = ['int', 'string']
	tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	assert tc.lexical_match_smartcast_type(value)? == Type(int_)
	mut memo := tc.body_resolve_memo
	call_generation := memo.call_generation
	call_slot := int(value) & 2047
	memo.call_ids[call_slot] = int(value)
	memo.call_generations[call_slot] = call_generation
	memo.call_infos[call_slot] = CallInfo{ name: 'kept.call' }
	// Keep the same node ids and active body, but turn the intervening block
	// into a function boundary. Refreshing parent metadata invalidates all links.
	a.nodes[int(body)].kind = .fn_literal
	tc.refresh_direct_parent_index(&a)
	assert memo.call_generation == call_generation
	assert tc.lexical_match_smartcast_type(value) == none
	a.nodes[int(body)].kind = .block
	tc.invalidate_checked_expr_type(int(body))
	assert memo.call_generation == call_generation
	assert memo.call_generations[call_slot] == call_generation
	assert memo.call_infos[call_slot].name == 'kept.call'
	assert tc.lexical_match_smartcast_type(value)? == Type(int_)
	tc.disarm_body_resolve_memo()
	assert tc.lexical_match_smartcast_type(value)? == Type(int_)
}

fn test_match_parent_cache_rechecks_parents_added_by_prepared_collect() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.ident, 'value')
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.sum_types['Value'] = ['int', 'string']
	tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	assert tc.lexical_match_smartcast_type(value) == none
	start := a.nodes.len
	body := match_parent_cache_wrap(mut a, .block, value)
	match_node := match_parent_cache_match(mut a, 'value', 'int', body)
	match_parent_cache_wrap(mut a, .fn_decl, match_node)
	// The appended tree assigns a previously parentless node its first parent.
	// Keep the original body generation active, as a prepared client can do.
	tc.extend_direct_parent_index(&a, start)
	assert tc.lexical_match_smartcast_type(value)? == Type(int_)
	tc.disarm_body_resolve_memo()
	assert tc.lexical_match_smartcast_type(value)? == Type(int_)
}
