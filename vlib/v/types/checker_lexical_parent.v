module types

import v.flat

// Each checker owns its topology memo, including read-only reachability forks.
// Scope bindings and smartcast types are deliberately not cached here.
@[heap]
struct LexicalParentMemo {
mut:
	generation  u32 = 1
	nodes_len   int
	parents_len int
	ids         [2048]int
	generations [2048]u32
	parents     [2048]flat.NodeId
	children    [2048]flat.NodeId
	depths      [2048]int
}

fn (mut memo LexicalParentMemo) invalidate() {
	memo.generation++
	if memo.generation == 0 {
		memo.generations = [2048]u32{}
		memo.generation = 1
	}
}

fn (tc &TypeChecker) invalidate_lexical_parent_memo() {
	if !isnil(tc.lexical_parent_memo) {
		mut memo := tc.lexical_parent_memo
		memo.invalidate()
	}
}

// lexical_parent_link skips ancestors that cannot affect lexical narrowing.
// `child` is the immediate child of the returned parent, so branch selection and
// preceding-write checks still use the same node as the ordinary parent walk.
// Keep the full distance: match narrowing has a 64-parent limit, while if/for
// narrowing remains available beyond that limit.
@[direct_array_access]
fn (tc &TypeChecker) lexical_parent_link(id flat.NodeId) (flat.NodeId, flat.NodeId, int) {
	initial_idx := int(id)
	if initial_idx < 0 || initial_idx >= tc.direct_parent_ids.len {
		return flat.empty_node, id, 1
	}
	mut wtc := unsafe { tc }
	if isnil(wtc.lexical_parent_memo) {
		wtc.lexical_parent_memo = &LexicalParentMemo{
			nodes_len:   tc.a.nodes.len
			parents_len: tc.direct_parent_ids.len
		}
	}
	mut memo := wtc.lexical_parent_memo
	if memo.nodes_len != tc.a.nodes.len || memo.parents_len != tc.direct_parent_ids.len {
		memo.invalidate()
		memo.nodes_len = tc.a.nodes.len
		memo.parents_len = tc.direct_parent_ids.len
	}
	initial_slot := initial_idx & 2047
	if memo.generations[initial_slot] == memo.generation && memo.ids[initial_slot] == initial_idx {
		return memo.parents[initial_slot], memo.children[initial_slot], memo.depths[initial_slot]
	}
	mut path := [64]flat.NodeId{}
	mut path_len := 0
	mut current := id
	mut parent := flat.empty_node
	mut child := id
	mut depth := 0
	for {
		idx := int(current)
		if idx < 0 || idx >= tc.direct_parent_ids.len {
			break
		}
		slot := idx & 2047
		if memo.generations[slot] == memo.generation && memo.ids[slot] == idx {
			parent = memo.parents[slot]
			child = memo.children[slot]
			depth += memo.depths[slot]
			break
		}
		if path_len < path.len {
			path[path_len] = current
			path_len++
		}
		child = current
		parent = tc.direct_parent_ids[idx]
		depth++
		if !tc.valid_node_id(parent)
			|| tc.a.nodes[int(parent)].kind in [.if_expr, .for_stmt, .match_branch, .match_stmt,
				.fn_decl, .fn_literal, .lambda_expr] {
			break
		}
		current = parent
	}
	for i in 0 .. path_len {
		idx := int(path[i])
		slot := idx & 2047
		memo.ids[slot] = idx
		memo.parents[slot] = parent
		memo.children[slot] = child
		memo.depths[slot] = depth - i
		memo.generations[slot] = memo.generation
	}
	return parent, child, depth
}

// SmartcastKeyFilter holds every expression key that a lexical smartcast in the
// node range [lo, hi] of one function can narrow: the subjects of its `is`
// tests (negated or not), the operands of its `== none` / `!= none` (or nil)
// comparisons, and its match subjects. Every narrowing the lexical walk finds
// is named by one of them, also through a bool condition saved in a local
// (whose initializer is in the same function). It also records whether the
// range has an `unsafe` block at all. Both are taken on the first query of the
// range.
@[heap]
struct SmartcastKeyFilter {
mut:
	lo               int = -1
	hi               int = -1
	ready            bool
	has_unsafe_block bool
	keys             []string
	index            map[string]bool
}

const smartcast_key_filter_linear = 16

fn (mut filter SmartcastKeyFilter) arm(lo int, hi int) {
	filter.lo = lo
	filter.hi = hi
	filter.ready = false
}

fn (mut filter SmartcastKeyFilter) add(key string) {
	if key.len == 0 {
		return
	}
	if filter.index.len > 0 {
		filter.index[key] = true
		return
	}
	if key in filter.keys {
		return
	}
	filter.keys << keep_string(key)
	if filter.keys.len > smartcast_key_filter_linear {
		for k in filter.keys {
			filter.index[k] = true
		}
	}
}

fn (filter &SmartcastKeyFilter) has(key string) bool {
	if filter.index.len > 0 {
		return key in filter.index
	}
	return key in filter.keys
}

@[direct_array_access]
fn (tc &TypeChecker) collect_smartcast_keys(mut filter SmartcastKeyFilter) {
	filter.keys.clear()
	if filter.index.len > 0 {
		filter.index.clear()
	}
	filter.has_unsafe_block = false
	hi := int_min(filter.hi, tc.a.nodes.len - 1)
	for i in filter.lo .. hi + 1 {
		node := unsafe { &tc.a.nodes[i] }
		match node.kind {
			.block {
				if node.value == 'unsafe' {
					filter.has_unsafe_block = true
				}
			}
			.is_expr, .match_stmt {
				if node.children_count > 0 {
					filter.add(tc.expr_key(tc.a.child(node, 0)))
				}
			}
			.infix {
				if node.op in [.eq, .ne] && node.children_count >= 2 {
					lhs := tc.a.child(node, 0)
					rhs := tc.a.child(node, 1)
					if tc.a.node(lhs).kind in [.none_expr, .nil_literal]
						|| tc.a.node(rhs).kind in [.none_expr, .nil_literal] {
						filter.add(tc.expr_key(lhs))
						filter.add(tc.expr_key(rhs))
					}
				}
			}
			else {}
		}
	}
	filter.ready = true
}

// node_outside_unsafe_blocks reports that the node `idx` of the function being
// checked has no `unsafe` block around it, as that function has none.
fn (tc &TypeChecker) node_outside_unsafe_blocks(idx int) bool {
	if isnil(tc.smartcast_key_filter) {
		return false
	}
	mut filter := tc.smartcast_key_filter
	if idx < filter.lo || idx > filter.hi || filter.lo < 0 {
		return false
	}
	if !filter.ready {
		tc.collect_smartcast_keys(mut filter)
	}
	return !filter.has_unsafe_block
}

// lexical_smartcast_key_unused reports that no lexical smartcast of the
// function being checked can narrow `key` at the node `idx`.
fn (tc &TypeChecker) lexical_smartcast_key_unused(idx int, key string) bool {
	if isnil(tc.smartcast_key_filter) {
		return false
	}
	mut filter := tc.smartcast_key_filter
	if idx < filter.lo || idx > filter.hi || filter.lo < 0 {
		return false
	}
	if !filter.ready {
		tc.collect_smartcast_keys(mut filter)
	}
	return !filter.has(key)
}
