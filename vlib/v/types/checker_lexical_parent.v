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
