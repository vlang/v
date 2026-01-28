// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// --- Dominators (Lengauer-Tarjan) ---

struct LTContext {
mut:
	parent   []int   // DFS tree parent
	semi     []int   // Semidominator (BlockID)
	vertex   []int   // Map DFS number -> BlockID
	bucket   [][]int // bucket[w] = set of vertices v s.t. semi[v] = w
	dfnum    []int   // DFS number (0 means unvisited)
	ancestor []int   // DSU parent
	label    []int   // DSU label (min semi in path)
	n        int     // Counter
}

fn compute_dominators(mut m ssa.Module) {
	// Pre-allocate context once and reuse for all functions
	// Size based on total blocks since block IDs are indices into m.blocks
	max_id := m.blocks.len

	mut ctx := LTContext{
		parent:   []int{len: max_id, init: -1}
		semi:     []int{len: max_id, init: -1}
		vertex:   []int{len: max_id + 1, init: -1}
		bucket:   [][]int{len: max_id}
		dfnum:    []int{len: max_id, init: 0}
		ancestor: []int{len: max_id, init: -1}
		label:    []int{len: max_id, init: -1}
		n:        0
	}

	for func in m.funcs {
		if func.blocks.len == 0 {
			continue
		}

		// Reset context for this function (only reset what's needed)
		ctx.n = 0
		for blk_id in func.blocks {
			ctx.parent[blk_id] = -1
			ctx.semi[blk_id] = blk_id
			ctx.vertex[blk_id] = -1
			ctx.bucket[blk_id] = []
			ctx.dfnum[blk_id] = 0
			ctx.ancestor[blk_id] = -1
			ctx.label[blk_id] = blk_id
			// Initialize idom to -1
			m.blocks[blk_id].idom = -1
		}

		entry := func.blocks[0]
		lt_dfs(mut m, entry, mut ctx)

		// Process in reverse DFS order (skip root)
		for i := ctx.n; i >= 2; i-- {
			w := ctx.vertex[i]

			// 1. Calculate Semidominator
			for p in m.blocks[w].preds {
				// Only process reachable predecessors
				if ctx.dfnum[p] == 0 {
					continue
				}

				u := ctx.eval(p)
				if ctx.dfnum[ctx.semi[u]] < ctx.dfnum[ctx.semi[w]] {
					ctx.semi[w] = ctx.semi[u]
				}
			}

			// Add w to bucket of its semidominator
			ctx.bucket[ctx.semi[w]] << w

			// Link to parent in forest
			ctx.link(ctx.parent[w], w)

			// 2. Implicitly compute IDom
			parent_w := ctx.parent[w]
			// Drain bucket of parent
			// Note: We copy to iterate because we might clear/modify?
			// Standard algo drains bucket[parent_w] now.
			for v in ctx.bucket[parent_w] {
				u := ctx.eval(v)
				if ctx.semi[u] == ctx.semi[v] {
					m.blocks[v].idom = parent_w
				} else {
					m.blocks[v].idom = u // Deferred: idom[v] = idom[u]
				}
			}
			ctx.bucket[parent_w] = []
		}

		// 3. Explicitly compute IDom
		for i := 2; i <= ctx.n; i++ {
			w := ctx.vertex[i]
			if m.blocks[w].idom != ctx.vertex[ctx.dfnum[ctx.semi[w]]] {
				m.blocks[w].idom = m.blocks[m.blocks[w].idom].idom
			}
		}

		m.blocks[entry].idom = entry

		// Build Dom Tree Children
		for blk_id in func.blocks {
			m.blocks[blk_id].dom_tree = []
		}
		for blk_id in func.blocks {
			idom := m.blocks[blk_id].idom
			if idom != -1 && idom != blk_id {
				m.blocks[idom].dom_tree << blk_id
			}
		}
	}
}

fn lt_dfs(mut m ssa.Module, v int, mut ctx LTContext) {
	ctx.n++
	ctx.dfnum[v] = ctx.n
	ctx.vertex[ctx.n] = v

	for w in m.blocks[v].succs {
		if ctx.dfnum[w] == 0 {
			ctx.parent[w] = v
			lt_dfs(mut m, w, mut ctx)
		}
	}
}

fn (mut ctx LTContext) compress(v int) {
	if ctx.ancestor[ctx.ancestor[v]] != -1 {
		ctx.compress(ctx.ancestor[v])

		// Update label based on ancestor
		if ctx.dfnum[ctx.semi[ctx.label[ctx.ancestor[v]]]] < ctx.dfnum[ctx.semi[ctx.label[v]]] {
			ctx.label[v] = ctx.label[ctx.ancestor[v]]
		}
		ctx.ancestor[v] = ctx.ancestor[ctx.ancestor[v]]
	}
}

fn (mut ctx LTContext) eval(v int) int {
	if ctx.ancestor[v] == -1 {
		return ctx.label[v]
	}
	ctx.compress(v)
	// If label[ancestor[v]] is better than label[v], use it?
	// The path compression updates label[v] to be the best in the path.
	// However, standard EVAL checks:
	if ctx.dfnum[ctx.semi[ctx.label[ctx.ancestor[v]]]] >= ctx.dfnum[ctx.semi[ctx.label[v]]] {
		return ctx.label[v]
	}
	return ctx.label[ctx.ancestor[v]]
}

fn (mut ctx LTContext) link(v int, w int) {
	ctx.ancestor[w] = v
}
