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

		// Validate that all block IDs and their successor/predecessor
		// references are within bounds.
		mut valid := true
		for blk_id in func.blocks {
			if blk_id < 0 || blk_id >= max_id {
				valid = false
				break
			}
			for s in m.blocks[blk_id].succs {
				if s < 0 || s >= max_id {
					valid = false
					break
				}
			}
			if !valid {
				break
			}
			for p in m.blocks[blk_id].preds {
				if p < 0 || p >= max_id {
					valid = false
					break
				}
			}
			if !valid {
				break
			}
		}
		if !valid {
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
			if i >= ctx.vertex.len {
				continue
			}
			w := ctx.vertex[i]
			if w < 0 || w >= m.blocks.len {
				continue
			}

			// 1. Calculate Semidominator
			for p in m.blocks[w].preds {
				// Only process reachable predecessors
				if p < 0 || p >= ctx.dfnum.len || ctx.dfnum[p] == 0 {
					continue
				}

				u := ctx.eval(p)
				if u < 0 || u >= ctx.semi.len {
					continue
				}
				semi_u := ctx.semi[u]
				semi_w := ctx.semi[w]
				if semi_u < 0 || semi_u >= ctx.dfnum.len || semi_w < 0 || semi_w >= ctx.dfnum.len {
					continue
				}
				if ctx.dfnum[semi_u] < ctx.dfnum[semi_w] {
					ctx.semi[w] = ctx.semi[u]
				}
			}

			// Add w to bucket of its semidominator
			semi_w2 := ctx.semi[w]
			if semi_w2 >= 0 && semi_w2 < ctx.bucket.len {
				ctx.bucket[semi_w2] << w
			}

			// Link to parent in forest
			ctx.link(ctx.parent[w], w)

			// 2. Implicitly compute IDom
			parent_w := ctx.parent[w]
			if parent_w < 0 || parent_w >= ctx.bucket.len {
				continue
			}
			// Drain bucket of parent
			// Note: We copy to iterate because we might clear/modify?
			// Standard algo drains bucket[parent_w] now.
			for v in ctx.bucket[parent_w] {
				if v < 0 || v >= m.blocks.len {
					continue
				}
				u := ctx.eval(v)
				if u < 0 || u >= ctx.semi.len || v >= ctx.semi.len {
					continue
				}
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
			if i >= ctx.vertex.len {
				continue
			}
			w := ctx.vertex[i]
			if w < 0 || w >= m.blocks.len {
				continue
			}
			semi_w := ctx.semi[w]
			if semi_w < 0 || semi_w >= ctx.dfnum.len {
				continue
			}
			dfnum_semi_w := ctx.dfnum[semi_w]
			if dfnum_semi_w < 0 || dfnum_semi_w >= ctx.vertex.len {
				continue
			}
			target := ctx.vertex[dfnum_semi_w]
			if target < 0 {
				continue
			}
			if m.blocks[w].idom != target {
				idom_w := m.blocks[w].idom
				if idom_w >= 0 && idom_w < m.blocks.len {
					next_idom := m.blocks[idom_w].idom
					if next_idom >= 0 {
						m.blocks[w].idom = next_idom
					}
				}
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
	if v < 0 || v >= ctx.dfnum.len {
		return
	}
	ctx.n++
	if ctx.n >= ctx.vertex.len {
		return
	}
	ctx.dfnum[v] = ctx.n
	ctx.vertex[ctx.n] = v

	if v >= m.blocks.len {
		return
	}
	for w in m.blocks[v].succs {
		if w < 0 || w >= ctx.dfnum.len {
			continue
		}
		if ctx.dfnum[w] == 0 {
			ctx.parent[w] = v
			lt_dfs(mut m, w, mut ctx)
		}
	}
}

fn (mut ctx LTContext) compress(v int) {
	if v < 0 || v >= ctx.ancestor.len {
		return
	}
	av := ctx.ancestor[v]
	if av < 0 || av >= ctx.ancestor.len {
		return
	}
	if ctx.ancestor[av] != -1 {
		ctx.compress(av)

		// Update label based on ancestor
		new_av := ctx.ancestor[v]
		if new_av < 0 || new_av >= ctx.label.len || v >= ctx.label.len {
			return
		}
		label_av := ctx.label[new_av]
		label_v := ctx.label[v]
		if label_av < 0 || label_av >= ctx.semi.len || label_v < 0 || label_v >= ctx.semi.len {
			return
		}
		semi_label_av := ctx.semi[label_av]
		semi_label_v := ctx.semi[label_v]
		if semi_label_av < 0 || semi_label_av >= ctx.dfnum.len || semi_label_v < 0
			|| semi_label_v >= ctx.dfnum.len {
			return
		}
		if ctx.dfnum[semi_label_av] < ctx.dfnum[semi_label_v] {
			ctx.label[v] = ctx.label[new_av]
		}
		aav := ctx.ancestor[new_av]
		if aav >= 0 {
			ctx.ancestor[v] = aav
		}
	}
}

fn (mut ctx LTContext) eval(v int) int {
	if v < 0 || v >= ctx.ancestor.len {
		return if v >= 0 && v < ctx.label.len { ctx.label[v] } else { 0 }
	}
	if ctx.ancestor[v] == -1 {
		return ctx.label[v]
	}
	ctx.compress(v)
	av := ctx.ancestor[v]
	if av < 0 || av >= ctx.label.len {
		return if v < ctx.label.len { ctx.label[v] } else { 0 }
	}
	if v >= ctx.label.len {
		return 0
	}
	label_av := ctx.label[av]
	label_v := ctx.label[v]
	if label_av < 0 || label_av >= ctx.semi.len || label_v < 0 || label_v >= ctx.semi.len {
		return label_v
	}
	semi_lav := ctx.semi[label_av]
	semi_lv := ctx.semi[label_v]
	if semi_lav < 0 || semi_lav >= ctx.dfnum.len || semi_lv < 0 || semi_lv >= ctx.dfnum.len {
		return label_v
	}
	if ctx.dfnum[semi_lav] >= ctx.dfnum[semi_lv] {
		return label_v
	}
	return label_av
}

fn (mut ctx LTContext) link(v int, w int) {
	ctx.ancestor[w] = v
}
