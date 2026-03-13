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
	idom     []int   // Immediate dominator (flat array, indexed by block ID)
	dom_tree [][]int // Dom tree children (flat array, indexed by block ID)
	n        int     // Counter
}

// DFS frame for iterative lt_dfs
struct DfsFrame {
mut:
	node     int
	succ_idx int // next successor to process
}

// DomInfo holds dominator results in flat arrays to avoid struct field access issues
struct DomInfo {
mut:
	idom     []int   // Immediate dominator, indexed by block ID
	dom_tree [][]int // Dom tree children, indexed by block ID
}

fn compute_dominators(mut m ssa.Module, cfg &CfgData) DomInfo {
	max_id := m.blocks.len

	mut ctx := LTContext{
		parent:   []int{len: max_id, init: -1}
		semi:     []int{len: max_id, init: -1}
		vertex:   []int{len: max_id + 1, init: -1}
		bucket:   [][]int{len: max_id}
		dfnum:    []int{len: max_id, init: 0}
		ancestor: []int{len: max_id, init: -1}
		label:    []int{len: max_id, init: -1}
		idom:     []int{len: max_id, init: -1}
		dom_tree: [][]int{len: max_id}
		n:        0
	}

	for fi in 0 .. m.funcs.len {
		func := m.funcs[fi]
		if func.blocks.len == 0 {
			continue
		}

		// Validate that all block IDs and their successor/predecessor
		// references are within bounds.
		mut valid := true
		n_func_blocks := func.blocks.len
		for fbi in 0 .. n_func_blocks {
			blk_id := func.blocks[fbi]
			if blk_id < 0 || blk_id >= max_id {
				valid = false
				break
			}
			n_succs := cfg.succs[blk_id].len
			for si in 0 .. n_succs {
				s := cfg.succs[blk_id][si]
				if s < 0 || s >= max_id {
					valid = false
					break
				}
			}
			if !valid {
				break
			}
			n_preds := cfg.preds[blk_id].len
			for pi in 0 .. n_preds {
				p := cfg.preds[blk_id][pi]
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
		for fbi2 in 0 .. n_func_blocks {
			blk_id := func.blocks[fbi2]
			ctx.parent[blk_id] = -1
			ctx.semi[blk_id] = blk_id
			ctx.vertex[blk_id] = -1
			ctx.bucket[blk_id].clear()
			ctx.dfnum[blk_id] = 0
			ctx.ancestor[blk_id] = -1
			ctx.label[blk_id] = blk_id
			ctx.idom[blk_id] = -1
			ctx.dom_tree[blk_id] = []
		}

		entry := func.blocks[0]
		lt_dfs(entry, cfg, mut ctx)

		// Process in reverse DFS order (skip root)
		for i := ctx.n; i >= 2; i-- {
			if i >= ctx.vertex.len {
				continue
			}
			w := ctx.vertex[i]
			if w < 0 || w >= max_id {
				continue
			}

			// 1. Calculate Semidominator - use cfg.preds instead of m.blocks[w].preds
			n_w_preds := cfg.preds[w].len
			for pi2 in 0 .. n_w_preds {
				p := cfg.preds[w][pi2]
				if p < 0 || p >= max_id || ctx.dfnum[p] == 0 {
					continue
				}

				u := ctx.eval(p)
				if u < 0 || u >= max_id {
					continue
				}
				semi_u := ctx.semi[u]
				semi_w := ctx.semi[w]
				if semi_u < 0 || semi_u >= max_id || semi_w < 0 || semi_w >= max_id {
					continue
				}
				if ctx.dfnum[semi_u] < ctx.dfnum[semi_w] {
					ctx.semi[w] = ctx.semi[u]
				}
			}

			// Add w to bucket of its semidominator
			semi_w2 := ctx.semi[w]
			if semi_w2 >= 0 && semi_w2 < max_id {
				ctx.bucket[semi_w2] << w
			}

			// Link to parent in forest
			ctx.link(ctx.parent[w], w)

			// 2. Implicitly compute IDom
			parent_w := ctx.parent[w]
			if parent_w < 0 || parent_w >= max_id {
				continue
			}
			// Drain bucket of parent
			n_bucket := ctx.bucket[parent_w].len
			for bvi in 0 .. n_bucket {
				v := ctx.bucket[parent_w][bvi]
				if v < 0 || v >= max_id {
					continue
				}
				u := ctx.eval(v)
				if u < 0 || u >= max_id || v >= max_id {
					continue
				}
				if ctx.semi[u] == ctx.semi[v] {
					ctx.idom[v] = parent_w
				} else {
					ctx.idom[v] = u // Deferred: idom[v] = idom[u]
				}
			}
			ctx.bucket[parent_w].clear()
		}

		// 3. Explicitly compute IDom
		for i := 2; i <= ctx.n; i++ {
			if i >= ctx.vertex.len {
				continue
			}
			w := ctx.vertex[i]
			if w < 0 || w >= max_id {
				continue
			}
			semi_w := ctx.semi[w]
			if semi_w < 0 || semi_w >= max_id {
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
			if ctx.idom[w] != target {
				idom_w := ctx.idom[w]
				if idom_w >= 0 && idom_w < max_id {
					next_idom := ctx.idom[idom_w]
					if next_idom >= 0 {
						ctx.idom[w] = next_idom
					}
				}
			}
		}

		ctx.idom[entry] = entry

		// Build Dom Tree Children
		for fbi4 in 0 .. n_func_blocks {
			blk_id := func.blocks[fbi4]
			idom := ctx.idom[blk_id]
			if idom != -1 && idom != blk_id {
				if idom >= 0 && idom < max_id {
					ctx.dom_tree[idom] << blk_id
				}
			}
		}
	}

	return DomInfo{
		idom:     ctx.idom
		dom_tree: ctx.dom_tree
	}
}

fn lt_dfs(root int, cfg &CfgData, mut ctx LTContext) {
	if root < 0 || root >= ctx.dfnum.len {
		return
	}
	n_total_blocks := cfg.succs.len

	mut stack := []DfsFrame{}
	// Visit root
	ctx.n++
	if ctx.n >= ctx.vertex.len {
		return
	}
	ctx.dfnum[root] = ctx.n
	ctx.vertex[ctx.n] = root

	stack << DfsFrame{
		node: root
	}

	for stack.len > 0 {
		top := stack.len - 1
		node := stack[top].node
		if node < 0 || node >= n_total_blocks {
			stack.pop()
			continue
		}
		// Use cfg.succs flat array instead of m.blocks[node].succs
		n_succs := cfg.succs[node].len
		si := stack[top].succ_idx
		if si < n_succs {
			// Avoid stack[top].succ_idx++ -- chained field increment broken in ARM64 self-hosted
			mut frame := stack[top]
			frame.succ_idx++
			stack[top] = frame
			w := cfg.succs[node][si]
			if w >= 0 && w < ctx.dfnum.len && ctx.dfnum[w] == 0 {
				ctx.parent[w] = node
				ctx.n++
				if ctx.n >= ctx.vertex.len {
					return
				}
				ctx.dfnum[w] = ctx.n
				ctx.vertex[ctx.n] = w
				stack << DfsFrame{
					node: w
				}
			}
		} else {
			stack.pop()
		}
	}
}

fn (mut ctx LTContext) compress(v int) {
	if v < 0 || v >= ctx.ancestor.len {
		return
	}
	mut chain := []int{}
	mut cur := v
	for cur >= 0 && cur < ctx.ancestor.len {
		av := ctx.ancestor[cur]
		if av < 0 || av >= ctx.ancestor.len {
			break
		}
		if ctx.ancestor[av] == -1 {
			break
		}
		chain << cur
		cur = av
	}
	for ci := chain.len - 1; ci >= 0; ci-- {
		node := chain[ci]
		if node < 0 || node >= ctx.ancestor.len {
			continue
		}
		anc := ctx.ancestor[node]
		if anc < 0 || anc >= ctx.label.len || node >= ctx.label.len {
			continue
		}
		label_anc := ctx.label[anc]
		label_node := ctx.label[node]
		if label_anc < 0 || label_anc >= ctx.semi.len || label_node < 0
			|| label_node >= ctx.semi.len {
			continue
		}
		semi_label_anc := ctx.semi[label_anc]
		semi_label_node := ctx.semi[label_node]
		if semi_label_anc < 0 || semi_label_anc >= ctx.dfnum.len || semi_label_node < 0
			|| semi_label_node >= ctx.dfnum.len {
			continue
		}
		if ctx.dfnum[semi_label_anc] < ctx.dfnum[semi_label_node] {
			ctx.label[node] = ctx.label[anc]
		}
		aav := ctx.ancestor[anc]
		if aav >= 0 {
			ctx.ancestor[node] = aav
		}
	}
}

fn (mut ctx LTContext) eval(v int) int {
	if v < 0 || v >= ctx.ancestor.len {
		if v >= 0 && v < ctx.label.len {
			return ctx.label[v]
		}
		return 0
	}
	if ctx.ancestor[v] == -1 {
		return ctx.label[v]
	}
	ctx.compress(v)
	av := ctx.ancestor[v]
	if av < 0 || av >= ctx.label.len {
		if v < ctx.label.len {
			return ctx.label[v]
		}
		return 0
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
