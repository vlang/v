// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import time

// Optimize Module
pub fn (mut m Module) optimize() {
	t := time.now()
	// 1. Build Control Flow Graph (Predecessors)
	m.build_cfg()

	// 2. Compute Dominator Tree (Lengauer-Tarjan)
	m.compute_dominators()

	// 3. Promote Memory to Register (Construct SSA / Phi Nodes)
	// NOTE: Disabled due to bug with nested loops containing break statements
	// m.promote_memory_to_register()

	// 4. Scalar Optimizations (run until fixed point)
	mut opt_changed := true
	for opt_changed {
		opt_changed = false
		opt_changed = m.constant_fold() || opt_changed
		opt_changed = m.branch_fold() || opt_changed
		opt_changed = m.dead_code_elimination() || opt_changed
		opt_changed = m.simplify_phi_nodes() || opt_changed
	}

	m.merge_blocks()
	m.remove_unreachable_blocks()

	// 6. Eliminate Phi Nodes (Lower to Copies for Backend)
	// This includes Critical Edge Splitting and Briggs Parallel Copy Resolution
	m.eliminate_phi_nodes()

	println('SSA optimization took ${time.since(t)}')
}

// --- 1. CFG Construction ---
fn (mut m Module) build_cfg() {
	// Use a set for deduplication instead of linear search
	mut seen_succs := map[int]bool{}
	mut seen_preds := map[int]bool{}

	for func in m.funcs {
		// Clear existing preds/succs
		for blk_id in func.blocks {
			m.blocks[blk_id].preds = []
			m.blocks[blk_id].succs = []
		}

		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			if blk.instrs.len == 0 {
				continue
			}
			term_val_id := blk.instrs.last()
			term := m.instrs[m.values[term_val_id].index]

			// Clear the set for reuse
			seen_succs.clear()

			match term.op {
				.br {
					s1 := m.get_block_from_val(term.operands[1])
					s2 := m.get_block_from_val(term.operands[2])
					if !seen_succs[s1] {
						seen_succs[s1] = true
						m.blocks[blk_id].succs << s1
					}
					if !seen_succs[s2] {
						seen_succs[s2] = true
						m.blocks[blk_id].succs << s2
					}
				}
				.jmp {
					s := m.get_block_from_val(term.operands[0])
					seen_succs[s] = true
					m.blocks[blk_id].succs << s
				}
				.switch_ {
					// default
					s := m.get_block_from_val(term.operands[1])
					if !seen_succs[s] {
						seen_succs[s] = true
						m.blocks[blk_id].succs << s
					}
					// cases
					for i := 3; i < term.operands.len; i += 2 {
						cs := m.get_block_from_val(term.operands[i])
						if !seen_succs[cs] {
							seen_succs[cs] = true
							m.blocks[blk_id].succs << cs
						}
					}
				}
				else {}
			}

			// Build predecessors - use seen_preds to check if already added
			for s in m.blocks[blk_id].succs {
				// Reset seen_preds for each successor check
				seen_preds.clear()
				for p in m.blocks[s].preds {
					seen_preds[p] = true
				}
				if !seen_preds[blk_id] {
					m.blocks[s].preds << blk_id
				}
			}
		}
	}
}

// --- 2. Dominators (Lengauer-Tarjan) ---

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

fn (mut m Module) compute_dominators() {
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
		m.lt_dfs(entry, mut ctx)

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

fn (mut m Module) lt_dfs(v int, mut ctx LTContext) {
	ctx.n++
	ctx.dfnum[v] = ctx.n
	ctx.vertex[ctx.n] = v

	for w in m.blocks[v].succs {
		if ctx.dfnum[w] == 0 {
			ctx.parent[w] = v
			m.lt_dfs(w, mut ctx)
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

// --- 3. Mem2Reg (Promote Allocas) ---
struct Mem2RegCtx {
mut:
	defs           map[int][]int
	uses           map[int][]int
	phi_placements map[int][]int
	stacks         map[int][]int
}

fn (mut m Module) promote_memory_to_register() {
	for func in m.funcs {
		mut ctx := Mem2RegCtx{
			defs:           map[int][]int{}
			uses:           map[int][]int{}
			phi_placements: map[int][]int{}
			stacks:         map[int][]int{}
		}

		// 1. Analyze Allocas
		mut promotable := []int{}
		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			for val_id in blk.instrs {
				instr := m.instrs[m.values[val_id].index]
				if instr.op == .alloca {
					if m.is_promotable(val_id) {
						promotable << val_id
						ctx.stacks[val_id] = []
					}
				}

				if instr.op == .store {
					ptr := instr.operands[1]
					// Avoid duplicate def entries for same block
					if blk_id !in ctx.defs[ptr] {
						ctx.defs[ptr] << blk_id
					}
				} else if instr.op == .load {
					ptr := instr.operands[0]
					// Avoid duplicate use entries for same block
					if blk_id !in ctx.uses[ptr] {
						ctx.uses[ptr] << blk_id
					}
				}
			}
		}

		// 2. Insert Phis (Dominance Frontier)
		df := m.compute_dominance_frontier(func)

		for alloc_id in promotable {
			mut worklist := ctx.defs[alloc_id].clone()
			mut visited := map[int]bool{}
			mut has_phi := map[int]bool{}

			for worklist.len > 0 {
				b := worklist.pop()
				for d in df[b] {
					if !has_phi[d] {
						ctx.phi_placements[d] << alloc_id
						has_phi[d] = true
						if !visited[d] {
							visited[d] = true
							worklist << d
						}
					}
				}
			}
		}

		// Insert Phis
		for blk_id, allocs in ctx.phi_placements {
			for alloc_id in allocs {
				typ := m.type_store.types[m.values[alloc_id].typ].elem_type
				phi_val := m.add_instr_front(.phi, blk_id, typ, [])
				m.values[phi_val].name = '${m.values[alloc_id].name}.phi'
			}
		}

		// 3. Rename Variables
		if func.blocks.len > 0 {
			entry := func.blocks[0]
			m.rename_recursive(entry, mut ctx)
		}
	}
}

fn (m Module) is_promotable(alloc_id int) bool {
	uses := m.values[alloc_id].uses
	for u in uses {
		if u >= m.values.len {
			continue
		}
		user := m.values[u]
		if user.kind != .instruction {
			return false
		}
		instr := m.instrs[user.index]
		match instr.op {
			.load {
				if instr.operands.len == 0 || instr.operands[0] != alloc_id {
					return false
				}
			}
			.store {
				// Only safe if used as pointer (index 1)
				if instr.operands.len < 2 || instr.operands[1] != alloc_id {
					return false
				}
			}
			else {
				// Escape (GEP, Call, Phi, etc.)
				return false
			}
		}
	}
	return true
}

fn (mut m Module) compute_dominance_frontier(func Function) map[int][]int {
	mut df := map[int][]int{}
	for blk_id in func.blocks {
		preds := m.blocks[blk_id].preds
		if preds.len >= 2 {
			for p in preds {
				mut runner := p
				idom := m.blocks[blk_id].idom
				// Safety check: idom != -1
				for runner != -1 && runner != idom {
					// Avoid duplicate entries in dominance frontier
					if blk_id !in df[runner] {
						df[runner] << blk_id
					}
					if runner == m.blocks[runner].idom {
						break
					}
					runner = m.blocks[runner].idom
				}
			}
		}
	}
	return df
}

fn (mut m Module) rename_recursive(blk_id int, mut ctx Mem2RegCtx) {
	blk := m.blocks[blk_id]

	// 1. Push Phis to stack
	if phis := ctx.phi_placements[blk_id] {
		for alloc_id in phis {
			name := '${m.values[alloc_id].name}.phi'
			for val_id in blk.instrs {
				instr := m.instrs[m.values[val_id].index]
				if instr.op != .phi {
					break
				}
				if m.values[val_id].name == name {
					ctx.stacks[alloc_id] << val_id
					break
				}
			}
		}
	}

	// 2. Process Instructions
	mut stack_counts := map[int]int{}
	for k, v in ctx.stacks {
		stack_counts[k] = v.len
	}

	mut instrs_to_nop := []int{}

	for val_id in blk.instrs {
		instr := m.instrs[m.values[val_id].index]
		match instr.op {
			.store {
				ptr := instr.operands[1]
				val := instr.operands[0]
				if _ := ctx.stacks[ptr] {
					ctx.stacks[ptr] << val
					instrs_to_nop << val_id
				}
			}
			.load {
				ptr := instr.operands[0]
				if stack := ctx.stacks[ptr] {
					mut repl := 0
					if stack.len > 0 {
						repl = stack.last()
					} else {
						// Undef - reading uninitialized memory
						res_type := m.values[val_id].typ
						repl = m.add_value_node(.constant, res_type, 'undef', 0)
					}
					m.replace_uses(val_id, repl)
					instrs_to_nop << val_id
				}
			}
			.alloca {
				if _ := ctx.stacks[val_id] {
					instrs_to_nop << val_id
				}
			}
			else {}
		}
	}

	for vid in instrs_to_nop {
		m.instrs[m.values[vid].index].op = .bitcast
		m.instrs[m.values[vid].index].operands = []
	}

	// 3. Update Successor Phi Operands
	for succ_id in blk.succs {
		if phis := ctx.phi_placements[succ_id] {
			for alloc_id in phis {
				succ_blk := m.blocks[succ_id]
				for vid in succ_blk.instrs {
					v := m.values[vid]
					if v.kind != .instruction {
						continue
					}
					ins := m.instrs[v.index]
					if ins.op == .phi && v.name == '${m.values[alloc_id].name}.phi' {
						mut val := 0
						if ctx.stacks[alloc_id].len > 0 {
							val = ctx.stacks[alloc_id].last()
						} else {
							// Undef - reading uninitialized memory
							typ := m.type_store.types[m.values[alloc_id].typ].elem_type
							val = m.add_value_node(.constant, typ, 'undef', 0)
						}
						m.instrs[v.index].operands << val
						m.instrs[v.index].operands << m.blocks[blk_id].val_id
					}
				}
			}
		}
	}

	// 4. Recurse Dom Children
	for child in blk.dom_tree {
		m.rename_recursive(child, mut ctx)
	}

	// 5. Pop Stacks
	for k, count in stack_counts {
		for ctx.stacks[k].len > count {
			ctx.stacks[k].pop()
		}
	}
}

// --- 4. Simplify Phi Nodes ---
// Remove trivial phi nodes where all operands are the same or self-referential.
// A phi is trivial if all its non-self operands resolve to the same value.
// This reduces unnecessary instructions before phi elimination.
fn (mut m Module) simplify_phi_nodes() bool {
	mut any_changed := false
	mut changed := true
	for changed {
		changed = false
		for func in m.funcs {
			for blk_id in func.blocks {
				for val_id in m.blocks[blk_id].instrs {
					if m.values[val_id].kind != .instruction {
						continue
					}
					instr := m.instrs[m.values[val_id].index]
					if instr.op != .phi {
						continue
					}

					// Check if phi is trivial (all non-self operands are the same)
					mut unique_val := -1
					mut is_trivial := true

					for i := 0; i < instr.operands.len; i += 2 {
						op_val := instr.operands[i]
						// Skip self-references (phi refers to itself)
						if op_val == val_id {
							continue
						}
						if unique_val == -1 {
							unique_val = op_val
						} else if unique_val != op_val {
							is_trivial = false
							break
						}
					}

					// If trivial and we found a unique value, replace all uses
					if is_trivial && unique_val != -1 {
						// Replace all uses of this phi with the unique value
						m.replace_uses(val_id, unique_val)
						// Mark phi as dead (will be cleaned up by DCE or ignored)
						m.instrs[m.values[val_id].index].op = .bitcast
						m.instrs[m.values[val_id].index].operands = []
						changed = true
						any_changed = true
					}
				}
			}
		}
	}
	return any_changed
}

// --- 5. Critical Edge Splitting ---
// A critical edge is an edge from a block with multiple successors to a block
// with multiple predecessors. We must split these edges to correctly place
// phi copies during phi elimination.
//
// Example:
//     A (2+ succs)                     A (2+ succs)
//    / \                              / \
//   /   \           becomes          /   \
//  B     C (2+ preds)               B    split_A_C
//                                          |
//                                          C
//
// This ensures that copies inserted for C's phis from A don't affect B's path.
fn (mut m Module) split_critical_edges() {
	m.build_cfg()

	for mut func in m.funcs {
		mut new_blocks := []BlockID{}

		// Collect edges to split (can't modify while iterating)
		mut edges_to_split := [][]BlockID{} // [pred_id, succ_id]

		// Find all critical edges
		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			if blk.succs.len > 1 {
				for succ_id in blk.succs {
					if m.blocks[succ_id].preds.len > 1 {
						edges_to_split << [blk_id, succ_id]
					}
				}
			}
		}

		// Split each critical edge
		for edge in edges_to_split {
			pred_id := edge[0]
			succ_id := edge[1]

			// Create new intermediate block
			split_blk := m.add_block(func.id, 'split_${pred_id}_${succ_id}')
			new_blocks << split_blk

			// Add unconditional jump from split block to original successor
			succ_val := m.blocks[succ_id].val_id
			m.add_instr(.jmp, split_blk, 0, [succ_val])

			// Update predecessor's terminator to jump to split block instead of successor
			pred_blk := m.blocks[pred_id]
			if pred_blk.instrs.len > 0 {
				term_val_id := pred_blk.instrs.last()
				mut term := &m.instrs[m.values[term_val_id].index]

				old_succ_val := m.blocks[succ_id].val_id
				new_succ_val := m.blocks[split_blk].val_id

				// Replace ALL occurrences (handles switch with duplicate targets)
				for i in 0 .. term.operands.len {
					if term.operands[i] == old_succ_val {
						term.operands[i] = new_succ_val
					}
				}
			}

			// Update phi nodes in successor to reference split block instead of pred
			for val_id in m.blocks[succ_id].instrs {
				if m.values[val_id].kind != .instruction {
					continue
				}
				mut instr := &m.instrs[m.values[val_id].index]
				if instr.op == .phi {
					old_pred_val := m.blocks[pred_id].val_id
					new_pred_val := m.blocks[split_blk].val_id
					// Replace all occurrences (defensive - handles edge cases)
					for i := 1; i < instr.operands.len; i += 2 {
						if instr.operands[i] == old_pred_val {
							instr.operands[i] = new_pred_val
						}
					}
				}
			}
		}
		// Note: new blocks are already added to func.blocks by add_block()
	}

	// Rebuild CFG after splitting
	m.build_cfg()
}

// --- 6. Phi Elimination with Briggs Parallel Copy Resolution ---
//
// When eliminating phi nodes, we need to insert copy instructions in predecessor
// blocks. However, multiple phis at a join point create "parallel copies" that
// must all read their sources before any destination is written.
//
// Example problem:
//   phi a = [b, pred], ...
//   phi b = [a, pred], ...
//
// Naive sequential: a <- b; b <- a  // WRONG: second copy reads new 'a'
// Correct parallel: both read old values, then both write
//
// Briggs' algorithm sequences copies to handle:
// 1. Dependencies: emit a <- b before c <- a
// 2. Cycles: use temporary to break a <- b, b <- a cycles

// Helper struct for parallel copy resolution
struct ParallelCopy {
	dest int
	src  int
}

fn (mut m Module) eliminate_phi_nodes() {
	// First split critical edges to ensure correct copy placement
	m.split_critical_edges()

	for func in m.funcs {
		// Collect all phi copies grouped by predecessor block
		// Map: pred_block -> list of (dest, src) pairs
		mut pred_copies := map[int][]ParallelCopy{}

		for blk_id in func.blocks {
			for val_id in m.blocks[blk_id].instrs {
				if m.values[val_id].kind != .instruction {
					continue
				}
				instr := m.instrs[m.values[val_id].index]
				if instr.op == .phi {
					// Phi operands: [val0, blk0, val1, blk1, ...]
					for i := 0; i < instr.operands.len; i += 2 {
						val_in := instr.operands[i]
						blk_val := instr.operands[i + 1]
						pred_blk_idx := m.values[blk_val].index

						pred_copies[pred_blk_idx] << ParallelCopy{
							dest: val_id
							src:  val_in
						}
					}
				}
			}
		}

		// For each predecessor, resolve parallel copies using Briggs algorithm
		for pred_blk, copies in pred_copies {
			m.resolve_parallel_copies_briggs(pred_blk, copies)
		}

		// Remove phi instructions (mark as nop/bitcast with no operands)
		for blk_id in func.blocks {
			for val_id in m.blocks[blk_id].instrs {
				if m.values[val_id].kind != .instruction {
					continue
				}
				if m.instrs[m.values[val_id].index].op == .phi {
					m.instrs[m.values[val_id].index].op = .bitcast
					m.instrs[m.values[val_id].index].operands = []
				}
			}
		}
	}
}

// Briggs Parallel Copy Resolution Algorithm
//
// Sequences parallel copies to handle dependencies and cycles correctly.
// Based on: Briggs et al., "Practical Improvements to the Construction and
// Destruction of Static Single Assignment Form", SPE 1998.
//
// Algorithm overview:
// 1. Build worklist of (dest, src) pairs, filtering self-copies
// 2. Track loc[v] = current location of value originally in v
// 3. Track pred[d] = source value needed by destination d
// 4. Ready set = destinations whose sources are not themselves destinations
// 5. Loop:
//    - If ready non-empty: emit copy, update locations, check for newly ready
//    - Else (cycle): use temp to break cycle, add to ready
//
// Example cycle resolution for a <- b, b <- a:
//   temp <- b; a <- temp; b <- a
fn (mut m Module) resolve_parallel_copies_briggs(blk_id int, copies []ParallelCopy) {
	if copies.len == 0 {
		return
	}

	// Filter out self-copies (dest == src) and build working set
	mut worklist := []ParallelCopy{}
	for copy in copies {
		if copy.dest != copy.src {
			worklist << copy
		}
	}

	if worklist.len == 0 {
		return
	}

	// loc[b] = current location of value originally in b
	// pred[a] = the source for destination a
	mut loc := map[int]int{}
	mut pred := map[int]int{}

	// Set of destinations that still need to be written
	mut to_do := map[int]bool{}

	// Initialize tracking structures
	for copy in worklist {
		loc[copy.src] = copy.src // Initially, value is at its original location
		pred[copy.dest] = copy.src // Destination needs this source
		to_do[copy.dest] = true // Mark as pending
	}

	// Ready set: destinations whose sources are not themselves destinations
	// These can be safely copied without overwriting needed values
	mut ready := []int{}

	for copy in worklist {
		// A copy is ready if its source is not a destination of another copy
		if !to_do[copy.src] {
			ready << copy.dest
		}
	}

	// Sequenced copies to emit
	mut sequenced := []ParallelCopy{}

	// Main loop: process until all copies are sequenced
	for to_do.len > 0 {
		if ready.len > 0 {
			// Process a ready copy
			b := ready.pop() // Destination
			a := pred[b] // Source value needed
			c := loc[a] // Current location of that value

			// Emit: b <- c (copy from current location of a to b)
			sequenced << ParallelCopy{
				dest: b
				src:  c
			}

			// Update location: value originally at a is now at b
			loc[a] = b
			to_do.delete(b)

			// Check if this makes another copy ready
			// If 'a' was a destination waiting for its source, check if it's now ready
			if to_do[a] {
				// 'a' is ready if its source's current location is not a pending destination
				src_of_a := pred[a]
				loc_of_src := if l := loc[src_of_a] { l } else { src_of_a }
				if !to_do[loc_of_src] {
					ready << a
				}
			}
		} else {
			// No ready copies means we have a cycle
			// Break the cycle by saving one value to a temporary

			// Pick any remaining destination
			mut b := 0
			for dest, _ in to_do {
				b = dest
				break
			}

			// Create a temporary to hold the value at b's current location
			typ := m.values[b].typ
			temp := m.add_value_node(.instruction, typ, 'phi_tmp_${m.values.len}', 0)

			// Save the current location of b to temp
			c := loc[b]
			sequenced << ParallelCopy{
				dest: temp
				src:  c
			}

			// Update: value originally at b is now at temp
			loc[b] = temp

			// Now b should be ready (its source location is temp, not a pending dest)
			ready << b
		}
	}

	// Emit the sequenced copies as assign instructions in the predecessor block
	for copy in sequenced {
		m.insert_copy_in_block(blk_id, copy.dest, copy.src)
	}
}

fn (mut m Module) insert_copy_in_block(blk_id int, dest int, src int) {
	typ := m.values[dest].typ
	m.instrs << Instruction{
		op:       .assign
		block:    blk_id
		typ:      typ
		operands: [ValueID(dest), src]
	}
	val_id := m.add_value_node(.instruction, typ, 'copy', m.instrs.len - 1)

	// Safe insertion: find terminator and insert before it
	mut insert_idx := m.blocks[blk_id].instrs.len
	if insert_idx > 0 {
		last_val := m.blocks[blk_id].instrs.last()
		last_instr := m.instrs[m.values[last_val].index]

		if last_instr.op in [.ret, .br, .jmp, .switch_, .unreachable] {
			insert_idx = m.blocks[blk_id].instrs.len - 1
		}
	}

	m.blocks[blk_id].instrs.insert(insert_idx, val_id)
}

fn (mut m Module) constant_fold() bool {
	mut changed := false
	for func in m.funcs {
		for blk_id in func.blocks {
			// Iterate directly without cloning - we don't modify the array during iteration
			for val_id in m.blocks[blk_id].instrs {
				if m.values[val_id].kind != .instruction {
					continue
				}

				instr := m.instrs[m.values[val_id].index]

				if instr.operands.len == 2 {
					lhs := m.values[instr.operands[0]]
					rhs := m.values[instr.operands[1]]

					// Skip undef values - can't fold with undefined
					if lhs.kind == .constant && lhs.name == 'undef' {
						continue
					}
					if rhs.kind == .constant && rhs.name == 'undef' {
						continue
					}

					// Try algebraic simplifications first (even with non-constants)
					repl, needs_zero := m.try_algebraic_simplify(val_id, instr, lhs, rhs)
					if repl >= 0 {
						if needs_zero {
							// x * 0 or x & 0 - create zero constant
							typ := m.values[val_id].typ
							zero_val := m.add_value_node(.constant, typ, '0', 0)
							m.replace_uses(val_id, zero_val)
						} else {
							m.replace_uses(val_id, repl)
						}
						changed = true
						continue
					}

					// Constant folding - both operands must be constants
					if lhs.kind == .constant && rhs.kind == .constant {
						l_int := lhs.name.i64()
						r_int := rhs.name.i64()

						mut result := i64(0)
						mut folded := false

						match instr.op {
							.add {
								result = l_int + r_int
								folded = true
							}
							.sub {
								result = l_int - r_int
								folded = true
							}
							.mul {
								result = l_int * r_int
								folded = true
							}
							.sdiv {
								if r_int != 0 {
									result = l_int / r_int
									folded = true
								}
							}
							.srem {
								if r_int != 0 {
									result = l_int % r_int
									folded = true
								}
							}
							.and_ {
								result = l_int & r_int
								folded = true
							}
							.or_ {
								result = l_int | r_int
								folded = true
							}
							.xor {
								result = l_int ^ r_int
								folded = true
							}
							.shl {
								if r_int >= 0 && r_int < 64 {
									result = i64(u64(l_int) << u64(r_int))
									folded = true
								}
							}
							.ashr {
								if r_int >= 0 && r_int < 64 {
									// Arithmetic shift right preserves sign
									result = l_int >> u64(r_int)
									folded = true
								}
							}
							.lshr {
								if r_int >= 0 && r_int < 64 {
									// Logical shift right treats as unsigned
									result = i64(u64(l_int) >> u64(r_int))
									folded = true
								}
							}
							.eq {
								result = if l_int == r_int { 1 } else { 0 }
								folded = true
							}
							.ne {
								result = if l_int != r_int { 1 } else { 0 }
								folded = true
							}
							.lt {
								result = if l_int < r_int { 1 } else { 0 }
								folded = true
							}
							.gt {
								result = if l_int > r_int { 1 } else { 0 }
								folded = true
							}
							.le {
								result = if l_int <= r_int { 1 } else { 0 }
								folded = true
							}
							.ge {
								result = if l_int >= r_int { 1 } else { 0 }
								folded = true
							}
							else {}
						}

						if folded {
							typ := m.values[val_id].typ
							const_val := m.add_value_node(.constant, typ, result.str(),
								0)
							m.replace_uses(val_id, const_val)
							changed = true
						}
					}
				}
			}
		}
	}
	return changed
}

// Branch folding: simplify conditional branches with constant conditions
fn (mut m Module) branch_fold() bool {
	mut changed := false
	for func in m.funcs {
		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			if blk.instrs.len == 0 {
				continue
			}

			term_val_id := blk.instrs.last()
			term := m.instrs[m.values[term_val_id].index]

			if term.op == .br {
				// br cond, true_blk, false_blk
				cond_val := m.values[term.operands[0]]
				if cond_val.kind == .constant && cond_val.name != 'undef' {
					cond_int := cond_val.name.i64()
					// Replace with unconditional jump to the taken branch
					target := if cond_int != 0 { term.operands[1] } else { term.operands[2] }
					m.instrs[m.values[term_val_id].index].op = .jmp
					m.instrs[m.values[term_val_id].index].operands = [target]
					changed = true
				}
			}
		}
	}
	return changed
}

// Algebraic simplifications: x+0=x, x*1=x, x*0=0, etc.
// Returns (replacement_id, needs_zero) - if needs_zero is true, caller should create zero constant
fn (m Module) try_algebraic_simplify(val_id int, instr Instruction, lhs Value, rhs Value) (int, bool) {
	// Check if either operand is a constant
	mut const_val := i64(0)
	mut const_is_rhs := false
	mut other_id := 0

	if lhs.kind == .constant && lhs.name != 'undef' {
		const_val = lhs.name.i64()
		const_is_rhs = false
		other_id = instr.operands[1]
	} else if rhs.kind == .constant && rhs.name != 'undef' {
		const_val = rhs.name.i64()
		const_is_rhs = true
		other_id = instr.operands[0]
	} else {
		return -1, false
	}

	match instr.op {
		.add {
			// x + 0 = 0 + x = x
			if const_val == 0 {
				return other_id, false
			}
		}
		.sub {
			// x - 0 = x
			if const_is_rhs && const_val == 0 {
				return other_id, false
			}
		}
		.mul {
			// x * 0 = 0 * x = 0
			if const_val == 0 {
				return val_id, true // Signal caller to create zero
			}
			// x * 1 = 1 * x = x
			if const_val == 1 {
				return other_id, false
			}
		}
		.sdiv {
			// x / 1 = x
			if const_is_rhs && const_val == 1 {
				return other_id, false
			}
		}
		.and_ {
			// x & 0 = 0 & x = 0
			if const_val == 0 {
				return val_id, true // Signal caller to create zero
			}
		}
		.or_ {
			// x | 0 = 0 | x = x
			if const_val == 0 {
				return other_id, false
			}
		}
		.xor {
			// x ^ 0 = 0 ^ x = x
			if const_val == 0 {
				return other_id, false
			}
		}
		.shl, .ashr, .lshr {
			// x << 0 = x >> 0 = x
			if const_is_rhs && const_val == 0 {
				return other_id, false
			}
		}
		else {}
	}

	return -1, false
}

fn (mut m Module) dead_code_elimination() bool {
	mut any_changed := false
	mut changed := true
	for changed {
		changed = false
		for func in m.funcs {
			for blk_id in func.blocks {
				// First pass: find dead instructions (don't modify yet)
				mut dead_instrs := []int{}
				for val_id in m.blocks[blk_id].instrs {
					val := m.values[val_id]
					if val.kind == .instruction {
						instr := m.instrs[val.index]
						side_effects := instr.op in [.store, .call, .call_indirect, .ret, .br,
							.jmp, .switch_, .unreachable, .assign, .fence, .atomicrmw]
						if !side_effects && val.uses.len == 0 {
							dead_instrs << val_id
						}
					}
				}

				// Only rebuild the instruction list if we found dead instructions
				if dead_instrs.len > 0 {
					// Remove uses from dead instructions
					for val_id in dead_instrs {
						instr := m.instrs[m.values[val_id].index]
						for op_id in instr.operands {
							m.remove_use(op_id, val_id)
						}
					}

					// Build set for O(1) lookup
					mut dead_set := map[int]bool{}
					for d in dead_instrs {
						dead_set[d] = true
					}

					// Filter out dead instructions
					mut new_instrs := []int{cap: m.blocks[blk_id].instrs.len}
					for val_id in m.blocks[blk_id].instrs {
						if !dead_set[val_id] {
							new_instrs << val_id
						}
					}
					m.blocks[blk_id].instrs = new_instrs
					changed = true
					any_changed = true
				}
			}
		}
	}
	return any_changed
}

fn (mut m Module) remove_use(val_id int, user_id int) {
	if val_id >= m.values.len {
		return
	}
	mut val := &m.values[val_id]
	// Remove all occurrences (handles instructions that use same value multiple times)
	// Iterate in reverse to safely delete while iterating
	for i := val.uses.len - 1; i >= 0; i-- {
		if val.uses[i] == user_id {
			val.uses.delete(i)
		}
	}
}

fn (mut m Module) remove_unreachable_blocks() {
	// Re-build CFG first
	m.build_cfg()
	for mut func in m.funcs {
		if func.blocks.len == 0 {
			continue
		}
		// BFS/DFS from entry
		mut reachable := map[int]bool{}
		mut q := [func.blocks[0]]
		reachable[func.blocks[0]] = true

		for q.len > 0 {
			curr := q.pop()
			for succ in m.blocks[curr].succs {
				if !reachable[succ] {
					reachable[succ] = true
					q << succ
				}
			}
		}

		mut new_blocks := []int{}
		for blk in func.blocks {
			if reachable[blk] {
				new_blocks << blk
			}
		}
		func.blocks = new_blocks
	}
}

fn (mut m Module) merge_blocks() {
	// If Block A jumps unconditionally to B, and B has only A as predecessor:
	// 1. Move instructions from B to A
	// 2. Update A's successors to B's successors
	// 3. Remove B

	// We need to be careful about iteration while modifying.
	// Loop until no changes.
	mut changed := true
	mut first_iter := true
	for changed {
		changed = false
		// Only rebuild CFG on first iteration or after actual changes
		if first_iter {
			m.build_cfg()
			first_iter = false
		}

		for mut func in m.funcs {
			// We iterate through blocks.
			// If we merge A->B, we can't merge B->C in same pass easily.
			mut merged := map[int]bool{}

			for blk_id in func.blocks {
				if merged[blk_id] {
					continue
				}
				blk := m.blocks[blk_id]

				// Check if unconditional jump
				if blk.instrs.len > 0 {
					last_val := blk.instrs.last()
					last_instr := m.instrs[m.values[last_val].index]

					if last_instr.op == .jmp {
						target_val := last_instr.operands[0]
						target_id := m.get_block_from_val(target_val)

						// Candidate: target_id
						if target_id != blk_id && m.blocks[target_id].preds.len == 1
							&& m.blocks[target_id].preds[0] == blk_id {
							// MERGE
							// Remove JMP from A
							m.blocks[blk_id].instrs.delete_last()

							// Append B's instrs to A
							m.blocks[blk_id].instrs << m.blocks[target_id].instrs

							// Update instructions in B to point to A (for their 'block' field)?
							// Not strictly needed if we just use the list.
							// But we need to update Phis in successors of B?
							// If B has successors, their Phis might refer to B.
							// Since B is gone, they now refer to A.
							for succ_id in m.blocks[target_id].succs {
								succ := m.blocks[succ_id]
								for iv in succ.instrs {
									v := m.values[iv]
									if v.kind != .instruction {
										continue
									}
									ins := m.instrs[v.index]
									if ins.op == .phi {
										// Replace all occurrences (defensive - handles edge cases)
										// i=1,3,5... are block references in phi [val0, blk0, val1, blk1, ...]
										for i := 1; i < ins.operands.len; i += 2 {
											if ins.operands[i] == m.blocks[target_id].val_id {
												m.instrs[v.index].operands[i] = m.blocks[blk_id].val_id
											}
										}
									}
								}
							}

							// Remove B from func
							merged[target_id] = true
							changed = true
						}
					}
				}
			}

			// Filter out merged blocks
			if merged.len > 0 {
				mut new_blks := []int{}
				for b in func.blocks {
					if !merged[b] {
						new_blks << b
					}
				}
				func.blocks = new_blks
			}
		}
		// Rebuild CFG for next iteration if we made changes
		if changed {
			m.build_cfg()
		}
	}
}
