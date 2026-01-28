// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// --- Simplify Phi Nodes ---
// Remove trivial phi nodes where all operands are the same or self-referential.
// A phi is trivial if all its non-self operands resolve to the same value.
// This reduces unnecessary instructions before phi elimination.
fn simplify_phi_nodes(mut m ssa.Module) bool {
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

					// Dead phi removal: phi with no uses can be removed
					if m.values[val_id].uses.len == 0 {
						// Remove uses from this phi's operands
						for i := 0; i < instr.operands.len; i += 2 {
							op_val := instr.operands[i]
							if op_val != val_id { // Don't try to remove self-reference
								remove_phi_use(mut m, op_val, val_id)
							}
						}
						// Mark phi as dead
						m.instrs[m.values[val_id].index].op = .bitcast
						m.instrs[m.values[val_id].index].operands = []
						changed = true
						any_changed = true
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

// Helper to remove a use from a value's uses list (for dead phi removal)
fn remove_phi_use(mut m ssa.Module, val_id int, user_id int) {
	if val_id >= m.values.len {
		return
	}
	mut val := &m.values[val_id]
	for i := val.uses.len - 1; i >= 0; i-- {
		if val.uses[i] == user_id {
			val.uses.delete(i)
		}
	}
}

// --- Critical Edge Splitting ---
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
fn split_critical_edges(mut m ssa.Module) {
	build_cfg(mut m)

	for mut func in m.funcs {
		mut new_blocks := []ssa.BlockID{}

		// Collect edges to split (can't modify while iterating)
		mut edges_to_split := [][]ssa.BlockID{} // [pred_id, succ_id]

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
	build_cfg(mut m)
}

// --- Phi Elimination with Briggs Parallel Copy Resolution ---
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

fn eliminate_phi_nodes(mut m ssa.Module) {
	// First split critical edges to ensure correct copy placement
	split_critical_edges(mut m)

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
			resolve_parallel_copies_briggs(mut m, pred_blk, copies)
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
fn resolve_parallel_copies_briggs(mut m ssa.Module, blk_id int, copies []ParallelCopy) {
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
		insert_copy_in_block(mut m, blk_id, copy.dest, copy.src)
	}
}

fn insert_copy_in_block(mut m ssa.Module, blk_id int, dest int, src int) {
	typ := m.values[dest].typ
	m.instrs << ssa.Instruction{
		op:       .assign
		block:    blk_id
		typ:      typ
		operands: [ssa.ValueID(dest), src]
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
