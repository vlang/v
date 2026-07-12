module optimize

import v3.ssa

// --- Simplify trivial phi nodes ---
// A phi is trivial if all its non-self operands resolve to a single value, or if
// it has no uses at all. Trivial phis are replaced and marked dead (nop bitcast).
fn simplify_phi_nodes(mut m ssa.Module) bool {
	mut any_changed := false
	mut changed := true
	for changed {
		changed = false
		for fi in 0 .. m.funcs.len {
			func := m.funcs[fi]
			for blk_id in func.blocks {
				blk := m.blocks[blk_id]
				for val_id in blk.instrs {
					val := m.values[val_id]
					if val.kind != .instruction {
						continue
					}
					instr := m.instrs[val.index]
					if instr.op != .phi {
						continue
					}

					// Dead phi: no uses -> remove.
					if val.uses.len == 0 {
						for i := 0; i < instr.operands.len; i += 2 {
							op_val := instr.operands[i]
							if op_val != val_id {
								remove_use(mut m, op_val, val_id)
							}
						}
						mut dead_phi := m.instrs[val.index]
						dead_phi.op = .bitcast
						dead_phi.operands = []
						m.instrs[val.index] = dead_phi
						changed = true
						any_changed = true
						continue
					}

					// Trivial phi: all non-self value operands identical.
					mut unique_val := -1
					mut is_trivial := true
					for i := 0; i < instr.operands.len; i += 2 {
						op_val := instr.operands[i]
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
					if is_trivial && unique_val != -1 {
						m.replace_uses(val_id, unique_val)
						mut triv_phi := m.instrs[val.index]
						triv_phi.op = .bitcast
						triv_phi.operands = []
						m.instrs[val.index] = triv_phi
						changed = true
						any_changed = true
					}
				}
			}
		}
	}
	return any_changed
}

// prune_phi_operands drops phi operand pairs whose predecessor block is no longer
// an actual predecessor (e.g. after branch folding / unreachable-block removal),
// keeping phis consistent with the current CFG. A phi reduced to a single distinct
// incoming value is replaced by that value. Requires up-to-date preds + uses.
fn prune_phi_operands(mut m ssa.Module) {
	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			preds := m.blocks[blk_id].preds
			for val_id in m.blocks[blk_id].instrs {
				if val_id <= 0 || val_id >= m.values.len || m.values[val_id].kind != .instruction {
					continue
				}
				idx := m.values[val_id].index
				if m.instrs[idx].op != .phi {
					continue
				}
				ops := m.instrs[idx].operands
				mut kept := []ssa.ValueID{}
				mut distinct := []ssa.ValueID{}
				for i := 0; i + 1 < ops.len; i += 2 {
					pred := int(ops[i + 1])
					if pred in preds {
						kept << ops[i]
						kept << ops[i + 1]
						if ops[i] !in distinct {
							distinct << ops[i]
						}
					}
				}
				if kept.len == ops.len && distinct.len > 1 {
					continue // unchanged, still a real phi
				}
				if distinct.len == 1 {
					// Trivial phi -> its single incoming value.
					m.replace_uses(val_id, distinct[0])
					mut nop := m.instrs[idx]
					nop.op = .bitcast
					nop.operands = []
					m.instrs[idx] = nop
				} else if distinct.len == 0 {
					mut nop := m.instrs[idx]
					nop.op = .bitcast
					nop.operands = []
					m.instrs[idx] = nop
				} else {
					mut pruned := m.instrs[idx]
					pruned.operands = kept
					m.instrs[idx] = pruned
				}
			}
		}
	}
}

// --- Critical edge splitting ---
// Inserts an empty block on every edge whose source has multiple successors and
// whose target has multiple predecessors, so phi-elimination copies have a unique
// home. Operands here are raw block ids (v3 convention).
fn split_critical_edges(mut m ssa.Module) {
	build_cfg(mut m)
	for fi in 0 .. m.funcs.len {
		mut edges_to_split := [][]int{}
		func := m.funcs[fi]
		for blk_id in func.blocks {
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			if m.blocks[blk_id].succs.len > 1 {
				for succ_id in m.blocks[blk_id].succs {
					if succ_id < 0 || succ_id >= m.blocks.len {
						continue
					}
					if m.blocks[succ_id].preds.len > 1 {
						edges_to_split << [int(blk_id), int(succ_id)]
					}
				}
			}
		}

		for edge in edges_to_split {
			pred_id := edge[0]
			succ_id := edge[1]
			if pred_id < 0 || pred_id >= m.blocks.len || succ_id < 0 || succ_id >= m.blocks.len {
				continue
			}
			split_blk := m.add_block(fi, 'split_${pred_id}_${succ_id}')
			m.add_instr(.jmp, split_blk, 0, [ssa.ValueID(succ_id)])

			// Re-point predecessor terminator from succ to the split block.
			pred_blk := m.blocks[pred_id]
			if pred_blk.instrs.len > 0 {
				term_val_id := pred_blk.instrs[pred_blk.instrs.len - 1]
				if term_val_id >= 0 && term_val_id < m.values.len {
					idx := m.values[term_val_id].index
					mut term := m.instrs[idx]
					for i in 0 .. term.operands.len {
						if int(term.operands[i]) == succ_id {
							term.operands[i] = ssa.ValueID(split_blk)
						}
					}
					m.instrs[idx] = term
				}
			}

			// Re-point phi predecessors in succ from pred to the split block.
			for vid in m.blocks[succ_id].instrs {
				if vid < 0 || vid >= m.values.len || m.values[vid].kind != .instruction {
					continue
				}
				pidx := m.values[vid].index
				if m.instrs[pidx].op != .phi {
					continue
				}
				mut phi := m.instrs[pidx]
				for i := 1; i < phi.operands.len; i += 2 {
					if int(phi.operands[i]) == pred_id {
						phi.operands[i] = ssa.ValueID(split_blk)
					}
				}
				m.instrs[pidx] = phi
			}
		}
	}
	build_cfg(mut m)
}

// eliminate_phi_nodes lowers phis to `assign` copies placed in predecessor blocks,
// sequencing the per-predecessor parallel copies (Briggs) to respect dependencies.
fn eliminate_phi_nodes(mut m ssa.Module) {
	split_critical_edges(mut m)

	n_blocks := m.blocks.len
	mut pred_copy_dests := [][]int{len: n_blocks}
	mut pred_copy_srcs := [][]int{len: n_blocks}

	for fi in 0 .. m.funcs.len {
		func := m.funcs[fi]
		mut pred_copy_blocks := []int{}

		for blk_id in func.blocks {
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			for val_id in m.blocks[blk_id].instrs {
				if val_id < 0 || val_id >= m.values.len {
					continue
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				instr := m.instrs[val.index]
				if instr.op != .phi {
					continue
				}
				for i := 0; i + 1 < instr.operands.len; i += 2 {
					val_in := instr.operands[i]
					pred_blk_idx := int(instr.operands[i + 1])
					if pred_blk_idx < 0 || pred_blk_idx >= n_blocks {
						continue
					}
					if pred_copy_dests[pred_blk_idx].len == 0 {
						pred_copy_blocks << pred_blk_idx
					}
					array2d_append(mut pred_copy_dests, pred_blk_idx, val_id)
					array2d_append(mut pred_copy_srcs, pred_blk_idx, val_in)
				}
			}
		}

		for pred_blk in pred_copy_blocks {
			resolve_parallel_copies(mut m, pred_blk, pred_copy_dests[pred_blk],
				pred_copy_srcs[pred_blk])
		}
		for pred_blk in pred_copy_blocks {
			pred_copy_dests[pred_blk] = []
			pred_copy_srcs[pred_blk] = []
		}

		// Remove phi instructions (nop them).
		for blk_id in func.blocks {
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			for val_id in m.blocks[blk_id].instrs {
				if val_id < 0 || val_id >= m.values.len {
					continue
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				idx := val.index
				if m.instrs[idx].op == .phi {
					mut cleanup := m.instrs[idx]
					cleanup.op = .bitcast
					cleanup.operands = []
					m.instrs[idx] = cleanup
				}
			}
		}
	}
}

// resolve_parallel_copies sequences a set of parallel copies dest[i] = src[i]
// using Briggs' worklist algorithm, breaking cycles with a temp.
fn resolve_parallel_copies(mut m ssa.Module, blk_id int, dests []int, srcs []int) {
	if dests.len == 0 {
		return
	}
	mut p_dest := []int{}
	mut p_src := []int{}
	for ci in 0 .. dests.len {
		if dests[ci] != srcs[ci] {
			p_dest << dests[ci]
			p_src << srcs[ci]
		}
	}
	np := p_dest.len
	if np == 0 {
		return
	}

	mut max_id := m.values.len
	for i in 0 .. np {
		if p_dest[i] >= max_id {
			max_id = p_dest[i] + 1
		}
		if p_src[i] >= max_id {
			max_id = p_src[i] + 1
		}
	}
	mut src_ref_count := []int{len: max_id + np + 4}

	for i in 0 .. np {
		src_ref_count[p_src[i]]++
	}

	mut alive := []int{len: np}
	for i in 0 .. np {
		alive[i] = 1
	}

	mut worklist := []int{}
	for i in 0 .. np {
		if src_ref_count[p_dest[i]] == 0 {
			worklist << i
		}
	}

	mut s_dest := []int{}
	mut s_src := []int{}
	mut remaining := np

	for remaining > 0 {
		if worklist.len > 0 {
			idx := worklist.pop()
			if idx < 0 || idx >= np || alive[idx] == 0 {
				continue
			}
			alive[idx] = 0
			remaining--
			d := p_dest[idx]
			s := p_src[idx]
			s_dest << d
			s_src << s
			if src_ref_count[s] > 0 {
				src_ref_count[s]--
			}
			if src_ref_count[s] == 0 {
				for j in 0 .. np {
					if alive[j] == 1 && p_dest[j] == s {
						worklist << j
					}
				}
			}
			continue
		}

		// Cycle: break it by copying one source into a temp.
		mut ci := -1
		for k in 0 .. np {
			if alive[k] == 1 {
				ci = k
				break
			}
		}
		if ci < 0 {
			break
		}
		cycle_src := p_src[ci]
		mut typ := 0
		if cycle_src >= 0 && cycle_src < m.values.len {
			typ = m.values[cycle_src].typ
		}
		if typ == 0 && p_dest[ci] >= 0 && p_dest[ci] < m.values.len {
			typ = m.values[p_dest[ci]].typ
		}
		temp := insert_temp_in_block(mut m, blk_id, cycle_src, typ)
		for temp >= src_ref_count.len {
			src_ref_count << 0
		}
		src_ref_count[temp] = 0
		for i in 0 .. np {
			if alive[i] == 1 && p_src[i] == cycle_src {
				p_src[i] = temp
				src_ref_count[temp]++
			}
		}
		src_ref_count[cycle_src] = 0
		for j in 0 .. np {
			if alive[j] == 1 && p_dest[j] == cycle_src {
				worklist << j
			}
		}
	}

	for si in 0 .. s_dest.len {
		insert_copy_in_block(mut m, blk_id, s_dest[si], s_src[si])
	}
}

// insert_temp_in_block updates insert temp in block state for optimize.
fn insert_temp_in_block(mut m ssa.Module, blk_id int, src int, typ int) int {
	m.instrs << ssa.Instruction{
		op:       .bitcast
		block:    blk_id
		typ:      typ
		operands: [ssa.ValueID(src)]
	}
	temp_id := m.add_value(.instruction, typ, 'phi_tmp_${m.values.len}', m.instrs.len - 1)
	insert_before_terminator(mut m, blk_id, temp_id)
	return temp_id
}

// insert_copy_in_block updates insert copy in block state for optimize.
fn insert_copy_in_block(mut m ssa.Module, blk_id int, dest int, src int) {
	typ := m.values[dest].typ
	m.instrs << ssa.Instruction{
		op:       .assign
		block:    blk_id
		typ:      typ
		operands: [ssa.ValueID(dest), ssa.ValueID(src)]
	}
	val_id := m.add_value(.instruction, typ, 'copy', m.instrs.len - 1)
	insert_before_terminator(mut m, blk_id, val_id)
}

// insert_before_terminator updates insert before terminator state for optimize.
fn insert_before_terminator(mut m ssa.Module, blk_id int, new_val int) {
	mut blk := m.blocks[blk_id]
	mut insert_idx := blk.instrs.len
	if insert_idx > 0 {
		last_val_id := blk.instrs[blk.instrs.len - 1]
		last_val := m.values[last_val_id]
		last_instr := m.instrs[last_val.index]
		if last_instr.op in [.ret, .br, .jmp, .switch_, .unreachable] {
			insert_idx = blk.instrs.len - 1
		}
	}
	mut new_instrs := []ssa.ValueID{cap: blk.instrs.len + 1}
	for ii in 0 .. insert_idx {
		new_instrs << blk.instrs[ii]
	}
	new_instrs << ssa.ValueID(new_val)
	for ii in insert_idx .. blk.instrs.len {
		new_instrs << blk.instrs[ii]
	}
	blk.instrs = new_instrs
	m.blocks[blk_id] = blk
}
