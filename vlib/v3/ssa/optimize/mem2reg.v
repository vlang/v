module optimize

import v3.ssa

// --- Mem2Reg (promote allocas to SSA values + phi nodes) ---
// Phi predecessor operands store raw block ids, matching the rest of the v3
// SSA (and the arm64 backend's emit_phi_edge_copies). Per-alloca analysis data
// uses compact slots so one small promotable set does not allocate by the
// module's complete value namespace.
struct Mem2RegCtx {
mut:
	alloc_ids      []int       // compact slot -> alloca value id
	alloc_slots    map[int]int // alloca value id -> compact slot
	defs           [][]int     // compact slot -> blocks storing to it
	phi_placements [][]int     // block_id -> compact slots needing a phi here
	phi_vals       [][]int     // block_id -> parallel phi value ids
	stacks         [][]int     // compact slot -> reaching-definition stack
	phi_blocks     []int
}

// array2d_append supports array2d append handling for optimize.
fn array2d_append(mut arr [][]int, idx int, val int) {
	mut inner := arr[idx]
	inner << val
	arr[idx] = inner
}

// promote_memory_to_register promotes scalar allocas to SSA registers, inserting
// phi nodes at dominance frontiers and renaming load/store chains.
fn promote_memory_to_register(mut m ssa.Module, dom DomInfo, cfg &CfgData) {
	n_blocks := m.blocks.len
	mut df := [][]int{len: n_blocks}
	mut df_blocks := []int{}
	mut visited_stamp := []int{len: n_blocks}
	mut has_phi_stamp := []int{len: n_blocks}
	mut stamp := 0

	for fi in 0 .. m.funcs.len {
		mut promotable := []int{}
		func := m.funcs[fi]
		// First assign a compact slot to every promotable alloca.
		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			for val_id in blk.instrs {
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				instr := m.instrs[val.index]
				if instr.op == .alloca {
					if is_promotable(m, val_id) {
						promotable << val_id
					}
				}
			}
		}
		if promotable.len == 0 {
			continue
		}
		mut ctx := Mem2RegCtx{
			alloc_ids:      promotable
			alloc_slots:    map[int]int{}
			defs:           [][]int{len: promotable.len}
			phi_placements: [][]int{len: n_blocks}
			phi_vals:       [][]int{len: n_blocks}
			stacks:         [][]int{len: promotable.len}
			phi_blocks:     []int{}
		}
		for slot, alloc_id in promotable {
			ctx.alloc_slots[alloc_id] = slot
		}
		// Collect definition blocks without linear duplicate scans. Instructions
		// from one block are contiguous, so one last-block stamp per slot suffices.
		mut last_def_block := []int{len: promotable.len, init: -1}
		for blk_id in func.blocks {
			for val_id in m.blocks[blk_id].instrs {
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				instr := m.instrs[val.index]
				if instr.op != .store || instr.operands.len < 2 {
					continue
				}
				if slot := ctx.alloc_slots[instr.operands[1]] {
					if last_def_block[slot] != blk_id {
						array2d_append(mut ctx.defs, slot, blk_id)
						last_def_block[slot] = blk_id
					}
				}
			}
		}

		compute_dominance_frontier_flat(m, fi, &dom, cfg, mut df, mut df_blocks)

		// Insert phis at iterated dominance frontiers of each promotable alloca.
		for slot in 0 .. promotable.len {
			stamp++
			mut worklist := ctx.defs[slot].clone()
			for worklist.len > 0 {
				b := worklist.pop()
				if b < 0 || b >= n_blocks {
					continue
				}
				for d in df[b] {
					if has_phi_stamp[d] != stamp {
						first_phi_in_block := ctx.phi_placements[d].len == 0
						array2d_append(mut ctx.phi_placements, d, slot)
						if first_phi_in_block {
							ctx.phi_blocks << d
						}
						has_phi_stamp[d] = stamp
						if visited_stamp[d] != stamp {
							visited_stamp[d] = stamp
							worklist << d
						}
					}
				}
			}
		}

		// Materialize the phi instructions (empty operands, filled during rename).
		for blk_id in ctx.phi_blocks {
			for slot in ctx.phi_placements[blk_id] {
				alloc_id := ctx.alloc_ids[slot]
				alloc_val := m.values[alloc_id]
				alloc_typ := m.type_store.types[alloc_val.typ]
				typ := alloc_typ.elem_type
				phi_val := m.add_instr_front(.phi, blk_id, typ, [])
				mut pv := m.values[phi_val]
				pv.name = '${alloc_val.name}.phi_${blk_id}'
				m.values[phi_val] = pv
				array2d_append(mut ctx.phi_vals, blk_id, phi_val)
			}
		}

		// Rename: walk the dominator tree, threading reaching definitions.
		if func.blocks.len > 0 {
			rename_blocks(mut m, func.blocks[0], mut ctx, dom, cfg)
		}
		for d in df_blocks {
			df[d] = []
		}
		df_blocks = []
	}
}

// is_promotable returns true if every use of the alloca is a direct load/store,
// i.e. the pointer never escapes. Array- and pointer-backed slots stay in memory.
fn is_promotable(m &ssa.Module, alloc_id int) bool {
	if alloc_id > 0 && alloc_id < m.values.len {
		alloc_typ_id := m.values[alloc_id].typ
		if alloc_typ_id > 0 && alloc_typ_id < m.type_store.types.len {
			alloc_typ := m.type_store.types[alloc_typ_id]
			if alloc_typ.kind == .ptr_t {
				elem_typ_id := alloc_typ.elem_type
				if elem_typ_id > 0 && elem_typ_id < m.type_store.types.len {
					elem_typ := m.type_store.types[elem_typ_id]
					// Keep pointer-, array- and aggregate-backed slots in memory.
					// Promoting them to SSA values would require the backend to
					// thread by-value aggregates through phi copies, which the v3
					// arm64 lowering does not yet handle reliably.
					if elem_typ.kind in [.ptr_t, .array_t, .struct_t] {
						return false
					}
				}
			}
		}
	}

	for u in m.values[alloc_id].uses {
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
				if instr.operands.len < 2 || instr.operands[1] != alloc_id {
					return false
				}
			}
			else {
				return false
			}
		}
	}
	return true
}

// compute_dominance_frontier_flat supports compute dominance frontier flat handling for optimize.
fn compute_dominance_frontier_flat(m &ssa.Module, func_idx int, dom &DomInfo, cfg &CfgData, mut df [][]int, mut df_blocks []int) {
	func := m.funcs[func_idx]
	mut last_frontier_target := []int{len: m.blocks.len, init: -1}
	for blk_id in func.blocks {
		if blk_id < 0 || blk_id >= m.blocks.len {
			continue
		}
		num_preds := cfg.preds[blk_id].len
		if num_preds >= 2 {
			for pi in 0 .. num_preds {
				mut runner := cfg.preds[blk_id][pi]
				idom := dom.idom[blk_id]
				for runner != -1 && runner != idom {
					if runner < 0 || runner >= m.blocks.len {
						break
					}
					if last_frontier_target[runner] != blk_id {
						first_frontier_for_block := df[runner].len == 0
						array2d_append(mut df, runner, blk_id)
						last_frontier_target[runner] = blk_id
						if first_frontier_for_block {
							df_blocks << runner
						}
					}
					if runner == dom.idom[runner] {
						break
					}
					runner = dom.idom[runner]
				}
			}
		}
	}
}

// RenameFrame represents rename frame data used by optimize.
struct RenameFrame {
mut:
	blk_id        int
	child_idx     int
	pushed_allocs []int
	processed     bool
}

// rename_blocks supports rename blocks handling for optimize.
fn rename_blocks(mut m ssa.Module, root_blk int, mut ctx Mem2RegCtx, dom DomInfo, cfg &CfgData) {
	mut work := []RenameFrame{}
	work << RenameFrame{
		blk_id: root_blk
	}
	mut visited := []bool{len: m.blocks.len}

	for work.len > 0 {
		fi := work.len - 1
		blk_id := work[fi].blk_id

		if !work[fi].processed {
			if blk_id >= 0 && blk_id < visited.len && visited[blk_id] {
				work.pop()
				continue
			}
			if blk_id >= 0 && blk_id < visited.len {
				visited[blk_id] = true
			}
			mut frame2 := work[fi]
			frame2.processed = true
			work[fi] = frame2

			// 1. Push phis defined in this block.
			if blk_id < ctx.phi_placements.len && ctx.phi_placements[blk_id].len > 0 {
				n_phi := ctx.phi_placements[blk_id].len
				for pai in 0 .. n_phi {
					slot := ctx.phi_placements[blk_id][pai]
					if pai < ctx.phi_vals[blk_id].len {
						phi_val_id := ctx.phi_vals[blk_id][pai]
						array2d_append(mut ctx.stacks, slot, phi_val_id)
						mut wf := work[fi]
						wf.pushed_allocs << slot
						work[fi] = wf
					}
				}
			}

			// 2. Process instructions and compact the block once. Removed memory
			// operations become unreachable value tombstones instead of pretending
			// to be empty bitcasts.
			blk2 := m.blocks[blk_id]
			mut kept_instrs := []ssa.ValueID{cap: blk2.instrs.len}
			for val_id in blk2.instrs {
				val := m.values[val_id]
				if val.kind != .instruction {
					kept_instrs << val_id
					continue
				}
				instr := m.instrs[val.index]
				mut remove := false
				if instr.op == .store {
					if instr.operands.len >= 2 {
						if slot := ctx.alloc_slots[instr.operands[1]] {
							array2d_append(mut ctx.stacks, slot, instr.operands[0])
							mut wf := work[fi]
							wf.pushed_allocs << slot
							work[fi] = wf
							remove = true
						}
					}
				} else if instr.op == .load {
					if instr.operands.len > 0 {
						if slot := ctx.alloc_slots[instr.operands[0]] {
							stack := ctx.stacks[slot]
							mut repl := 0
							if stack.len > 0 {
								repl = stack.last()
							} else {
								repl = m.get_or_add_const(val.typ, 'undef')
							}
							m.replace_uses(val_id, repl)
							remove = true
						}
					}
				} else if instr.op == .alloca {
					if _ := ctx.alloc_slots[val_id] {
						remove = true
					}
				}
				if remove {
					m.detach_instruction_uses(val_id)
					mut dead := m.values[val_id]
					dead.kind = .unknown
					dead.uses = []ssa.ValueID{}
					m.values[val_id] = dead
				} else {
					kept_instrs << val_id
				}
			}
			if kept_instrs.len != blk2.instrs.len {
				mut compacted := m.blocks[blk_id]
				compacted.instrs = kept_instrs
				m.blocks[blk_id] = compacted
			}

			// 3. Fill phi operands in CFG successors (raw block id as predecessor).
			for succ_id in cfg.succs[blk_id] {
				if succ_id < ctx.phi_placements.len && ctx.phi_placements[succ_id].len > 0 {
					n_succ_phi := ctx.phi_placements[succ_id].len
					for spai in 0 .. n_succ_phi {
						slot := ctx.phi_placements[succ_id][spai]
						if spai < ctx.phi_vals[succ_id].len {
							vid := ctx.phi_vals[succ_id][spai]
							phi_val := if ctx.stacks[slot].len > 0 {
								ctx.stacks[slot].last()
							} else {
								alloc_id := ctx.alloc_ids[slot]
								alloc_v := m.values[alloc_id]
								alloc_typ := m.type_store.types[alloc_v.typ]
								m.get_or_add_const(alloc_typ.elem_type, 'undef')
							}
							m.append_phi_operands(vid, phi_val, blk_id)
						}
					}
				}
			}
		}

		// 4. Recurse into dominator-tree children.
		n_children := dom.dom_tree[blk_id].len
		child_idx := work[fi].child_idx
		if child_idx < n_children {
			child := dom.dom_tree[blk_id][child_idx]
			mut frame := work[fi]
			frame.child_idx++
			work[fi] = frame
			work << RenameFrame{
				blk_id: child
			}
		} else {
			// 5. Pop reaching definitions pushed in this block.
			for i := work[fi].pushed_allocs.len - 1; i >= 0; i-- {
				slot := work[fi].pushed_allocs[i]
				mut s := ctx.stacks[slot]
				s.pop()
				ctx.stacks[slot] = s
			}
			work.pop()
		}
	}
}
