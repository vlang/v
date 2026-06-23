module optimize

import v3.ssa

// --- Mem2Reg (promote allocas to SSA values + phi nodes) ---
// Flat arrays indexed by value_id / block_id. Phi predecessor operands store
// raw block ids, matching the rest of the v3 SSA (and the arm64 backend's
// emit_phi_edge_copies, which reads operand[odd] as a block id).
struct Mem2RegCtx {
mut:
	defs           [][]int // alloc_id -> blocks storing to it
	uses           [][]int // alloc_id -> blocks loading from it
	phi_placements [][]int // block_id -> alloc_ids needing a phi here
	phi_vals       [][]int // block_id -> parallel phi value ids
	stacks         [][]int // alloc_id -> reaching-definition stack
	is_promotable  []bool
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
	n_values := m.values.len
	n_blocks := m.blocks.len

	mut ctx := Mem2RegCtx{
		defs:           [][]int{len: n_values}
		uses:           [][]int{len: n_values}
		phi_placements: [][]int{len: n_blocks}
		phi_vals:       [][]int{len: n_blocks}
		stacks:         [][]int{len: n_values}
		is_promotable:  []bool{len: n_values}
		phi_blocks:     []int{}
	}

	mut df := [][]int{len: n_blocks}
	mut df_blocks := []int{}
	mut visited := []bool{len: n_blocks}
	mut has_phi := []bool{len: n_blocks}

	for fi in 0 .. m.funcs.len {
		mut promotable := []int{}
		func := m.funcs[fi]
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
						ctx.is_promotable[val_id] = true
					}
				}
				if instr.op == .store {
					ptr := instr.operands[1]
					if ptr < n_values && blk_id !in ctx.defs[ptr] {
						array2d_append(mut ctx.defs, ptr, blk_id)
					}
				} else if instr.op == .load {
					ptr := instr.operands[0]
					if ptr < n_values && blk_id !in ctx.uses[ptr] {
						array2d_append(mut ctx.uses, ptr, blk_id)
					}
				}
			}
		}

		compute_dominance_frontier_flat(m, fi, &dom, cfg, mut df, mut df_blocks)

		// Insert phis at iterated dominance frontiers of each promotable alloca.
		for alloc_id in promotable {
			mut worklist := ctx.defs[alloc_id].clone()
			mut visited_list := []int{}
			mut has_phi_list := []int{}
			for worklist.len > 0 {
				b := worklist.pop()
				if b < 0 || b >= n_blocks {
					continue
				}
				for d in df[b] {
					if !has_phi[d] {
						array2d_append(mut ctx.phi_placements, d, alloc_id)
						if d !in ctx.phi_blocks {
							ctx.phi_blocks << d
						}
						has_phi[d] = true
						has_phi_list << d
						if !visited[d] {
							visited[d] = true
							visited_list << d
							worklist << d
						}
					}
				}
			}
			for d in visited_list {
				visited[d] = false
			}
			for d in has_phi_list {
				has_phi[d] = false
			}
		}

		// Materialize the phi instructions (empty operands, filled during rename).
		for blk_id in ctx.phi_blocks {
			for alloc_id in ctx.phi_placements[blk_id] {
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

		// Reset per-function state.
		for alloc_id in promotable {
			ctx.defs[alloc_id] = []
			ctx.uses[alloc_id] = []
			ctx.stacks[alloc_id] = []
			ctx.is_promotable[alloc_id] = false
		}
		for pb in ctx.phi_blocks {
			ctx.phi_placements[pb] = []
			ctx.phi_vals[pb] = []
		}
		ctx.phi_blocks = []
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
					if blk_id !in df[runner] {
						array2d_append(mut df, runner, blk_id)
						if runner !in df_blocks {
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
					alloc_id := ctx.phi_placements[blk_id][pai]
					if pai < ctx.phi_vals[blk_id].len {
						phi_val_id := ctx.phi_vals[blk_id][pai]
						mut new_stack := ctx.stacks[alloc_id].clone()
						new_stack << phi_val_id
						ctx.stacks[alloc_id] = new_stack
						mut wf := work[fi]
						wf.pushed_allocs << alloc_id
						work[fi] = wf
					}
				}
			}

			// 2. Process instructions: rewrite loads, record store values, nop them.
			mut instrs_to_nop := []int{}
			blk2 := m.blocks[blk_id]
			for val_id in blk2.instrs {
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				instr := m.instrs[val.index]
				if instr.op == .store {
					ptr := instr.operands[1]
					if ptr < ctx.is_promotable.len && ctx.is_promotable[ptr] {
						mut new_stack := ctx.stacks[ptr].clone()
						new_stack << instr.operands[0]
						ctx.stacks[ptr] = new_stack
						mut wf := work[fi]
						wf.pushed_allocs << ptr
						work[fi] = wf
						instrs_to_nop << val_id
					}
				} else if instr.op == .load {
					ptr := instr.operands[0]
					if ptr < ctx.is_promotable.len && ctx.is_promotable[ptr] {
						stack := ctx.stacks[ptr]
						mut repl := 0
						if stack.len > 0 {
							repl = stack.last()
						} else {
							repl = m.get_or_add_const(val.typ, 'undef')
						}
						m.replace_uses(val_id, repl)
						instrs_to_nop << val_id
					}
				} else if instr.op == .alloca {
					if val_id < ctx.is_promotable.len && ctx.is_promotable[val_id] {
						instrs_to_nop << val_id
					}
				}
			}
			for vid in instrs_to_nop {
				vid_val := m.values[vid]
				mut nop_instr := m.instrs[vid_val.index]
				nop_instr.op = .bitcast
				nop_instr.operands = []
				m.instrs[vid_val.index] = nop_instr
			}

			// 3. Fill phi operands in CFG successors (raw block id as predecessor).
			for succ_id in cfg.succs[blk_id] {
				if succ_id < ctx.phi_placements.len && ctx.phi_placements[succ_id].len > 0 {
					n_succ_phi := ctx.phi_placements[succ_id].len
					for spai in 0 .. n_succ_phi {
						alloc_id := ctx.phi_placements[succ_id][spai]
						if spai < ctx.phi_vals[succ_id].len {
							vid := ctx.phi_vals[succ_id][spai]
							phi_v := m.values[vid]
							phi_val := if ctx.stacks[alloc_id].len > 0 {
								ctx.stacks[alloc_id].last()
							} else {
								alloc_v := m.values[alloc_id]
								alloc_typ := m.type_store.types[alloc_v.typ]
								m.get_or_add_const(alloc_typ.elem_type, 'undef')
							}
							m.append_phi_operands(phi_v.index, phi_val, blk_id)
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
			pushed := work[fi].pushed_allocs.clone()
			work.pop()
			for i := pushed.len - 1; i >= 0; i-- {
				mut s := ctx.stacks[pushed[i]].clone()
				s.pop()
				ctx.stacks[pushed[i]] = s
			}
		}
	}
}
