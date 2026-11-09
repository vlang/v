module optimize

import v.ssa

// Memory optimizations, after the `(Load (Store ...))` rules of Go's
// cmd/compile/internal/ssa/_gen/generic.rules:
//
// - a load after a store or load of the same address, with nothing in between
//   that may write it, reuses that value;
// - a store overwritten before anything can read it is dropped;
// - stores to a stack slot that is never read and whose address never escapes
//   are dropped.
//
// Zig leaves this to LLVM. Everything here is local to a block, so it needs no
// memory SSA.

struct MemoryRoots {
mut:
	root    map[int]int // pointer value -> alloca it derives from
	escaped map[int]bool
	read    map[int]bool
}

fn memory_is_scalar(m &ssa.Module, typ ssa.TypeID) bool {
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return false
	}
	return m.type_store.types[typ].kind !in [.struct_t, .array_t] && m.type_size(typ) <= 8
}

// memory_roots finds, for each pointer, the alloca it is derived from and which
// allocas may be reachable from elsewhere.
fn memory_roots(m &ssa.Module, fi int) MemoryRoots {
	mut r := MemoryRoots{}
	for blk_id in m.funcs[fi].blocks {
		for val_id in m.blocks[blk_id].instrs {
			val := m.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := m.instrs[val.index]
			match instr.op {
				.alloca {
					r.root[int(val_id)] = int(val_id)
				}
				.get_element_ptr, .bitcast {
					if base := r.root[int(instr.operands[0])] {
						r.root[int(val_id)] = base
					}
				}
				else {}
			}
		}
	}
	for blk_id in m.funcs[fi].blocks {
		for val_id in m.blocks[blk_id].instrs {
			val := m.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := m.instrs[val.index]
			for oi, operand in instr.operands {
				if !instr.is_value_operand(oi) {
					continue
				}
				base := r.root[int(operand)] or { continue }
				match instr.op {
					.load {
						r.read[base] = true
					}
					.get_element_ptr, .bitcast {
						if oi != 0 {
							r.escaped[base] = true
						}
					}
					.store {
						if oi == 0 {
							r.escaped[base] = true
						}
					}
					else {
						r.escaped[base] = true
					}
				}
			}
		}
	}
	return r
}

// memory_may_alias is conservative: only two distinct slots, or a slot whose
// address never escapes against anything else, are known apart.
fn memory_may_alias(r &MemoryRoots, a int, b int) bool {
	if a == b {
		return true
	}
	root_a := r.root[a] or { 0 }
	root_b := r.root[b] or { 0 }
	if root_a != 0 && root_b != 0 {
		return root_a == root_b
	}
	if root_a != 0 && !r.escaped[root_a] {
		return false
	}
	if root_b != 0 && !r.escaped[root_b] {
		return false
	}
	return true
}

fn optimize_memory(mut m ssa.Module) {
	for fi in 0 .. m.funcs.len {
		if m.funcs[fi].blocks.len == 0 {
			continue
		}
		roots := memory_roots(m, fi)
		mut dead := map[int]bool{}
		for blk_id in m.funcs[fi].blocks {
			optimize_memory_block(mut m, blk_id, &roots, mut dead)
		}
		// Stores to slots nothing can ever read.
		for blk_id in m.funcs[fi].blocks {
			for val_id in m.blocks[blk_id].instrs {
				val := m.values[val_id]
				if val.kind != .instruction || m.instrs[val.index].op != .store {
					continue
				}
				base := roots.root[int(m.instrs[val.index].operands[1])] or { continue }
				if !roots.escaped[base] && !roots.read[base] {
					dead[int(val_id)] = true
				}
			}
		}
		if dead.len == 0 {
			continue
		}
		for blk_id in m.funcs[fi].blocks {
			mut kept := []ssa.ValueID{cap: m.blocks[blk_id].instrs.len}
			for val_id in m.blocks[blk_id].instrs {
				if dead[int(val_id)] {
					m.detach_instruction_uses(val_id)
				} else {
					kept << val_id
				}
			}
			m.blocks[blk_id].instrs = kept
		}
	}
}

fn memory_operation_may_write(op ssa.OpCode) bool {
	return op in [.call, .call_indirect, .call_sret, .atomicrmw, .cmpxchg, .fence, .inline_asm, .heap_alloc]
}

fn optimize_memory_block(mut m ssa.Module, blk_id int, roots &MemoryRoots, mut dead map[int]bool) {
	mut known := map[int]ssa.ValueID{} // address -> value it holds
	mut pending := map[int]int{} // address -> store nothing has read yet
	for val_id in m.blocks[blk_id].instrs {
		val := m.values[val_id]
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		match instr.op {
			.load {
				address := int(instr.operands[0])
				for candidate in pending.keys() {
					if memory_may_alias(roots, candidate, address) {
						pending.delete(candidate)
					}
				}
				if !memory_is_scalar(m, val.typ) {
					continue
				}
				if existing := known[address] {
					if m.values[existing].typ == val.typ {
						m.replace_uses(val_id, existing)
						continue
					}
				}
				known[address] = val_id
			}
			.store {
				stored := instr.operands[0]
				address := int(instr.operands[1])
				for candidate in known.keys() {
					if memory_may_alias(roots, candidate, address) {
						known.delete(candidate)
					}
				}
				for candidate in pending.keys() {
					if candidate != address && memory_may_alias(roots, candidate, address) {
						pending.delete(candidate)
					}
				}
				if !memory_is_scalar(m, m.values[stored].typ) {
					continue
				}
				if previous := pending[address] {
					dead[previous] = true
				}
				pending[address] = int(val_id)
				known[address] = stored
			}
			else {
				if memory_operation_may_write(instr.op) {
					for candidate in known.keys() {
						base := roots.root[candidate] or { 0 }
						if base == 0 || roots.escaped[base] {
							known.delete(candidate)
						}
					}
					// A callee can read what escaped.
					for candidate in pending.keys() {
						base := roots.root[candidate] or { 0 }
						if base == 0 || roots.escaped[base] {
							pending.delete(candidate)
						}
					}
				}
			}
		}
	}
}
