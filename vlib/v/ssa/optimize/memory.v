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

// MemoryRoots holds, indexed by value, the alloca each pointer derives from, and
// by alloca whether it escapes or is read. The arrays are reused across
// functions and cleared through `touched`.
struct MemoryRoots {
mut:
	root    []int
	escaped []bool
	read    []bool
	touched []int
}

// The per-block tables stay small on purpose: a huge initializer function has
// thousands of stores, and only recent ones are worth remembering.
const memory_tracked_addresses = 32

struct MemoryEntry {
	address int
	value   ssa.ValueID
}

fn memory_is_scalar(m &ssa.Module, typ ssa.TypeID) bool {
	if typ <= 0 || int(typ) >= m.type_store.types.len {
		return false
	}
	return m.type_store.types[typ].kind !in [.struct_t, .array_t] && m.type_size(typ) <= 8
}

// memory_roots finds, for each pointer, the alloca it is derived from and which
// allocas may be reachable from elsewhere.
fn memory_roots(m &ssa.Module, fi int, mut r MemoryRoots) {
	for val_id in r.touched {
		r.root[val_id] = 0
		r.escaped[val_id] = false
		r.read[val_id] = false
	}
	r.touched.clear()
	// The blocks of a function need not be stored in dominance order, so a
	// derived pointer can come before its base: propagate to a fixed point.
	// Each pass that changes something roots at least one more pointer, and
	// a pointer's root never changes once set (SSA defines it once).
	mut changed := true
	for changed {
		changed = false
		for blk_id in m.funcs[fi].blocks {
			for val_id in m.blocks[blk_id].instrs {
				if r.root[int(val_id)] != 0 {
					continue
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				instr := m.instrs[val.index]
				match instr.op {
					.alloca {
						r.root[int(val_id)] = int(val_id)
						r.touched << int(val_id)
						changed = true
					}
					.get_element_ptr, .bitcast {
						if instr.operands.len == 0 {
							continue
						}
						operand := int(instr.operands[0])
						if operand <= 0 || operand >= r.root.len {
							continue
						}
						base := r.root[operand]
						if base != 0 {
							r.root[int(val_id)] = base
							r.touched << int(val_id)
							changed = true
						}
					}
					else {}
				}
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
				if !instr.is_value_operand(oi) || int(operand) <= 0 || int(operand) >= r.root.len {
					continue
				}
				base := r.root[int(operand)]
				if base == 0 {
					continue
				}
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
}

// memory_may_alias is conservative: only two distinct slots, or a slot whose
// address never escapes against anything else, are known apart.
fn memory_may_alias(r &MemoryRoots, a int, b int) bool {
	if a == b {
		return true
	}
	root_a := if a > 0 && a < r.root.len { r.root[a] } else { 0 }
	root_b := if b > 0 && b < r.root.len { r.root[b] } else { 0 }
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
	mut roots := MemoryRoots{
		root:    []int{len: m.values.len}
		escaped: []bool{len: m.values.len}
		read:    []bool{len: m.values.len}
	}
	mut known := []MemoryEntry{cap: memory_tracked_addresses}
	mut pending := []MemoryEntry{cap: memory_tracked_addresses}
	mut dead := []ssa.ValueID{}
	mut dead_flag := []bool{len: m.values.len}
	for fi in 0 .. m.funcs.len {
		if m.funcs[fi].blocks.len == 0 {
			continue
		}
		memory_roots(m, fi, mut roots)
		for blk_id in m.funcs[fi].blocks {
			known.clear()
			pending.clear()
			optimize_memory_block(mut m, blk_id, &roots, mut known, mut pending, mut dead, mut dead_flag)
		}
		// Stores to slots nothing can ever read.
		for blk_id in m.funcs[fi].blocks {
			for val_id in m.blocks[blk_id].instrs {
				val := m.values[val_id]
				if val.kind != .instruction || m.instrs[val.index].op != .store {
					continue
				}
				address := int(m.instrs[val.index].operands[1])
				base := if address > 0 && address < roots.root.len {
					roots.root[address]
				} else {
					0
				}
				if base != 0 && !roots.escaped[base] && !roots.read[base] && !dead_flag[val_id] {
					dead_flag[val_id] = true
					dead << val_id
				}
			}
		}
		if dead.len == 0 {
			continue
		}
		for blk_id in m.funcs[fi].blocks {
			mut kept := []ssa.ValueID{cap: m.blocks[blk_id].instrs.len}
			for val_id in m.blocks[blk_id].instrs {
				if dead_flag[val_id] {
					m.detach_instruction_uses(val_id)
				} else {
					kept << val_id
				}
			}
			m.blocks[blk_id].instrs = kept
		}
		for val_id in dead {
			dead_flag[val_id] = false
		}
		dead.clear()
	}
}

fn memory_operation_may_write(op ssa.OpCode) bool {
	// A started thread or coroutine may read what was stored before it starts.
	return op in [.call, .call_indirect, .call_sret, .atomic_load, .atomic_store, .atomic_xchg,
		.atomic_add, .atomic_sub, .cmpxchg, .fence, .inline_asm, .heap_alloc, .go_call, .spawn_call]
}

// memory_forget removes the entries `address` may overwrite.
fn memory_forget(roots &MemoryRoots, mut entries []MemoryEntry, address int, keep_exact bool) {
	mut out := 0
	for entry in entries {
		if memory_may_alias(roots, entry.address, address) && !(keep_exact && entry.address == address) {
			continue
		}
		entries[out] = entry
		out++
	}
	entries.trim(out)
}

fn memory_lookup(entries []MemoryEntry, address int) ?ssa.ValueID {
	for entry in entries {
		if entry.address == address {
			return entry.value
		}
	}
	return none
}

fn memory_remember(mut entries []MemoryEntry, address int, value ssa.ValueID) {
	if entries.len >= memory_tracked_addresses {
		entries.clear()
	}
	entries << MemoryEntry{
		address: address
		value:   value
	}
}

fn memory_forget_escaping(roots &MemoryRoots, mut entries []MemoryEntry) {
	mut out := 0
	for entry in entries {
		base := if entry.address > 0 && entry.address < roots.root.len {
			roots.root[entry.address]
		} else {
			0
		}
		if base != 0 && !roots.escaped[base] {
			entries[out] = entry
			out++
		}
	}
	entries.trim(out)
}

fn optimize_memory_block(mut m ssa.Module, blk_id int, roots &MemoryRoots, mut known []MemoryEntry, mut pending []MemoryEntry, mut dead []ssa.ValueID, mut dead_flag []bool) {
	for val_id in m.blocks[blk_id].instrs {
		val := m.values[val_id]
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		match instr.op {
			.load {
				address := int(instr.operands[0])
				memory_forget(roots, mut pending, address, false)
				if !memory_is_scalar(m, val.typ) {
					continue
				}
				if existing := memory_lookup(known, address) {
					if m.values[existing].typ == val.typ {
						m.replace_uses(val_id, existing)
						continue
					}
				}
				memory_remember(mut known, address, val_id)
			}
			.store {
				stored := instr.operands[0]
				address := int(instr.operands[1])
				memory_forget(roots, mut known, address, false)
				if !memory_is_scalar(m, m.values[stored].typ) {
					memory_forget(roots, mut pending, address, false)
					continue
				}
				if previous := memory_lookup(pending, address) {
					// Only a store that covers every byte of the earlier one
					// makes it dead: `store i8` keeps the rest of a `store i64`.
					previous_value := m.instrs[m.values[previous].index].operands[0]
					if m.type_size(m.values[stored].typ) >= m.type_size(m.values[previous_value].typ)
						&& !dead_flag[int(previous)] {
						dead_flag[int(previous)] = true
						dead << previous
					}
				}
				memory_forget(roots, mut pending, address, false)
				memory_remember(mut pending, address, val_id)
				memory_remember(mut known, address, stored)
			}
			else {
				if memory_operation_may_write(instr.op) {
					memory_forget_escaping(roots, mut known)
					memory_forget_escaping(roots, mut pending)
				}
			}
		}
	}
}
