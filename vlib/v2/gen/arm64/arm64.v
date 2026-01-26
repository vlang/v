// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

import v2.ssa
import encoding.binary

pub struct Gen {
	mod &ssa.Module
mut:
	macho &MachOObject

	stack_map      map[int]int
	alloca_offsets map[int]int
	stack_size     int
	curr_offset    int

	block_offsets  map[int]int
	pending_labels map[int][]int

	// Register allocation
	reg_map   map[int]int
	used_regs []int
	next_blk  int
}

pub fn Gen.new(mod &ssa.Module) &Gen {
	return &Gen{
		mod:   mod
		macho: MachOObject.new()
	}
}

pub fn (mut g Gen) gen() {
	for func in g.mod.funcs {
		g.gen_func(func)
	}

	// Globals in __data (Section 3)
	for gvar in g.mod.globals {
		for g.macho.data_data.len % 8 != 0 {
			g.macho.data_data << 0
		}
		addr := u64(g.macho.data_data.len)
		g.macho.add_symbol('_' + gvar.name, addr, true, 3)
		for _ in 0 .. 8 {
			g.macho.data_data << 0
		}
	}

	// Patch symbol addresses
	cstring_base := u64(g.macho.text_data.len)
	// Align data section to 8 bytes
	data_base := (cstring_base + u64(g.macho.str_data.len) + 7) & ~7

	for mut sym in g.macho.symbols {
		if sym.sect == 2 {
			sym.value += cstring_base
		} else if sym.sect == 3 {
			sym.value += data_base
		}
	}
}

fn (mut g Gen) gen_func(func ssa.Function) {
	g.curr_offset = g.macho.text_data.len
	g.stack_map = map[int]int{}
	g.alloca_offsets = map[int]int{}
	g.block_offsets = map[int]int{}
	g.pending_labels = map[int][]int{}
	g.reg_map = map[int]int{}
	g.used_regs = []int{}
	g.allocate_registers(func)

	// Stack Frame
	mut slot_offset := 8

	for pid in func.params {
		g.stack_map[pid] = -slot_offset
		slot_offset += 8
	}

	for i, blk_id in func.blocks {
		g.next_blk = if i + 1 < func.blocks.len { func.blocks[i + 1] } else { -1 }
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]

			if instr.op == .alloca {
				// Reserve 64 bytes for data.
				// Align to 16 bytes.
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += 64
				g.alloca_offsets[val_id] = -slot_offset

				// CRITICAL FIX: Ensure the next instruction does not use the slot
				// overlapping with the base of the alloca data.
				// alloca_offsets points to the bottom of the block.
				// The next instruction would get -slot_offset (which is the bottom).
				// We advance slot_offset to skip the block completely.
				// Note: slot_offset is already at the bottom.
				// But we need to ensure the *next* usage doesn't pick this address.
				// Since stack_map assignment comes *before* this increment, the next instr
				// will use the current slot_offset.
				// If slot_offset is 96, next gets -96. Data is -96..-32.
				// So we need to bump it so next gets -104.
				slot_offset += 8
			}

			if val_id in g.reg_map {
				continue
			}
			// Assign slot for result of instruction (or pointer for alloca)
			g.stack_map[val_id] = -slot_offset
			slot_offset += 8
		}
	}

	g.stack_size = (slot_offset + 16) & ~0xF

	g.macho.add_symbol('_' + func.name, u64(g.curr_offset), true, 1)

	// Prologue
	g.emit(0xA9BF7BFD) // stp fp, lr, [sp, -16]!
	g.emit(0x910003FD) // mov fp, sp

	// Save callee-saved regs
	for i := 0; i < g.used_regs.len; i += 2 {
		r1 := g.used_regs[i]
		mut r2 := 31 // xzr // r1 // dummy
		if i + 1 < g.used_regs.len {
			r2 = g.used_regs[i + 1]
		}
		// code := 0xA9BF0000 | (u32(r2) << 10) | (31 << 5) | u32(r1)
		code := 0xA9BF0000 | (u32(r2) << 10) | (31 << 5) | u32(r1)
		g.emit(code)
	}

	g.emit_sub_sp(g.stack_size)

	// Spill params
	for i, pid in func.params {
		if i < 8 {
			if reg := g.reg_map[pid] {
				g.emit_mov_reg(reg, i)
			} else {
				offset := g.stack_map[pid]
				g.emit_str_reg_offset(i, 29, offset)
			}
		}
	}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		g.block_offsets[blk_id] = g.macho.text_data.len - g.curr_offset

		if offsets := g.pending_labels[blk_id] {
			for off in offsets {
				target := g.block_offsets[blk_id]
				rel := (target - off) / 4
				abs_off := g.curr_offset + off
				instr := g.read_u32(abs_off)

				mut new_instr := u32(0)
				// Check for CBNZ (0xB5...) vs B (0x14...) vs B.cond (0x54...)
				if (instr & 0xFF000000) == 0xB5000000 {
					// CBNZ
					new_instr = (instr & 0xFF000000) | ((u32(rel) & 0x7FFFF) << 5) | (instr & 0x1F)
				} else if (instr & 0xFC000000) == 0x14000000 {
					// B imm26
					new_instr = (instr & 0xFC000000) | (u32(rel) & 0x3FFFFFF)
				} else {
					// B.cond
					new_instr = (instr & 0xFF000000) | ((u32(rel) & 0x7FFFF) << 5) | (instr & 0x1F)
				}
				g.write_u32(abs_off, new_instr)
			}
		}

		for val_id in blk.instrs {
			g.gen_instr(val_id)
		}
	}
}

fn (mut g Gen) gen_instr(val_id int) {
	instr := g.mod.instrs[g.mod.values[val_id].index]

	match instr.op {
		.add, .sub, .mul, .sdiv, .srem, .and_, .or_, .xor, .shl, .ashr, .lshr, .eq, .ne, .lt, .gt,
		.le, .ge {
			// Optimization: Use actual registers if allocated, avoid shuffling to x8/x9
			// Dest register
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Op0 (LHS)
			lhs_reg := g.get_operand_reg(instr.operands[0], 8)

			// Op1 (RHS) - Check immediate optimization
			mut is_imm := false
			mut imm_val := i64(0)
			mut rhs_reg := 9 // Default scratch for RHS

			op1 := g.mod.values[instr.operands[1]]
			if op1.kind == .constant && instr.op in [.add, .sub] {
				v := op1.name.i64()
				if v >= 0 && v < 4096 {
					is_imm = true
					imm_val = v
				}
			}

			if !is_imm {
				// Don't use x8 as scratch if LHS is in x8
				scratch := if lhs_reg == 8 { 9 } else { 8 }
				rhs_reg = g.get_operand_reg(instr.operands[1], scratch)
			}

			match instr.op {
				.add {
					if is_imm {
						// ADD Rd, Rn, #imm
						g.emit(0x91000000 | (u32(imm_val) << 10) | (u32(lhs_reg) << 5) | u32(dest_reg))
					} else {
						// ADD Rd, Rn, Rm
						g.emit(0x8B000000 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
					}
				}
				.sub {
					if is_imm {
						// SUB Rd, Rn, #imm
						g.emit(0xD1000000 | (u32(imm_val) << 10) | (u32(lhs_reg) << 5) | u32(dest_reg))
					} else {
						// SUB Rd, Rn, Rm
						g.emit(0xCB000000 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
					}
				}
				.mul {
					// MADD Rd, Rn, Rm, xzr (Ra=31) -> 0x9B007C00
					g.emit(0x9B007C00 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.sdiv {
					// SDIV Rd, Rn, Rm -> 0x9AC00C00
					g.emit(0x9AC00C00 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.srem {
					// Signed modulo: a % b = a - (a / b) * b
					// Choose temp register for quotient that doesn't conflict with inputs
					mut temp_reg := 10
					if lhs_reg == 10 || rhs_reg == 10 {
						temp_reg = 11
						if lhs_reg == 11 || rhs_reg == 11 {
							temp_reg = 12
						}
					}
					// SDIV temp, lhs, rhs
					g.emit(0x9AC00C00 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(temp_reg))
					// MSUB dest, temp, rhs, lhs  (dest = lhs - temp * rhs)
					g.emit(0x9B008000 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 10) | (u32(temp_reg) << 5) | u32(dest_reg))
				}
				.and_ {
					// AND Rd, Rn, Rm -> 0x8A000000
					g.emit(0x8A000000 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.or_ {
					// ORR Rd, Rn, Rm -> 0xAA000000
					g.emit(0xAA000000 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.xor {
					// EOR Rd, Rn, Rm -> 0xCA000000
					g.emit(0xCA000000 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.shl {
					// LSLV Rd, Rn, Rm -> 0x9AC02000
					g.emit(0x9AC02000 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.ashr {
					// ASRV Rd, Rn, Rm -> 0x9AC02800
					g.emit(0x9AC02800 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.lshr {
					// LSRV Rd, Rn, Rm -> 0x9AC02400
					g.emit(0x9AC02400 | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5) | u32(dest_reg))
				}
				.eq, .ne, .lt, .gt, .le, .ge {
					// CMP Rn, Rm (SUBS xzr, Rn, Rm) -> 0xEB00001F
					g.emit(0xEB00001F | (u32(rhs_reg) << 16) | (u32(lhs_reg) << 5))

					// CSET Rd, cond
					code := match instr.op {
						.eq { 0x9A9F17E0 } // EQ
						.ne { 0x9A9F07E0 } // NE
						.lt { 0x9A9FA7E0 } // LT
						.gt { 0x9A9FD7E0 } // GT
						.le { 0x9A9FC7E0 } // LE
						.ge { 0x9A9FB7E0 } // GE
						else { 0 }
					}
					g.emit(u32(code) | u32(dest_reg))
				}
				else {}
			}
			// If dest_reg was not the allocated one (e.g. was 8), move it.
			// Only if spilled (not in reg_map) do we need to store.
			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.store {
			val_reg := g.get_operand_reg(instr.operands[0], 8)
			ptr_reg := g.get_operand_reg(instr.operands[1], 9)

			// STR Rt, [Rn] -> 0xF9000000 | Rn<<5 | Rt
			g.emit(0xF9000000 | (u32(ptr_reg) << 5) | u32(val_reg))
		}
		.load {
			ptr_reg := g.get_operand_reg(instr.operands[0], 9)
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// LDR Rd, [Rn] -> 0xF9400000
			g.emit(0xF9400000 | (u32(ptr_reg) << 5) | u32(dest_reg))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.alloca {
			data_off := g.alloca_offsets[val_id]
			g.emit_add_fp_imm(8, data_off)
			g.store_reg_to_val(8, val_id)
		}
		.get_element_ptr {
			// GEP: Base + Index * 8 (assuming 64-bit/pointer types for now)
			base_reg := g.get_operand_reg(instr.operands[0], 8)

			// Ensure index load doesn't clobber base if base is 8
			idx_scratch := if base_reg == 8 { 9 } else { 8 }
			idx_reg := g.get_operand_reg(instr.operands[1], idx_scratch)

			// ADD Rd, Rn, Rm, LSL #3  (Shifted Register Add)
			// Encoding 64-bit: 0x8B200000 | (Rm<<16) | (imm6<<10) | (Rn<<5) | Rd
			// shift=3 -> imm6=000011 -> 0xC00
			// Dest is 8 (scratch)
			g.emit(0x8B200C00 | (u32(idx_reg) << 16) | (u32(base_reg) << 5) | 8)

			g.store_reg_to_val(8, val_id)
		}
		.call {
			// Load arguments in reverse order to avoid clobbering
			// If arg2 is in x0, loading arg1 to x0 first would clobber it
			num_args := instr.operands.len - 1
			for i := num_args; i >= 1; i-- {
				if i - 1 < 8 {
					g.load_val_to_reg(i - 1, instr.operands[i])
				}
			}
			fn_val := g.mod.values[instr.operands[0]]
			sym_idx := g.macho.add_undefined('_' + fn_val.name)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
			g.emit(0x94000000)

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_reg_to_val(0, val_id)
			}
		}
		.ret {
			if instr.operands.len > 0 {
				g.load_val_to_reg(0, instr.operands[0])
			}
			// Reset SP to the bottom of the callee-saved registers area
			// SP = FP - callee_saved_size
			callee_size := ((g.used_regs.len + 1) / 2) * 16
			g.emit(0xD1000000 | (u32(callee_size) << 10) | (29 << 5) | 31)
			// Restore callee-saved regs
			mut j := g.used_regs.len
			if j % 2 != 0 {
				j += 1
			}
			for j > 0 {
				base := j - 2
				r1 := g.used_regs[base]
				// mut r2 := r1
				mut r2 := 31 // xzr // r1 // dummy
				if base + 1 < g.used_regs.len {
					r2 = g.used_regs[base + 1]
				}
				// ldp r1, r2, [sp], 16
				// code := 0xA8C10000 | (u32(r2) << 10) | (31 << 5) | u32(r1)
				code := 0xA8C10000 | (u32(r2) << 10) | (31 << 5) | u32(r1)
				g.emit(code)
				j -= 2
			}
			g.emit(0xA8C17BFD)
			g.emit(0xD65F03C0)
		}
		.jmp {
			target_blk := instr.operands[0]
			target_idx := g.mod.values[target_blk].index

			// Fallthrough optimization: Don't jump if target is next block
			if target_idx == g.next_blk {
				return
			}

			if off := g.block_offsets[target_idx] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				g.emit(0x14000000 | (u32(rel) & 0x3FFFFFF))
			} else {
				g.record_pending_label(target_idx)
				g.emit(0x14000000)
			}
		}
		.br {
			g.load_val_to_reg(8, instr.operands[0])
			// Replaced CMP x8, #0; B.NE with CBNZ x8

			true_blk := g.mod.values[instr.operands[1]].index
			false_blk := g.mod.values[instr.operands[2]].index

			// Fallthrough optimization for False block
			// If false block is next, we only need CBNZ to true block

			if off := g.block_offsets[true_blk] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				// CBNZ x8, offset (0xB5...)
				g.emit(0xB5000008 | ((u32(rel) & 0x7FFFF) << 5))
			} else {
				g.record_pending_label(true_blk)
				g.emit(0xB5000008)
			}

			if false_blk == g.next_blk {
				return
			}

			if off := g.block_offsets[false_blk] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				g.emit(0x14000000 | (u32(rel) & 0x3FFFFFF))
			} else {
				g.record_pending_label(false_blk)
				g.emit(0x14000000)
			}
		}
		.switch_ {
			g.load_val_to_reg(8, instr.operands[0]) // Cond -> x8

			// Iterate cases: pairs of (val, blk) starting at index 2
			for i := 2; i < instr.operands.len; i += 2 {
				// We need val in a register. x9.
				g.load_val_to_reg(9, instr.operands[i])
				g.emit(0xEB09011F) // cmp x8, x9

				// b.eq target
				target_blk_val := instr.operands[i + 1]
				target_blk_idx := g.mod.values[target_blk_val].index

				// Emit branch EQ (cond = 0)
				// B.cond: 01010100 [imm19] 0[cond4] -> 0x54...0
				if off := g.block_offsets[target_blk_idx] {
					rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
					g.emit(0x54000000 | ((u32(rel) & 0x7FFFF) << 5))
				} else {
					g.record_pending_label(target_blk_idx)
					g.emit(0x54000000)
				}
			}

			// Default (Unconditional Branch)
			def_blk_val := instr.operands[1]
			def_idx := g.mod.values[def_blk_val].index
			if off := g.block_offsets[def_idx] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				g.emit(0x14000000 | (u32(rel) & 0x3FFFFFF))
			} else {
				g.record_pending_label(def_idx)
				g.emit(0x14000000)
			}
		}
		.bitcast {
			if instr.operands.len > 0 {
				g.load_val_to_reg(8, instr.operands[0])
				g.store_reg_to_val(8, val_id)
			}
		}
		.phi {
			// Phi nodes are handled by elim_phi_nodes inserting assignments in predecessors.
			// We just need to ensure the slot exists (handled in gen_func loop).
		}
		.assign {
			// assign dest_id, src_id
			// Used for Phi elimination: store src into dest's slot
			dest_id := instr.operands[0]
			src_id := instr.operands[1]

			g.load_val_to_reg(8, src_id)
			g.store_reg_to_val(8, dest_id)
		}
		else {
			eprintln('arm64: unknown instruction ${instr}')
			exit(1)
		}
	}
}

fn (mut g Gen) get_operand_reg(val_id int, fallback int) int {
	// If value is in a register, return it
	if r := g.reg_map[val_id] {
		return r
	}
	// Otherwise load it into fallback
	g.load_val_to_reg(fallback, val_id)
	return fallback
}

fn (mut g Gen) load_val_to_reg(reg int, val_id int) {
	val := g.mod.values[val_id]
	if val.kind == .constant {
		if val.name.starts_with('"') {
			// str_content := val.name.trim('"')

			raw_content := val.name.trim('"')
			// Handle escape sequences
			mut str_content := []u8{}
			mut i := 0
			for i < raw_content.len {
				if raw_content[i] == `\\` && i + 1 < raw_content.len {
					next_char := raw_content[i + 1]
					match next_char {
						`n` { str_content << 10 }
						`t` { str_content << 9 }
						`r` { str_content << 13 }
						`\\` { str_content << 92 }
						`"` { str_content << 34 }
						`'` { str_content << 39 }
						else { str_content << next_char }
					}
					i += 2
				} else {
					str_content << raw_content[i]
					i++
				}
			}

			str_offset := g.macho.str_data.len
			g.macho.str_data << str_content //.bytes()
			g.macho.str_data << 0

			sym_idx := g.macho.add_symbol('L_str_${str_offset}', u64(str_offset), false,
				2)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
			g.emit(0x90000000 | u32(reg))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
			g.emit(0x91000000 | u32(reg) | (u32(reg) << 5))
		} else {
			int_val := val.name.i64()
			g.emit_mov_imm64(reg, int_val)
		}
	} else if val.kind == .global {
		sym_idx := g.macho.add_undefined('_' + val.name)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(0x90000000 | u32(reg))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(0x91000000 | u32(reg) | (u32(reg) << 5))
	} else {
		// Handles .instruction, .argument, etc.
		if reg_idx := g.reg_map[val_id] {
			if reg_idx != reg {
				g.emit_mov_reg(reg, reg_idx)
			}
		} else {
			offset := g.stack_map[val_id]
			g.emit_ldr_reg_offset(reg, 29, offset)
		}
	}
}

fn (mut g Gen) store_reg_to_val(reg int, val_id int) {
	if reg_idx := g.reg_map[val_id] {
		if reg_idx != reg {
			g.emit_mov_reg(reg_idx, reg)
		}
	} else {
		offset := g.stack_map[val_id]
		g.emit_str_reg_offset(reg, 29, offset)
	}
}

fn (mut g Gen) emit_sub_sp(imm int) {
	if imm <= 0xFFF {
		// SUB SP, SP, #imm (unshifted)
		g.emit(0xD1000000 | (u32(imm) << 10) | (31 << 5) | 31)
	} else if imm <= 0xFFFFFF {
		// Split into high (shifted by 12) and low parts
		high := (imm >> 12) & 0xFFF
		low := imm & 0xFFF
		if high > 0 {
			// SUB SP, SP, #high, LSL #12
			g.emit(0xD1400000 | (u32(high) << 10) | (31 << 5) | 31)
		}
		if low > 0 {
			// SUB SP, SP, #low
			g.emit(0xD1000000 | (u32(low) << 10) | (31 << 5) | 31)
		}
	} else {
		// Very large stack: use register
		g.emit_mov_imm(10, u64(imm))
		// SUB SP, SP, X10
		g.emit(0xCB0A03FF)
	}
}

fn (mut g Gen) emit_add_fp_imm(rd int, imm int) {
	val := -imm
	if val <= 0xFFF {
		// SUB Rd, FP, #val
		g.emit(0xD1000000 | (u32(val) << 10) | (29 << 5) | u32(rd))
	} else if val <= 0xFFFFFF {
		// Split into high (shifted by 12) and low parts using x10 as temp
		high := (val >> 12) & 0xFFF
		low := val & 0xFFF
		// SUB Rd, FP, #high, LSL #12
		g.emit(0xD1400000 | (u32(high) << 10) | (29 << 5) | u32(rd))
		if low > 0 {
			// SUB Rd, Rd, #low
			g.emit(0xD1000000 | (u32(low) << 10) | (u32(rd) << 5) | u32(rd))
		}
	} else {
		// Very large offset: use register
		g.emit_mov_imm(10, u64(val))
		// SUB Rd, FP, X10
		g.emit(0xCB0003A0 | (10 << 16) | u32(rd))
	}
}

fn (mut g Gen) emit_str_reg_offset(rt int, rn int, offset int) {
	if offset >= -255 && offset <= 255 {
		imm9 := u32(offset & 0x1FF)
		g.emit(0xF8000000 | (imm9 << 12) | (u32(rn) << 5) | u32(rt))
	} else {
		// Large negative offset; use temp x10 for address
		imm := u64(-offset) // Positive imm
		g.emit_mov_imm(10, imm)
		g.emit(0xCB0A03AA) // sub x10, x29, x10
		g.emit(0xF9000140 | u32(rt)) // str xrt, [x10]
	}
}

fn (mut g Gen) emit_ldr_reg_offset(rt int, rn int, offset int) {
	if offset >= -255 && offset <= 255 {
		imm9 := u32(offset & 0x1FF)
		g.emit(0xF8400000 | (imm9 << 12) | (u32(rn) << 5) | u32(rt))
	} else {
		// Large negative offset; use temp x10 for address
		imm := u64(-offset) // Positive imm
		g.emit_mov_imm(10, imm)
		g.emit(0xCB0A03AA) // sub x10, x29, x10
		g.emit(0xF9400140 | u32(rt)) // ldr xrt, [x10]
	}
}

fn (mut g Gen) emit(code u32) {
	write_u32_le(mut g.macho.text_data, code)
}

fn (mut g Gen) record_pending_label(blk int) {
	off := g.macho.text_data.len - g.curr_offset
	g.pending_labels[blk] << off
}

fn (g Gen) read_u32(off int) u32 {
	return binary.little_endian_u32(g.macho.text_data[off..off + 4])
}

fn (mut g Gen) write_u32(off int, v u32) {
	binary.little_endian_put_u32(mut g.macho.text_data[off..off + 4], v)
}

pub fn (mut g Gen) write_file(path string) {
	g.macho.write(path)
}

fn (mut g Gen) emit_mov_imm(rd int, imm u64) {
	// Assume imm < 65536; use MOVZ xd, #imm
	g.emit(0xD2800000 | (u32(imm & 0xFFFF) << 5) | u32(rd))
	// For larger imm, add MOVK(s), but not needed for stack sizes.
}

fn (mut g Gen) emit_mov_imm64(rd int, val i64) {
	// Handle 0 specifically
	if val == 0 {
		g.emit_mov_reg(rd, 31) // mov reg, xzr
		return
	}

	// For small positive values (0 to 65535), use MOVZ
	if val > 0 && val <= 0xFFFF {
		// MOVZ xd, #imm16
		g.emit(0xD2800000 | (u32(val) << 5) | u32(rd))
		return
	}

	// For small negative values (-1 to -65536), use MOVN
	// MOVN xd, #imm16 sets xd to NOT(imm16 << shift)
	// For -1: MOVN xd, #0 -> ~0 = -1
	// For -42: MOVN xd, #41 -> ~41 = -42
	if val >= -65536 && val < 0 {
		not_val := u32(~val) // ~(-42) = 41
		// MOVN xd, #imm16 (64-bit): 0x92800000
		g.emit(0x92800000 | (not_val << 5) | u32(rd))
		return
	}

	// For larger values, use MOVZ followed by MOVK instructions
	uval := u64(val)

	// Emit MOVZ for first chunk (always at shift 0)
	chunk0 := u32(uval & 0xFFFF)
	g.emit(0xD2800000 | (chunk0 << 5) | u32(rd))

	// Emit MOVK for remaining non-zero chunks
	for shift := 16; shift < 64; shift += 16 {
		chunk := u32((uval >> shift) & 0xFFFF)
		if chunk != 0 {
			hw := u32(shift / 16)
			g.emit(0xF2800000 | (hw << 21) | (chunk << 5) | u32(rd))
		}
	}
}

fn (mut g Gen) emit_mov_reg(rd int, rm int) {
	// ORR xd, xzr, xm
	g.emit(0xAA0003E0 | (u32(rm) << 16) | u32(rd))
}

struct Interval {
mut:
	val_id   int
	start    int
	end      int
	has_call bool
}

fn (mut g Gen) allocate_registers(func ssa.Function) {
	mut intervals := map[int]&Interval{}
	mut call_indices := []int{}
	mut instr_idx := 0

	// Map block index to instruction range
	mut block_start := map[int]int{}
	mut block_end := map[int]int{}

	for pid in func.params {
		intervals[pid] = &Interval{
			val_id: pid
			start:  0
			end:    0
		}
	}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		block_start[blk_id] = instr_idx
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind == .instruction || val.kind == .argument {
				if unsafe { intervals[val_id] == nil } {
					intervals[val_id] = &Interval{
						val_id: val_id
						start:  instr_idx
						end:    instr_idx
					}
				}
			}

			instr := g.mod.instrs[val.index]
			if instr.op == .call {
				call_indices << instr_idx
			}

			for op in instr.operands {
				if g.mod.values[op].kind in [.instruction, .argument] {
					if mut interval := intervals[op] {
						if instr_idx > interval.end {
							interval.end = instr_idx
						}
					}
				}
			}
			instr_idx++
		}
		block_end[blk_id] = instr_idx
	}

	// Conservative approach: don't register-allocate values that cross block boundaries
	// This is slower but correct for loops
	mut block_of_def := map[int]int{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			block_of_def[val_id] = blk_id
		}
	}

	// Mark values used in different blocks than defined - extend to entire function
	total_instrs := instr_idx
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			for op in instr.operands {
				if g.mod.values[op].kind in [.instruction, .argument] {
					if def_blk := block_of_def[op] {
						if def_blk != blk_id {
							// Value crosses block boundary - extend to entire function
							if mut interval := intervals[op] {
								interval.end = total_instrs
							}
						}
					}
				}
			}
		}
	}

	// Mark intervals that cross a function call
	for _, mut interval in intervals {
		for call_idx in call_indices {
			if interval.start < call_idx && interval.end > call_idx {
				interval.has_call = true
				break
			}
		}
	}

	mut sorted := []&Interval{}
	for _, i in intervals {
		sorted << i
	}
	sorted.sort(a.start < b.start)

	mut active := []&Interval{}

	// Registers
	// Caller-saved (Temporaries): x9..x15
	// Callee-saved (Preserved): x19..x28
	// Reserve x8 and x9 as backend scratch registers
	// x10 is reserved as scratch for large offset operations
	short_regs := [11, 12, 13, 14, 15]
	long_regs := [19, 20, 21, 22, 23, 24, 25, 26, 27, 28]

	for i in sorted {
		for j := 0; j < active.len; j++ {
			if active[j].end < i.start {
				active.delete(j)
				j--
			}
		}

		// Decide which pool to use
		// mut pool := unsafe { short_regs }
		mut pool := short_regs.clone()
		if i.has_call {
			pool = long_regs.clone()
		}

		// Try allocation
		mut used := []bool{len: 32, init: false}
		for a in active {
			used[g.reg_map[a.val_id]] = true
		}

		for r in pool {
			if !used[r] {
				g.reg_map[i.val_id] = r
				active << i
				// Only track used callee-saved regs for prologue saving
				if r >= 19 && r <= 28 && r !in g.used_regs {
					g.used_regs << r
				}
				break
			}
		}
	}
	g.used_regs.sort()
}
