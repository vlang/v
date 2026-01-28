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
pub mut:
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

	// Track which string literals have been materialized (value_id -> str_data offset)
	string_literal_offsets map[int]int

	// Cache for parsed constant integer values (value_id -> parsed i64)
	const_cache map[int]i64
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
		// Calculate actual size of the global variable based on its type
		size := g.type_size(gvar.typ)
		if gvar.is_constant && size == 8 {
			// For constants, write the initial value
			mut bytes := []u8{len: 8}
			binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
			for b in bytes {
				g.macho.data_data << b
			}
		} else {
			// For regular globals, initialize with zeros
			for _ in 0 .. size {
				g.macho.data_data << 0
			}
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
	g.string_literal_offsets = map[int]int{}
	g.const_cache = map[int]i64{}
	g.allocate_registers(func)

	// Stack Frame
	mut slot_offset := 8

	for pid in func.params {
		g.stack_map[pid] = -slot_offset
		slot_offset += 8
	}

	// Pre-pass: allocate stack slots for string_literal values
	for val in g.mod.values {
		if val.kind == .string_literal {
			// String struct needs 24 bytes (str ptr + len + is_lit)
			slot_offset = (slot_offset + 15) & ~0xF
			slot_offset += 24
			g.stack_map[val.id] = -slot_offset
		}
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
				// Calculate allocation size based on the type
				// The alloca result type is ptr(T), so get the element type
				ptr_type := g.mod.type_store.types[val.typ]
				elem_type := g.mod.type_store.types[ptr_type.elem_type]

				// Calculate size based on element type
				mut alloc_size := 64 // Default for non-array types
				if elem_type.kind == .array_t {
					// Get the array element type to determine element size
					arr_elem_type := g.mod.type_store.types[elem_type.elem_type]
					elem_size := if arr_elem_type.width > 0 {
						(arr_elem_type.width + 7) / 8 // bits to bytes, rounded up
					} else {
						8 // default to 64-bit
					}
					alloc_size = elem_type.len * elem_size
				}

				// Align to 16 bytes.
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += alloc_size
				g.alloca_offsets[val_id] = -slot_offset

				// Ensure the next instruction does not use the slot
				// overlapping with the base of the alloca data.
				slot_offset += 8
			}

			if instr.op == .inline_string_init {
				// String struct needs 24 bytes (str ptr + len + is_lit)
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += 24
				g.stack_map[val_id] = -slot_offset
				continue // Don't allocate another 8 bytes below
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
	g.emit(asm_stp_fp_lr_pre())
	g.emit(asm_mov_fp_sp())

	// Save callee-saved regs
	for i := 0; i < g.used_regs.len; i += 2 {
		r1 := g.used_regs[i]
		mut r2 := 31 // xzr
		if i + 1 < g.used_regs.len {
			r2 = g.used_regs[i + 1]
		}
		g.emit(asm_stp_pair_pre(Reg(r1), Reg(r2)))
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
		.fadd, .fsub, .fmul, .fdiv, .frem {
			// Float operations using scalar SIMD instructions (d0-d7)
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// For now, load operands as float constants or from memory
			// Load LHS to d0
			g.load_float_operand(instr.operands[0], 0) // d0
			// Load RHS to d1
			g.load_float_operand(instr.operands[1], 1) // d1

			// Perform float operation: result in d0
			match instr.op {
				.fadd {
					g.emit(asm_fadd_d0_d0_d1())
				}
				.fsub {
					g.emit(asm_fsub_d0_d0_d1())
				}
				.fmul {
					g.emit(asm_fmul_d0_d0_d1())
				}
				.fdiv {
					g.emit(asm_fdiv_d0_d0_d1())
				}
				.frem {
					// No single instruction for frem on ARM64
					// Use: d0 = d0 - trunc(d0/d1) * d1
					g.emit(asm_fdiv_d2_d0_d1())
					g.emit(asm_frintz_d2())
					g.emit(asm_fnmsub_d0_d2_d1_d0())
				}
				else {}
			}

			// Convert d0 result back to integer register for storage
			// Store the float bits in the result (for later int() conversion)
			g.emit(asm_fmov_x_d(Reg(dest_reg), 0))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.fptosi {
			// Float to signed integer conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load float operand to d0
			g.load_float_operand(instr.operands[0], 0)

			// FCVTZS Xd, Dn (convert to signed int, truncate toward zero)
			g.emit(asm_fcvtzs_x_d(Reg(dest_reg), 0))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.sitofp {
			// Signed integer to float conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load integer operand to x8
			src_reg := g.get_operand_reg(instr.operands[0], 8)

			// SCVTF Dd, Xn (convert signed int to double)
			g.emit(asm_scvtf_d_x(0, Reg(src_reg)))

			// FMOV Xd, D0 (copy back bit pattern to integer reg for storage)
			g.emit(asm_fmov_x_d(Reg(dest_reg), 0))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.fptoui, .uitofp {
			// For now, handle same as signed versions
			// TODO: Add proper unsigned conversion support
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }
			g.load_val_to_reg(dest_reg, instr.operands[0])
			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
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
				v := g.get_const_int(instr.operands[1])
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
						g.emit(asm_add_imm(Reg(dest_reg), Reg(lhs_reg), u32(imm_val)))
					} else {
						g.emit(asm_add_reg(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
					}
				}
				.sub {
					if is_imm {
						g.emit(asm_sub_imm(Reg(dest_reg), Reg(lhs_reg), u32(imm_val)))
					} else {
						g.emit(asm_sub_reg(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
					}
				}
				.mul {
					g.emit(asm_mul(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.sdiv {
					g.emit(asm_sdiv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
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
					g.emit(asm_sdiv(Reg(temp_reg), Reg(lhs_reg), Reg(rhs_reg)))
					g.emit(asm_msub(Reg(dest_reg), Reg(temp_reg), Reg(rhs_reg), Reg(lhs_reg)))
				}
				.and_ {
					g.emit(asm_and(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.or_ {
					g.emit(asm_orr(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.xor {
					g.emit(asm_eor(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.shl {
					g.emit(asm_lslv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.ashr {
					g.emit(asm_asrv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.lshr {
					g.emit(asm_lsrv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.eq, .ne, .lt, .gt, .le, .ge {
					g.emit(asm_cmp_reg(Reg(lhs_reg), Reg(rhs_reg)))

					// CSET Rd, cond
					match instr.op {
						.eq { g.emit(asm_cset_eq(Reg(dest_reg))) }
						.ne { g.emit(asm_cset_ne(Reg(dest_reg))) }
						.lt { g.emit(asm_cset_lt(Reg(dest_reg))) }
						.gt { g.emit(asm_cset_gt(Reg(dest_reg))) }
						.le { g.emit(asm_cset_le(Reg(dest_reg))) }
						.ge { g.emit(asm_cset_ge(Reg(dest_reg))) }
						else {}
					}
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

			g.emit(asm_str(Reg(val_reg), Reg(ptr_reg)))
		}
		.load {
			ptr_reg := g.get_operand_reg(instr.operands[0], 9)
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			g.emit(asm_ldr(Reg(dest_reg), Reg(ptr_reg)))

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

			// ADD Rd, Rn, Rm, LSL #3 (Dest is 8 - scratch)
			g.emit(asm_add_reg_lsl3(Reg(8), Reg(base_reg), Reg(idx_reg)))

			g.store_reg_to_val(8, val_id)
		}
		.call {
			fn_val := g.mod.values[instr.operands[0]]
			fn_name := fn_val.name

			// Skip calls with empty function names (shouldn't happen, but safety check)
			if fn_name != '' {
				// Check if this is a variadic function (like sprintf)
				// On ARM64 macOS, variadic args must be passed on the stack
				is_variadic := fn_name in ['sprintf', 'printf', 'snprintf', 'fprintf', 'sscanf']
				num_fixed_args := if fn_name == 'sprintf' {
					2 // buffer, format
				} else if fn_name == 'printf' {
					1 // format
				} else if fn_name in ['snprintf', 'fprintf'] {
					3 // buffer/file, size, format
				} else if fn_name == 'sscanf' {
					2 // string, format
				} else {
					8 // default: all in registers
				}

				num_args := instr.operands.len - 1

				if is_variadic && num_args > num_fixed_args {
					// Variadic call: push variadic args to stack, fixed args to registers
					num_variadic := num_args - num_fixed_args

					// Allocate stack space for variadic args (8 bytes each, 16-byte aligned)
					stack_space := ((num_variadic * 8) + 15) & ~0xF
					if stack_space > 0 {
						g.emit_sub_sp(stack_space)
					}

					// Store variadic arguments to stack (in order)
					for i := 0; i < num_variadic; i++ {
						arg_idx := num_fixed_args + 1 + i // +1 because operands[0] is the function
						g.load_val_to_reg(8, instr.operands[arg_idx])
						// STR x8, [sp, #offset]
						offset := i * 8
						imm12 := u32(offset / 8)
						g.emit(asm_str_imm(Reg(8), sp, imm12))
					}

					// Load fixed arguments to registers (in reverse order to avoid clobbering)
					for i := num_fixed_args; i >= 1; i-- {
						g.load_val_to_reg(i - 1, instr.operands[i])
					}

					// Call function
					sym_idx := g.macho.add_undefined('_' + fn_name)
					g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
						true)
					g.emit(asm_bl_reloc())

					// Restore stack
					if stack_space > 0 {
						if stack_space <= 0xFFF {
							g.emit(asm_add_imm(sp, sp, u32(stack_space)))
						} else {
							g.emit_mov_imm(10, u64(stack_space))
							g.emit(asm_add_sp_reg(Reg(10)))
						}
					}
				} else {
					// Non-variadic call: all args in registers
					// Load arguments in reverse order to avoid clobbering
					for i := num_args; i >= 1; i-- {
						if i - 1 < 8 {
							g.load_val_to_reg(i - 1, instr.operands[i])
						}
					}

					sym_idx := g.macho.add_undefined('_' + fn_name)
					g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
						true)
					g.emit(asm_bl_reloc())
				}

				if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
					g.store_reg_to_val(0, val_id)
				}
			}
		}
		.call_indirect {
			// Indirect call through function pointer
			// operands[0] is the function pointer, rest are arguments
			num_args := instr.operands.len - 1

			// Load arguments in reverse order to avoid clobbering
			for i := num_args; i >= 1; i-- {
				if i - 1 < 8 {
					g.load_val_to_reg(i - 1, instr.operands[i])
				}
			}

			// Load function pointer to x9 (scratch register)
			g.load_val_to_reg(9, instr.operands[0])

			// BLR x9 - branch and link to register
			g.emit(asm_blr(Reg(9)))

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
			g.emit(asm_sub_imm(sp, fp, u32(callee_size)))
			// Restore callee-saved regs
			mut j := g.used_regs.len
			if j % 2 != 0 {
				j += 1
			}
			for j > 0 {
				base := j - 2
				r1 := g.used_regs[base]
				mut r2 := 31 // xzr
				if base + 1 < g.used_regs.len {
					r2 = g.used_regs[base + 1]
				}
				g.emit(asm_ldp_pair_post(Reg(r1), Reg(r2)))
				j -= 2
			}
			g.emit(asm_ldp_fp_lr_post())
			g.emit(asm_ret())
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
				g.emit(asm_b(rel))
			} else {
				g.record_pending_label(target_idx)
				g.emit(asm_b(0))
			}
		}
		.br {
			g.load_val_to_reg(8, instr.operands[0])

			true_blk := g.mod.values[instr.operands[1]].index
			false_blk := g.mod.values[instr.operands[2]].index

			// Fallthrough optimization for False block
			// If false block is next, we only need CBNZ to true block

			if off := g.block_offsets[true_blk] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				g.emit(asm_cbnz(Reg(8), rel))
			} else {
				g.record_pending_label(true_blk)
				g.emit(asm_cbnz(Reg(8), 0))
			}

			if false_blk == g.next_blk {
				return
			}

			if off := g.block_offsets[false_blk] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				g.emit(asm_b(rel))
			} else {
				g.record_pending_label(false_blk)
				g.emit(asm_b(0))
			}
		}
		.switch_ {
			g.load_val_to_reg(8, instr.operands[0]) // Cond -> x8

			// Iterate cases: pairs of (val, blk) starting at index 2
			for i := 2; i < instr.operands.len; i += 2 {
				// We need val in a register. x9.
				g.load_val_to_reg(9, instr.operands[i])
				g.emit(asm_cmp_reg(Reg(8), Reg(9)))

				// b.eq target
				target_blk_val := instr.operands[i + 1]
				target_blk_idx := g.mod.values[target_blk_val].index

				if off := g.block_offsets[target_blk_idx] {
					rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
					g.emit(asm_b_cond(cond_eq, rel))
				} else {
					g.record_pending_label(target_blk_idx)
					g.emit(asm_b_cond(cond_eq, 0))
				}
			}

			// Default (Unconditional Branch)
			def_blk_val := instr.operands[1]
			def_idx := g.mod.values[def_blk_val].index
			if off := g.block_offsets[def_idx] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				g.emit(asm_b(rel))
			} else {
				g.record_pending_label(def_idx)
				g.emit(asm_b(0))
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
		.unreachable {
			// Emit UDF #0 instruction (undefined trap)
			g.emit(asm_udf())
		}
		.inline_string_init {
			// Create string struct by value: { str, len, is_lit }
			// operands: [str_ptr, len, is_lit]
			// This instruction creates a string struct on the stack
			// The result is a pointer to the struct
			str_ptr_id := instr.operands[0]
			len_id := instr.operands[1]
			is_lit_id := instr.operands[2]

			// Get base pointer for this value's stack slot
			base_offset := g.stack_map[val_id]

			// Store str field (offset 0)
			g.load_val_to_reg(8, str_ptr_id)
			g.emit_str_reg_offset(8, 29, base_offset)

			// Store len field (offset 8)
			g.load_val_to_reg(9, len_id)
			g.emit_str_reg_offset(9, 29, base_offset + 8)

			// Store is_lit field (offset 16)
			g.load_val_to_reg(10, is_lit_id)
			g.emit_str_reg_offset(10, 29, base_offset + 16)

			// Return pointer to struct (base address)
			g.emit_add_fp_imm(8, base_offset) // x8 = fp + offset
			g.store_reg_to_val(8, val_id)
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

// load_float_operand loads a value into a float register (d0-d7).
// For constants, parses the float value and loads via literal pool or immediate.
// For memory values, loads from stack into integer reg then moves to float reg.
fn (mut g Gen) load_float_operand(val_id int, dreg int) {
	val := g.mod.values[val_id]
	if val.kind == .constant {
		// Parse float constant and load into float register
		// First load the bit pattern into an integer register
		f_val := val.name.f64()
		bits := *unsafe { &u64(&f_val) }

		// Load the 64-bit bits into x8
		g.emit_mov_imm64(8, i64(bits))
		g.emit(asm_fmov_d_x(dreg, Reg(8)))
	} else {
		// Load from stack into integer register then move to float
		g.load_val_to_reg(8, val_id)
		g.emit(asm_fmov_d_x(dreg, Reg(8)))
	}
}

// Get constant integer value with caching to avoid repeated string parsing
fn (mut g Gen) get_const_int(val_id int) i64 {
	if cached := g.const_cache[val_id] {
		return cached
	}
	int_val := g.mod.values[val_id].name.i64()
	g.const_cache[val_id] = int_val
	return int_val
}

fn (mut g Gen) load_val_to_reg(reg int, val_id int) {
	val := g.mod.values[val_id]
	if val.kind == .constant {
		// Quick check: if first char is '"' it's a string
		if val.name.len > 0 && val.name[0] == `"` {
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
			g.macho.str_data << str_content
			g.macho.str_data << 0

			sym_idx := g.macho.add_symbol('L_str_${str_offset}', u64(str_offset), false,
				2)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
			g.emit(asm_add_pageoff(Reg(reg)))
		} else {
			int_val := g.get_const_int(val_id)
			g.emit_mov_imm64(reg, int_val)
		}
	} else if val.kind == .global {
		sym_idx := g.macho.add_undefined('_' + val.name)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(asm_adrp(Reg(reg)))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(asm_add_pageoff(Reg(reg)))
	} else if val.kind == .string_literal {
		// String literal: create string struct { str, len, is_lit } on stack
		// val.name contains the string content, val.index contains the length

		// Get stack slot for this string struct (24 bytes: str ptr + len + is_lit)
		base_offset := g.stack_map[val_id]

		// Check if we've already materialized this string literal
		if _ := g.string_literal_offsets[val_id] {
			// Already materialized - just load pointer to the struct
			g.emit_add_fp_imm(reg, base_offset)
		} else {
			// First time - create string data and initialize struct
			str_content := val.name
			str_len := val.index

			// Create the string data in cstring section
			str_offset2 := g.macho.str_data.len
			g.macho.str_data << str_content.bytes()
			g.macho.str_data << 0 // null terminator

			// Track that we've materialized this string literal
			g.string_literal_offsets[val_id] = str_offset2

			// Store str pointer (offset 0): load address of string data
			sym_idx := g.macho.add_symbol('L_str_${str_offset2}', u64(str_offset2), false,
				2)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
			g.emit(asm_add_pageoff(Reg(reg)))
			g.emit_str_reg_offset(reg, 29, base_offset)

			// Store len (offset 8)
			g.emit_mov_imm64(9, str_len)
			g.emit_str_reg_offset(9, 29, base_offset + 8)

			// Store is_lit = 1 (offset 16)
			g.emit_mov_imm64(10, 1)
			g.emit_str_reg_offset(10, 29, base_offset + 16)

			// Load pointer to string struct into reg
			g.emit_add_fp_imm(reg, base_offset)
		}
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
		g.emit(asm_sub_imm(sp, sp, u32(imm)))
	} else if imm <= 0xFFFFFF {
		// Split into high (shifted by 12) and low parts
		high := (imm >> 12) & 0xFFF
		low := imm & 0xFFF
		if high > 0 {
			g.emit(asm_sub_imm_lsl12(sp, sp, u32(high)))
		}
		if low > 0 {
			g.emit(asm_sub_imm(sp, sp, u32(low)))
		}
	} else {
		// Very large stack: use register
		g.emit_mov_imm(10, u64(imm))
		g.emit(asm_sub_sp_reg(Reg(10)))
	}
}

fn (mut g Gen) emit_add_fp_imm(rd int, imm int) {
	val := -imm
	if val <= 0xFFF {
		g.emit(asm_sub_imm(Reg(rd), fp, u32(val)))
	} else if val <= 0xFFFFFF {
		// Split into high (shifted by 12) and low parts
		high := (val >> 12) & 0xFFF
		low := val & 0xFFF
		g.emit(asm_sub_imm_lsl12(Reg(rd), fp, u32(high)))
		if low > 0 {
			g.emit(asm_sub_imm(Reg(rd), Reg(rd), u32(low)))
		}
	} else {
		// Very large offset: use register
		g.emit_mov_imm(10, u64(val))
		g.emit(asm_sub_fp_to_reg(Reg(rd), Reg(10)))
	}
}

fn (mut g Gen) emit_str_reg_offset(rt int, rn int, offset int) {
	if offset >= -255 && offset <= 255 {
		g.emit(asm_stur(Reg(rt), Reg(rn), offset))
	} else {
		// Large negative offset; use temp x10 for address
		imm := u64(-offset) // Positive imm
		g.emit_mov_imm(10, imm)
		g.emit(asm_sub_fp_to_reg(Reg(10), Reg(10)))
		g.emit(asm_str(Reg(rt), Reg(10)))
	}
}

fn (mut g Gen) emit_ldr_reg_offset(rt int, rn int, offset int) {
	if offset >= -255 && offset <= 255 {
		g.emit(asm_ldur(Reg(rt), Reg(rn), offset))
	} else {
		// Large negative offset; use temp x10 for address
		imm := u64(-offset) // Positive imm
		g.emit_mov_imm(10, imm)
		g.emit(asm_sub_fp_to_reg(Reg(10), Reg(10)))
		g.emit(asm_ldr(Reg(rt), Reg(10)))
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

pub fn (mut g Gen) link_executable(output_path string) {
	mut linker := Linker.new(g.macho)
	linker.link(output_path, '_main')
}

fn (mut g Gen) emit_mov_imm(rd int, imm u64) {
	// Assume imm < 65536; use MOVZ xd, #imm
	g.emit(asm_movz(Reg(rd), u32(imm & 0xFFFF)))
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
		g.emit(asm_movz(Reg(rd), u32(val)))
		return
	}

	// For small negative values (-1 to -65536), use MOVN
	// MOVN xd, #imm16 sets xd to NOT(imm16 << shift)
	// For -1: MOVN xd, #0 -> ~0 = -1
	// For -42: MOVN xd, #41 -> ~41 = -42
	if val >= -65536 && val < 0 {
		not_val := u32(~val) // ~(-42) = 41
		g.emit(asm_movn(Reg(rd), not_val))
		return
	}

	// For larger values, use MOVZ followed by MOVK instructions
	uval := u64(val)

	// Emit MOVZ for first chunk (always at shift 0)
	chunk0 := u32(uval & 0xFFFF)
	g.emit(asm_movz(Reg(rd), chunk0))

	// Emit MOVK for remaining non-zero chunks
	for shift := 16; shift < 64; shift += 16 {
		chunk := u32((uval >> shift) & 0xFFFF)
		if chunk != 0 {
			g.emit(asm_movk(Reg(rd), chunk, shift))
		}
	}
}

fn (mut g Gen) emit_mov_reg(rd int, rm int) {
	g.emit(asm_mov_reg(Reg(rd), Reg(rm)))
}

struct Interval {
mut:
	val_id   int
	start    int
	end      int
	has_call bool
}

// Calculate the size of a type in bytes
fn (g Gen) type_size(typ_id ssa.TypeID) int {
	if typ_id == 0 {
		return 0 // void
	}
	typ := g.mod.type_store.types[typ_id]
	match typ.kind {
		.void_t {
			return 0
		}
		.int_t {
			return if typ.width > 0 { (typ.width + 7) / 8 } else { 8 }
		}
		.float_t {
			return if typ.width > 0 { (typ.width + 7) / 8 } else { 8 }
		}
		.ptr_t {
			return 8
		} // 64-bit pointers on arm64
		.array_t {
			elem_size := g.type_size(typ.elem_type)
			return typ.len * elem_size
		}
		.struct_t {
			mut total := 0
			for field_typ in typ.fields {
				// Align each field to its natural alignment (simplified: 8 bytes)
				if total % 8 != 0 {
					total = (total + 7) & ~7
				}
				total += g.type_size(field_typ)
			}
			// Align struct size to 8 bytes
			if total % 8 != 0 {
				total = (total + 7) & ~7
			}
			return if total > 0 { total } else { 8 }
		}
		.func_t {
			return 8
		} // function pointer
		.label_t {
			return 0
		}
		.metadata_t {
			return 0
		}
	}
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

	mut sorted := []&Interval{cap: intervals.len}
	for _, i in intervals {
		sorted << i
	}
	sorted.sort(a.start < b.start)

	mut active := []&Interval{cap: 32}

	// Registers
	// Caller-saved (Temporaries): x9..x15
	// Callee-saved (Preserved): x19..x28
	// Reserve x8 and x9 as backend scratch registers
	// x10 is reserved as scratch for large offset operations
	short_regs := [11, 12, 13, 14, 15]
	long_regs := [19, 20, 21, 22, 23, 24, 25, 26, 27, 28]

	// Reusable arrays to avoid allocation in the hot loop
	mut used := []bool{len: 32, init: false}
	mut used_regs_set := []bool{len: 32, init: false}

	for i in sorted {
		// Remove expired intervals from active list
		mut j := 0
		for j < active.len {
			if active[j].end < i.start {
				active.delete(j)
			} else {
				j++
			}
		}

		// Reset used array
		for k in 0 .. 32 {
			used[k] = false
		}
		for a in active {
			used[g.reg_map[a.val_id]] = true
		}

		// Decide which pool to use (avoid clone)
		pool := if i.has_call { long_regs } else { short_regs }

		for r in pool {
			if !used[r] {
				g.reg_map[i.val_id] = r
				active << i
				// Only track used callee-saved regs for prologue saving
				if r >= 19 && r <= 28 && !used_regs_set[r] {
					used_regs_set[r] = true
					g.used_regs << r
				}
				break
			}
		}
	}
	g.used_regs.sort()
}
