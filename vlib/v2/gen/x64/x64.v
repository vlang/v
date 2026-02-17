// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import v2.mir
import v2.ssa
import encoding.binary

pub struct Gen {
	mod &mir.Module
mut:
	elf &ElfObject

	stack_map      map[int]int
	alloca_offsets map[int]int
	stack_size     int
	curr_offset    int

	block_offsets  map[int]int
	pending_labels map[int][]int

	// Register allocation
	reg_map   map[int]int
	used_regs []int

	cur_func_ret_type         int
	cur_func_abi_ret_indirect bool
	sret_save_offset          int
}

struct Interval {
mut:
	val_id   int
	start    int
	end      int
	has_call bool
}

pub fn Gen.new(mod &mir.Module) &Gen {
	return &Gen{
		mod: mod
		elf: ElfObject.new()
	}
}

pub fn (mut g Gen) gen() {
	for func in g.mod.funcs {
		g.gen_func(func)
	}

	// Generate Globals in .data
	for gvar in g.mod.globals {
		for g.elf.data_data.len % 8 != 0 {
			g.elf.data_data << 0
		}
		addr := u64(g.elf.data_data.len)
		g.elf.add_symbol(gvar.name, addr, false, 2)
		if gvar.is_constant {
			// For constants, write the initial value
			mut bytes := []u8{len: 8}
			binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
			for b in bytes {
				g.elf.data_data << b
			}
		} else {
			// For regular globals, initialize with zeros
			for _ in 0 .. 8 {
				g.elf.data_data << 0
			}
		}
	}
}

fn (mut g Gen) gen_func(func mir.Function) {
	if func.blocks.len == 0 {
		// Emit a minimal stub: just a ret instruction
		// This is needed for functions like __v_init_consts that are called but have no body
		g.curr_offset = g.elf.text_data.len
		g.elf.add_symbol(func.name, u64(g.curr_offset), true, 1)
		g.emit(0xc3) // ret
		return
	}
	g.curr_offset = g.elf.text_data.len
	g.stack_map = map[int]int{}
	g.alloca_offsets = map[int]int{}
	g.block_offsets = map[int]int{}
	g.pending_labels = map[int][]int{}
	g.reg_map = map[int]int{}
	g.used_regs = []int{}
	g.cur_func_ret_type = func.typ
	g.cur_func_abi_ret_indirect = func.abi_ret_indirect
	g.sret_save_offset = 0

	g.allocate_registers(func)

	// Calculate Stack Frame
	mut slot_offset := 8

	// Hidden sret pointer slot (SysV: incoming in RDI)
	if func.abi_ret_indirect {
		g.sret_save_offset = -slot_offset
		slot_offset += 8
	}

	for pi, pid in func.params {
		param_typ := g.mod.values[pid].typ
		param_size := g.type_size(param_typ)
		is_indirect_param := pi < func.abi_param_class.len && func.abi_param_class[pi] == .indirect
		if is_indirect_param && param_size > 0 {
			slot_offset = (slot_offset + 15) & ~0xF
			slot_offset += param_size
			g.stack_map[pid] = -slot_offset
		} else {
			g.stack_map[pid] = -slot_offset
			slot_offset += 8
		}
	}

	for blk_id in func.blocks {
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

				// Align to 16 bytes
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += alloc_size
				g.alloca_offsets[val_id] = -slot_offset
				slot_offset += 8 // Slot for the pointer
			}

			if instr.op == .call_sret {
				result_typ := g.mod.type_store.types[val.typ]
				if result_typ.kind == .struct_t {
					result_size := g.type_size(val.typ)
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += result_size
					g.stack_map[val_id] = -slot_offset
					continue
				}
			}

			if val_id in g.reg_map {
				continue
			}
			g.stack_map[val_id] = -slot_offset
			slot_offset += 8
		}
	}

	g.stack_size = (slot_offset + 16) & ~0xF

	g.elf.add_symbol(func.name, u64(g.curr_offset), true, 1)

	// Prologue
	asm_endbr64(mut g)
	asm_push_rbp(mut g)
	asm_mov_rbp_rsp(mut g)

	// Push callee-saved regs
	for r in g.used_regs {
		asm_push(mut g, Reg(r))
	}

	// sub rsp, stack_size
	if g.stack_size > 0 {
		if g.stack_size <= 127 {
			asm_sub_rsp_imm8(mut g, u8(g.stack_size))
		} else {
			asm_sub_rsp_imm32(mut g, u32(g.stack_size))
		}
	}

	// Move Params (ABI: RDI, RSI, RDX, RCX, R8, R9)
	// For sret functions, hidden pointer consumes RDI.
	abi_regs := [7, 6, 2, 1, 8, 9]
	arg_reg_base := if func.abi_ret_indirect { 1 } else { 0 }
	num_reg_args := 6 - arg_reg_base
	if func.abi_ret_indirect && g.sret_save_offset != 0 {
		asm_store_rbp_disp_reg(mut g, g.sret_save_offset, rdi)
	}
	for i, pid in func.params {
		is_indirect_param := i < func.abi_param_class.len && func.abi_param_class[i] == .indirect
		if i < num_reg_args {
			src := abi_regs[i + arg_reg_base]
			if is_indirect_param {
				g.copy_indirect_param_from_reg(pid, src)
			} else if reg := g.reg_map[pid] {
				asm_mov_reg_reg(mut g, Reg(reg), Reg(src))
			} else {
				offset := g.stack_map[pid]
				asm_store_rbp_disp_reg(mut g, offset, Reg(src))
			}
		} else {
			// Stack parameters start at [rbp+16].
			stack_param_offset := 16 + (i - num_reg_args) * 8
			// Load from stack into RAX, then store to our slot
			asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
			if is_indirect_param {
				g.copy_indirect_param_from_reg(pid, int(rax))
			} else if reg := g.reg_map[pid] {
				asm_mov_reg_reg(mut g, Reg(reg), rax)
			} else {
				offset := g.stack_map[pid]
				asm_store_rbp_disp_reg(mut g, offset, rax)
			}
		}
	}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		g.block_offsets[blk_id] = g.elf.text_data.len - g.curr_offset

		if offsets := g.pending_labels[blk_id] {
			for off in offsets {
				target := g.block_offsets[blk_id]
				rel := target - (off + 4)
				abs_off := g.curr_offset + off
				g.write_u32(abs_off, u32(rel))
			}
		}

		for val_id in blk.instrs {
			g.gen_instr(val_id)
		}
	}
}

fn (mut g Gen) gen_instr(val_id int) {
	instr := g.mod.instrs[g.mod.values[val_id].index]
	op := g.selected_opcode(instr)

	// Temps: 0=RAX, 1=RCX

	match op {
		.add, .sub, .mul, .sdiv, .srem, .and_, .or_, .xor, .shl, .ashr, .lshr, .eq, .ne, .lt, .gt,
		.le, .ge {
			g.load_val_to_reg(0, instr.operands[0]) // RAX
			g.load_val_to_reg(1, instr.operands[1]) // RCX

			match op {
				.add {
					asm_add_rax_rcx(mut g)
				}
				.sub {
					asm_sub_rax_rcx(mut g)
				}
				.mul {
					asm_imul_rax_rcx(mut g)
				}
				.sdiv {
					asm_cqo(mut g)
					asm_idiv_rcx(mut g)
				}
				.srem {
					asm_cqo(mut g)
					asm_idiv_rcx(mut g)
					asm_mov_rax_rdx(mut g)
				}
				.and_ {
					asm_and_rax_rcx(mut g)
				}
				.or_ {
					asm_or_rax_rcx(mut g)
				}
				.xor {
					asm_xor_rax_rcx(mut g)
				}
				.shl {
					asm_shl_rax_cl(mut g)
				}
				.ashr {
					asm_sar_rax_cl(mut g)
				}
				.lshr {
					asm_shr_rax_cl(mut g)
				}
				.eq, .ne, .lt, .gt, .le, .ge {
					asm_cmp_rax_rcx(mut g)
					cc := match op {
						.eq { cc_e }
						.ne { cc_ne }
						.lt { cc_l }
						.gt { cc_g }
						.le { cc_le }
						.ge { cc_ge }
						else { cc_e }
					}
					asm_setcc_al_movzx(mut g, cc)
				}
				else {}
			}
			g.store_reg_to_val(0, val_id)
		}
		.store {
			src_id := instr.operands[0]
			src_typ := g.mod.values[src_id].typ
			src_type_info := g.mod.type_store.types[src_typ]
			src_size := g.type_size(src_typ)
			if src_type_info.kind == .struct_t && src_size > 16 {
				g.load_struct_src_address_to_reg(int(r10), src_id, src_typ)
				g.load_val_to_reg(int(r11), instr.operands[1])
				num_chunks := (src_size + 7) / 8
				for i := 0; i < num_chunks; i++ {
					disp := i * 8
					asm_mov_rax_mem_base_disp(mut g, r10, disp)
					asm_mov_mem_base_disp_rax(mut g, r11, disp)
				}
			} else {
				g.load_val_to_reg(0, src_id) // Val -> RAX
				g.load_val_to_reg(1, instr.operands[1]) // Ptr -> RCX
				asm_mov_mem_rcx_rax(mut g)
			}
		}
		.load {
			g.load_val_to_reg(1, instr.operands[0]) // Ptr -> RCX
			asm_mov_rax_mem_rcx(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.alloca {
			off := g.alloca_offsets[val_id]
			// lea rax, [rbp + off]
			if off >= -128 && off <= 127 {
				asm_lea_rax_rbp_disp8(mut g, i8(off))
			} else {
				asm_lea_rax_rbp_disp32(mut g, off)
			}
			g.store_reg_to_val(0, val_id)
		}
		.get_element_ptr {
			g.load_val_to_reg(0, instr.operands[0]) // Base -> RAX
			g.load_val_to_reg(1, instr.operands[1]) // Index -> RCX
			// add rax, (rcx << 3)
			asm_shl_rcx_3(mut g)
			asm_add_rax_rcx(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.call {
			abi_regs := [7, 6, 2, 1, 8, 9]
			num_args := instr.operands.len - 1

			// Push stack arguments in reverse order (args 7+)
			mut stack_args := 0
			if num_args > 6 {
				stack_args = num_args - 6
				// Align stack to 16 bytes if odd number of stack args
				if stack_args % 2 == 1 {
					asm_push(mut g, rax)
				}
				// Push in reverse order
				for i := num_args; i > 6; i-- {
					g.load_call_arg_to_reg(0, instr.operands[i], i - 1, instr) // RAX
					asm_push(mut g, rax)
				}
			}

			// Load register arguments
			for i in 1 .. instr.operands.len {
				if i - 1 < 6 {
					g.load_call_arg_to_reg(abi_regs[i - 1], instr.operands[i], i - 1,
						instr)
				}
			}
			fn_val := g.mod.values[instr.operands[0]]

			// xor eax, eax (Clear AL for variadic function calls)
			asm_xor_eax_eax(mut g)

			if fn_val.name != '' && fn_val.kind in [.unknown, .func_ref] {
				asm_call_rel32(mut g)
				sym_idx := g.elf.add_undefined(fn_val.name)
				// Use R_X86_64_PLT32 (4) for function calls to support shared libraries (libc)
				g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 4, -4)
				g.emit_u32(0)
			} else {
				g.load_val_to_reg(int(r10), instr.operands[0])
				asm_call_r10(mut g)
			}

			// Clean up stack arguments
			if stack_args > 0 {
				cleanup := (stack_args + (stack_args % 2)) * 8
				if cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(cleanup))
				}
			}

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_reg_to_val(0, val_id)
			}
		}
		.call_sret {
			// SysV x86_64: hidden sret pointer in RDI, then user args in RSI,RDX,RCX,R8,R9.
			abi_regs := [6, 2, 1, 8, 9]
			num_args := instr.operands.len - 1

			// Load destination pointer to RDI.
			g.load_address_of_val_to_reg(7, val_id)

			// Push stack arguments in reverse order (args 6+ after hidden sret ptr).
			mut stack_args := 0
			if num_args > 5 {
				stack_args = num_args - 5
				if stack_args % 2 == 1 {
					asm_push(mut g, rax)
				}
				for i := num_args; i > 5; i-- {
					g.load_call_arg_to_reg(0, instr.operands[i], i - 1, instr) // RAX
					asm_push(mut g, rax)
				}
			}

			// Load register arguments.
			for i := 1; i <= num_args && i <= 5; i++ {
				g.load_call_arg_to_reg(abi_regs[i - 1], instr.operands[i], i - 1, instr)
			}

			fn_val := g.mod.values[instr.operands[0]]

			// xor eax, eax (Clear AL for variadic function calls)
			asm_xor_eax_eax(mut g)

			if fn_val.name != '' && fn_val.kind in [.unknown, .func_ref] {
				asm_call_rel32(mut g)
				sym_idx := g.elf.add_undefined(fn_val.name)
				g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 4, -4)
				g.emit_u32(0)
			} else {
				g.load_val_to_reg(int(r10), instr.operands[0])
				asm_call_r10(mut g)
			}

			// Clean up stack arguments
			if stack_args > 0 {
				cleanup := (stack_args + (stack_args % 2)) * 8
				if cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(cleanup))
				}
			}
		}
		.call_indirect {
			// Indirect call through function pointer
			// operands[0] is the function pointer, rest are arguments
			abi_regs := [7, 6, 2, 1, 8, 9]
			num_args := instr.operands.len - 1

			// Push stack arguments in reverse order (args 7+)
			mut stack_args := 0
			if num_args > 6 {
				stack_args = num_args - 6
				// Align stack to 16 bytes if odd number of stack args
				if stack_args % 2 == 1 {
					asm_push(mut g, rax)
				}
				for i := num_args; i > 6; i-- {
					g.load_call_arg_to_reg(0, instr.operands[i], i - 1, instr)
					asm_push(mut g, rax)
				}
			}

			// Load register arguments
			for i in 1 .. instr.operands.len {
				if i - 1 < 6 {
					g.load_call_arg_to_reg(abi_regs[i - 1], instr.operands[i], i - 1,
						instr)
				}
			}

			// Load function pointer to r10 (caller-saved, not used for args)
			g.load_val_to_reg(10, instr.operands[0])

			// xor eax, eax (Clear AL for variadic function calls)
			asm_xor_eax_eax(mut g)

			// call *r10
			asm_call_r10(mut g)

			// Clean up stack arguments
			if stack_args > 0 {
				cleanup := (stack_args + (stack_args % 2)) * 8
				if cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(cleanup))
				}
			}

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_reg_to_val(0, val_id)
			}
		}
		.ret {
			if g.cur_func_abi_ret_indirect {
				if g.sret_save_offset != 0 {
					asm_load_reg_rbp_disp(mut g, rdi, g.sret_save_offset)
				}
				if instr.operands.len > 0 {
					ret_val_id := instr.operands[0]
					ret_size := g.type_size(g.cur_func_ret_type)
					if ret_size > 0 {
						g.load_struct_src_address_to_reg(int(r10), ret_val_id, g.cur_func_ret_type)
						num_chunks := (ret_size + 7) / 8
						for i := 0; i < num_chunks; i++ {
							disp := i * 8
							asm_mov_rax_mem_base_disp(mut g, r10, disp)
							asm_mov_mem_base_disp_rax(mut g, rdi, disp)
						}
					}
				}
				// SysV returns sret pointer in RAX.
				asm_mov_reg_reg(mut g, rax, rdi)
			} else if instr.operands.len > 0 {
				g.load_val_to_reg(0, instr.operands[0])
			}
			// Cleanup Stack
			if g.stack_size > 0 {
				if g.stack_size <= 127 {
					asm_add_rsp_imm8(mut g, u8(g.stack_size))
				} else {
					asm_add_rsp_imm32(mut g, u32(g.stack_size))
				}
			}
			// Pop callee-saved regs (reverse order)
			for i := g.used_regs.len - 1; i >= 0; i-- {
				asm_pop(mut g, Reg(g.used_regs[i]))
			}
			asm_pop_rbp(mut g)
			asm_ret(mut g)
		}
		.jmp {
			target_idx := g.mod.values[instr.operands[0]].index
			g.emit_jmp(target_idx)
		}
		.br {
			cond_id := instr.operands[0]
			true_blk := g.mod.values[instr.operands[1]].index
			false_blk := g.mod.values[instr.operands[2]].index

			// Test condition register directly if register-allocated
			if reg := g.reg_map[cond_id] {
				asm_test_reg_reg(mut g, Reg(reg))
			} else {
				g.load_val_to_reg(0, cond_id)
				asm_test_rax_rax(mut g)
			}

			// Emit je false_blk (jump if zero/false)
			asm_je_rel32(mut g)
			g.record_pending_label(false_blk)
			g.emit_u32(0)
			// Jump to true block (can't assume it's the next block)
			g.emit_jmp(true_blk)
		}
		.switch_ {
			g.load_val_to_reg(0, instr.operands[0]) // RAX
			for i := 2; i < instr.operands.len; i += 2 {
				g.load_val_to_reg(1, instr.operands[i])
				asm_cmp_rax_rcx(mut g)
				asm_je_rel32(mut g)
				target_idx := g.mod.values[instr.operands[i + 1]].index
				if off := g.block_offsets[target_idx] {
					rel := off - (g.elf.text_data.len - g.curr_offset + 4)
					g.emit_u32(u32(rel))
				} else {
					g.record_pending_label(target_idx)
					g.emit_u32(0)
				}
			}
			def_idx := g.mod.values[instr.operands[1]].index
			g.emit_jmp(def_idx)
		}
		.assign {
			dest_id := instr.operands[0]
			src_id := instr.operands[1]
			g.load_val_to_reg(0, src_id)
			g.store_reg_to_val(0, dest_id)
		}
		.bitcast, .trunc, .sext, .zext {
			// For x64: all registers are 64-bit, so integer type conversions
			// are mostly just copies. Truncation uses the lower bits naturally.
			// Sign/zero extension matters for 8/16/32 bit values.
			if instr.operands.len > 0 {
				g.load_val_to_reg(0, instr.operands[0])
				if op == .sext {
					// Sign-extend based on source type width
					src_typ := g.mod.values[instr.operands[0]].typ
					src_size := g.type_size(src_typ)
					match src_size {
						1 { asm_movsx_rax_al(mut g) }
						2 { asm_movsx_rax_ax(mut g) }
						4 { asm_movsxd_rax_eax(mut g) }
						else {}
					}
				} else if op == .zext {
					src_typ := g.mod.values[instr.operands[0]].typ
					src_size := g.type_size(src_typ)
					match src_size {
						1 { asm_movzx_rax_al(mut g) }
						2 { asm_movzx_rax_ax(mut g) }
						4 { asm_mov_eax_eax(mut g) }
						else {}
					}
				} else if op == .trunc {
					// Truncation: mask to target width
					dst_size := g.type_size(g.mod.values[val_id].typ)
					match dst_size {
						1 { asm_and_rax_imm32(mut g, 0xFF) }
						2 { asm_and_rax_imm32(mut g, 0xFFFF) }
						4 { asm_mov_eax_eax(mut g) }
						else {}
					}
				}
				g.store_reg_to_val(0, val_id)
			}
		}
		.fadd, .fsub, .fmul, .fdiv, .frem {
			// Floating-point operations using SSE2
			g.load_val_to_reg(0, instr.operands[0]) // LHS bits -> RAX
			asm_movq_xmm0_rax(mut g) // RAX -> xmm0
			g.load_val_to_reg(1, instr.operands[1]) // RHS bits -> RCX
			asm_movq_xmm1_rcx(mut g) // RCX -> xmm1

			match op {
				.fadd { asm_addsd_xmm0_xmm1(mut g) }
				.fsub { asm_subsd_xmm0_xmm1(mut g) }
				.fmul { asm_mulsd_xmm0_xmm1(mut g) }
				.fdiv { asm_divsd_xmm0_xmm1(mut g) }
				.frem {
					// xmm0 = xmm0 - trunc(xmm0/xmm1) * xmm1
					asm_movsd_xmm2_xmm0(mut g)
					asm_divsd_xmm2_xmm1(mut g)
					asm_roundsd_xmm2_xmm2_trunc(mut g)
					asm_mulsd_xmm2_xmm1(mut g)
					asm_subsd_xmm0_xmm2(mut g)
				}
				else {}
			}

			asm_movq_rax_xmm0(mut g) // xmm0 -> RAX
			g.store_reg_to_val(0, val_id)
		}
		.fptosi {
			// Float to signed integer conversion
			g.load_val_to_reg(0, instr.operands[0])
			asm_movq_xmm0_rax(mut g)
			asm_cvttsd2si_rax_xmm0(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.sitofp {
			// Signed integer to float conversion
			g.load_val_to_reg(0, instr.operands[0])
			asm_cvtsi2sd_xmm0_rax(mut g)
			asm_movq_rax_xmm0(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.fptoui, .uitofp {
			// For now, treat same as signed versions
			g.load_val_to_reg(0, instr.operands[0])
			g.store_reg_to_val(0, val_id)
		}
		.inline_string_init {
			// Create string struct by value: { str, len, is_lit }
			// operands: [str_ptr, len, is_lit]
			str_ptr_id := instr.operands[0]
			len_id := instr.operands[1]
			is_lit_id := instr.operands[2]

			// stack_map[val_id] points to the 8-byte pointer slot.
			// The 24-byte struct data lives right above it (at +8).
			base_offset := g.stack_map[val_id]
			struct_offset := base_offset + 8

			// Store str field (offset 0)
			g.load_val_to_reg(0, str_ptr_id)
			asm_store_rbp_disp_reg(mut g, struct_offset, rax)

			// Store len field (offset 8)
			g.load_val_to_reg(1, len_id)
			asm_store_rbp_disp_reg(mut g, struct_offset + 8, rcx)

			// Store is_lit field (offset 16)
			g.load_val_to_reg(0, is_lit_id)
			asm_store_rbp_disp_reg(mut g, struct_offset + 16, rax)

			// Return pointer to struct
			asm_lea_reg_rbp_disp(mut g, rax, struct_offset)
			g.store_reg_to_val(0, val_id)
		}
		.extractvalue {
			// Extract element from tuple/struct
			// operands: [tuple_val, index]
			tuple_id := instr.operands[0]
			idx_val := g.mod.values[instr.operands[1]]
			idx := idx_val.name.int()
			tuple_val := g.mod.values[tuple_id]
			mut tuple_is_large_agg := false
			mut field_byte_off := idx * 8
			mut field_elem_size := 8
			if tuple_val.typ > 0 && tuple_val.typ < g.mod.type_store.types.len {
				tuple_typ := g.mod.type_store.types[tuple_val.typ]
				tuple_is_large_agg = g.type_size(tuple_val.typ) > 16
					&& tuple_typ.kind in [.struct_t, .array_t]
				if tuple_typ.kind == .struct_t && idx >= 0 {
					field_byte_off = g.struct_field_offset_bytes(tuple_val.typ, idx)
					if idx < tuple_typ.fields.len {
						field_elem_size = g.type_size(tuple_typ.fields[idx])
						if field_elem_size <= 0 {
							field_elem_size = 8
						}
					}
				}
			}

			// Get tuple's stack location and load from offset
			if tuple_offset := g.stack_map[tuple_id] {
				if tuple_is_large_agg && idx >= 0
					&& g.large_aggregate_stack_value_is_pointer(tuple_id) {
					// Load pointer to aggregate, then load field from it
					asm_load_reg_rbp_disp(mut g, rcx, tuple_offset)
					if field_elem_size > 8 {
						// Multi-word struct field: copy all words
						if dst_offset := g.stack_map[val_id] {
							num_words := (field_elem_size + 7) / 8
							for w in 0 .. num_words {
								asm_load_reg_base_disp(mut g, rax, rcx, field_byte_off + w * 8)
								asm_store_rbp_disp_reg(mut g, dst_offset + w * 8, rax)
							}
						} else {
							asm_load_reg_base_disp(mut g, rax, rcx, field_byte_off)
							g.store_reg_to_val(0, val_id)
						}
					} else {
						asm_load_reg_base_disp(mut g, rax, rcx, field_byte_off)
						g.store_reg_to_val(0, val_id)
					}
				} else if field_elem_size > 8 {
					// Multi-word struct field stored inline
					if dst_offset := g.stack_map[val_id] {
						src_offset := tuple_offset + field_byte_off
						num_words := (field_elem_size + 7) / 8
						for w in 0 .. num_words {
							asm_load_reg_rbp_disp(mut g, rax, src_offset + w * 8)
							asm_store_rbp_disp_reg(mut g, dst_offset + w * 8, rax)
						}
					} else {
						field_offset := tuple_offset + field_byte_off
						asm_load_reg_rbp_disp(mut g, rax, field_offset)
						g.store_reg_to_val(0, val_id)
					}
				} else if field_elem_size in [1, 2, 4] {
					// Use sized load to avoid reading adjacent packed fields
					field_offset := tuple_offset + field_byte_off
					asm_lea_reg_rbp_disp(mut g, rcx, field_offset)
					match field_elem_size {
						1 { asm_movzx_rax_byte_mem_rcx(mut g) }
						2 { asm_movzx_rax_word_mem_rcx(mut g) }
						4 { asm_mov_eax_mem_rcx(mut g) }
						else {}
					}
					g.store_reg_to_val(0, val_id)
				} else {
					field_offset := tuple_offset + field_byte_off
					asm_load_reg_rbp_disp(mut g, rax, field_offset)
					g.store_reg_to_val(0, val_id)
				}
			} else if reg := g.reg_map[tuple_id] {
				// Register-allocated tuple
				if tuple_is_large_agg && idx >= 0 {
					if field_elem_size > 8 {
						if dst_offset := g.stack_map[val_id] {
							num_words := (field_elem_size + 7) / 8
							for w in 0 .. num_words {
								asm_load_reg_base_disp(mut g, rax, Reg(reg), field_byte_off + w * 8)
								asm_store_rbp_disp_reg(mut g, dst_offset + w * 8, rax)
							}
						} else {
							asm_load_reg_base_disp(mut g, rax, Reg(reg), field_byte_off)
							g.store_reg_to_val(0, val_id)
						}
					} else {
						asm_load_reg_base_disp(mut g, rax, Reg(reg), field_byte_off)
						g.store_reg_to_val(0, val_id)
					}
				} else if idx == 0 {
					if reg != 0 {
						asm_mov_reg_reg(mut g, rax, Reg(reg))
					}
					// Mask to field width for sub-8-byte fields
					if field_elem_size in [1, 2, 4] {
						mask := (u32(1) << u32(field_elem_size * 8)) - 1
						asm_and_rax_imm32(mut g, mask)
					}
					g.store_reg_to_val(0, val_id)
				} else {
					// Higher indices packed in same register - shift then mask
					g.load_val_to_reg(0, tuple_id)
					if field_byte_off > 0 && field_byte_off < 8 {
						asm_shr_rax_imm8(mut g, u8(field_byte_off * 8))
					}
					if field_elem_size in [1, 2, 4] {
						mask := (u32(1) << u32(field_elem_size * 8)) - 1
						asm_and_rax_imm32(mut g, mask)
					}
					g.store_reg_to_val(0, val_id)
				}
			} else {
				// Tuple not in stack_map or reg_map - fallback
				g.load_val_to_reg(0, tuple_id)
				g.store_reg_to_val(0, val_id)
			}
		}
		.struct_init {
			// Create struct from field values: operands are field values in order
			result_offset := g.stack_map[val_id]
			struct_typ := g.mod.type_store.types[instr.typ]
			struct_size := g.type_size(instr.typ)
			num_chunks := if struct_size > 0 { (struct_size + 7) / 8 } else { 1 }

			// Zero-initialize the entire struct first
			asm_xor_reg_reg(mut g, rax)
			for i in 0 .. num_chunks {
				asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
			}

			// Store each field value at its proper offset
			for fi, field_id in instr.operands {
				mut field_off := fi * 8
				if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
					field_off = g.struct_field_offset_bytes(instr.typ, fi)
				}

				mut field_typ_id := ssa.TypeID(0)
				if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
					field_typ_id = struct_typ.fields[fi]
				} else if field_id > 0 && field_id < g.mod.values.len {
					field_typ_id = g.mod.values[field_id].typ
				}
				mut field_size := if field_typ_id > 0 { g.type_size(field_typ_id) } else { 8 }
				if field_size <= 0 {
					field_size = 8
				}

				// Skip zero constants (already zeroed above)
				field_val := g.mod.values[field_id]
				if field_val.kind == .constant && field_val.name == '0' {
					continue
				}

				if field_size <= 8 {
					g.load_val_to_reg(0, field_id)
					asm_store_rbp_disp_reg(mut g, result_offset + field_off, rax)
				} else {
					// Multi-word field (nested struct)
					field_chunks := (field_size + 7) / 8
					if field_offset := g.stack_map[field_id] {
						mut src_ptr_reg := r10
						if field_size > 16
							&& g.large_aggregate_stack_value_is_pointer(field_id) {
							asm_load_reg_rbp_disp(mut g, r10, field_offset)
						} else {
							asm_lea_reg_rbp_disp(mut g, r10, field_offset)
						}
						for w in 0 .. field_chunks {
							asm_load_reg_base_disp(mut g, rax, src_ptr_reg, w * 8)
							asm_store_rbp_disp_reg(mut g, result_offset + field_off + w * 8,
								rax)
						}
					} else {
						// Fallback: store first word
						g.load_val_to_reg(0, field_id)
						asm_store_rbp_disp_reg(mut g, result_offset + field_off, rax)
					}
				}
			}
		}
		.insertvalue {
			// Insert element into tuple/struct
			// operands: [tuple_val, elem_val, index]
			tuple_id := instr.operands[0]
			elem_id := instr.operands[1]
			idx_val := g.mod.values[instr.operands[2]]
			idx := idx_val.name.int()

			// Get result's stack location
			result_offset := g.stack_map[val_id]
			tuple_typ := g.mod.type_store.types[instr.typ]
			tuple_size := g.type_size(instr.typ)
			num_chunks := if tuple_size > 0 { (tuple_size + 7) / 8 } else { 1 }
			mut elem_off := idx * 8
			if tuple_typ.kind == .struct_t && idx >= 0 && idx < tuple_typ.fields.len {
				elem_off = g.struct_field_offset_bytes(instr.typ, idx)
			}

			// Copy existing tuple data if not undef.
			tuple_val := g.mod.values[tuple_id]
			if !(tuple_val.kind == .constant && tuple_val.name == 'undef') {
				mut copied_tuple := false
				if tuple_offset := g.stack_map[tuple_id] {
					if tuple_size > 16
						&& g.large_aggregate_stack_value_is_pointer(tuple_id) {
						asm_load_reg_rbp_disp(mut g, r10, tuple_offset)
						for i in 0 .. num_chunks {
							asm_load_reg_base_disp(mut g, rax, r10, i * 8)
							asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
						}
					} else {
						for i in 0 .. num_chunks {
							asm_load_reg_rbp_disp(mut g, rax, tuple_offset + i * 8)
							asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
						}
					}
					copied_tuple = true
				} else if src_reg := g.reg_map[tuple_id] {
					for i in 0 .. num_chunks {
						asm_load_reg_base_disp(mut g, rax, Reg(src_reg), i * 8)
						asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
					}
					copied_tuple = true
				}
				if !copied_tuple {
					// Deterministic fallback: zero out
					asm_xor_reg_reg(mut g, rax)
					for i in 0 .. num_chunks {
						asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
					}
				}
			} else {
				// Start from zeroed storage for `insertvalue(undef, ...)`
				asm_xor_reg_reg(mut g, rax)
				for i in 0 .. num_chunks {
					asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
				}
			}

			// Store the new element at the specified index
			mut elem_typ_id := ssa.TypeID(0)
			if tuple_typ.kind == .struct_t && idx >= 0 && idx < tuple_typ.fields.len {
				elem_typ_id = tuple_typ.fields[idx]
			} else if elem_id > 0 && elem_id < g.mod.values.len {
				elem_typ_id = g.mod.values[elem_id].typ
			}
			mut elem_size := if elem_typ_id > 0 { g.type_size(elem_typ_id) } else { 8 }
			if elem_size <= 0 {
				elem_size = 8
			}
			if elem_size <= 8 {
				g.load_val_to_reg(0, elem_id)
				asm_store_rbp_disp_reg(mut g, result_offset + elem_off, rax)
			} else {
				elem_chunks := (elem_size + 7) / 8
				mut copied_elem := false
				if elem_offset := g.stack_map[elem_id] {
					if elem_size > 16
						&& g.large_aggregate_stack_value_is_pointer(elem_id) {
						asm_load_reg_rbp_disp(mut g, r10, elem_offset)
					} else {
						asm_lea_reg_rbp_disp(mut g, r10, elem_offset)
					}
					for i in 0 .. elem_chunks {
						asm_load_reg_base_disp(mut g, rax, r10, i * 8)
						asm_store_rbp_disp_reg(mut g, result_offset + elem_off + i * 8,
							rax)
					}
					copied_elem = true
				} else if src_reg := g.reg_map[elem_id] {
					for i in 0 .. elem_chunks {
						asm_load_reg_base_disp(mut g, rax, Reg(src_reg), i * 8)
						asm_store_rbp_disp_reg(mut g, result_offset + elem_off + i * 8,
							rax)
					}
					copied_elem = true
				}
				if !copied_elem {
					// Best effort fallback: store first word, clear the rest.
					g.load_val_to_reg(0, elem_id)
					asm_store_rbp_disp_reg(mut g, result_offset + elem_off, rax)
					asm_xor_reg_reg(mut g, rax)
					for i in 1 .. elem_chunks {
						asm_store_rbp_disp_reg(mut g, result_offset + elem_off + i * 8,
							rax)
					}
				}
			}
		}
		.phi {
			// Phi nodes are eliminated by optimization (converted to assignments)
			// but the instructions remain in the block. We ignore them here.
		}
		.unreachable {
			// Emit UD2 instruction (undefined trap)
			asm_ud2(mut g)
		}
		else {
			eprintln('x64: unknown op ${op} (${instr.selected_op})')
		}
	}
}

fn (g Gen) selected_opcode(instr mir.Instruction) ssa.OpCode {
	if instr.selected_op == '' {
		return instr.op
	}
	suffix := if instr.selected_op.contains('.') {
		instr.selected_op.all_after('.')
	} else {
		instr.selected_op
	}
	return match suffix {
		'add_rr' { .add }
		'sub_rr' { .sub }
		'mul_rr' { .mul }
		'sdiv_rr' { .sdiv }
		'and_rr' { .and_ }
		'or_rr' { .or_ }
		'xor_rr' { .xor }
		'load_mr' { .load }
		'store_rm' { .store }
		'call' { .call }
		'call_indirect' { .call_indirect }
		'call_sret' { .call_sret }
		'ret' { .ret }
		'br' { .br }
		'jmp' { .jmp }
		'switch' { .switch_ }
		'copy' { .assign }
		else { instr.op }
	}
}

fn (mut g Gen) emit_jmp(target_idx int) {
	asm_jmp_rel32(mut g)
	if off := g.block_offsets[target_idx] {
		rel := off - (g.elf.text_data.len - g.curr_offset + 4)
		g.emit_u32(u32(rel))
	} else {
		g.record_pending_label(target_idx)
		g.emit_u32(0)
	}
}

fn (mut g Gen) load_call_arg_to_reg(reg int, val_id int, arg_idx int, instr mir.Instruction) {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	if is_indirect {
		g.load_address_of_val_to_reg(reg, val_id)
		return
	}
	g.load_val_to_reg(reg, val_id)
}

fn (mut g Gen) load_struct_src_address_to_reg(reg int, val_id int, expected_struct_typ int) {
	val := g.mod.values[val_id]
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		val_typ := g.mod.type_store.types[val.typ]
		if val_typ.kind == .ptr_t && val_typ.elem_type == expected_struct_typ {
			g.load_val_to_reg(reg, val_id)
			return
		}
	}
	g.load_address_of_val_to_reg(reg, val_id)
}

fn (mut g Gen) copy_indirect_param_from_reg(param_id int, src_reg int) {
	param_typ := g.mod.values[param_id].typ
	param_size := g.type_size(param_typ)
	if param_size <= 0 {
		offset := g.stack_map[param_id]
		asm_store_rbp_disp_reg(mut g, offset, Reg(src_reg))
		return
	}
	if src_reg != int(r10) {
		asm_mov_reg_reg(mut g, r10, Reg(src_reg))
	}
	g.load_address_of_val_to_reg(int(r11), param_id)
	num_chunks := (param_size + 7) / 8
	for i := 0; i < num_chunks; i++ {
		disp := i * 8
		asm_mov_rax_mem_base_disp(mut g, r10, disp)
		asm_mov_mem_base_disp_rax(mut g, r11, disp)
	}
}

fn (mut g Gen) load_val_to_reg(reg int, val_id int) {
	val := g.mod.values[val_id]
	if val.kind == .constant {
		if val.name.starts_with('"') {
			str_content := val.name.trim('"')
			// Handle escapes like arm64.v
			mut raw_bytes := []u8{}
			mut i := 0
			for i < str_content.len {
				if str_content[i] == `\\` && i + 1 < str_content.len {
					match str_content[i + 1] {
						`n` { raw_bytes << 10 }
						`t` { raw_bytes << 9 }
						`r` { raw_bytes << 13 }
						`\\` { raw_bytes << 92 }
						`"` { raw_bytes << 34 }
						`'` { raw_bytes << 39 }
						else { raw_bytes << str_content[i + 1] }
					}
					i += 2
				} else {
					raw_bytes << str_content[i]
					i++
				}
			}

			str_offset := g.elf.rodata.len
			g.elf.rodata << raw_bytes
			g.elf.rodata << 0
			sym_name := 'L_str_${g.curr_offset}_${str_offset}'
			sym_idx := g.elf.add_symbol(sym_name, u64(str_offset), false, 3)

			// lea reg, [rip + disp]
			asm_lea_reg_rip(mut g, Reg(reg))
			g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
			g.emit_u32(0)
		} else {
			int_val := val.name.i64()
			if int_val == 0 {
				asm_xor_reg_reg(mut g, Reg(reg))
			} else if int_val > 0 && int_val <= 0x7FFFFFFF {
				asm_mov_reg_imm32(mut g, Reg(reg), u32(int_val))
			} else {
				asm_mov_reg_imm64(mut g, Reg(reg), u64(int_val))
			}
		}
	} else if val.kind == .global {
		asm_lea_reg_rip(mut g, Reg(reg))
		sym_idx := g.elf.add_undefined(val.name)
		g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
		g.emit_u32(0)
	} else {
		if reg_idx := g.reg_map[val_id] {
			if reg_idx != reg {
				asm_mov_reg_reg(mut g, Reg(reg), Reg(reg_idx))
			}
		} else {
			offset := g.stack_map[val_id]
			asm_load_reg_rbp_disp(mut g, Reg(reg), offset)
		}
	}
}

fn (mut g Gen) store_reg_to_val(reg int, val_id int) {
	if reg_idx := g.reg_map[val_id] {
		if reg_idx != reg {
			asm_mov_reg_reg(mut g, Reg(reg_idx), Reg(reg))
		}
	} else {
		offset := g.stack_map[val_id]
		asm_store_rbp_disp_reg(mut g, offset, Reg(reg))
	}
}

fn (g Gen) type_size(typ_id ssa.TypeID) int {
	if typ_id == 0 {
		return 0
	}
	if typ_id < 0 || typ_id >= g.mod.type_store.types.len {
		return 8
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
		}
		.array_t {
			return typ.len * g.type_size(typ.elem_type)
		}
		.struct_t {
			mut total := 0
			for field_typ in typ.fields {
				if total % 8 != 0 {
					total = (total + 7) & ~7
				}
				total += g.type_size(field_typ)
			}
			if total % 8 != 0 {
				total = (total + 7) & ~7
			}
			return if total > 0 { total } else { 8 }
		}
		.func_t {
			return 8
		}
		.label_t, .metadata_t {
			return 0
		}
	}
}

fn (mut g Gen) load_address_of_val_to_reg(reg int, val_id int) {
	offset := g.stack_map[val_id]
	if offset != 0 {
		if offset >= -128 && offset <= 127 {
			asm_lea_rax_rbp_disp8(mut g, i8(offset))
		} else {
			asm_lea_rax_rbp_disp32(mut g, offset)
		}
		if reg != 0 {
			asm_mov_reg_reg(mut g, Reg(reg), rax)
		}
		return
	}
	// Fallback: value already holds a pointer.
	g.load_val_to_reg(reg, val_id)
}

fn (g Gen) map_reg(r int) u8 {
	return u8(r)
}

fn (mut g Gen) emit(b u8) {
	g.elf.text_data << b
}

fn (mut g Gen) emit_u32(v u32) {
	g.emit(u8(v))
	g.emit(u8(v >> 8))
	g.emit(u8(v >> 16))
	g.emit(u8(v >> 24))
}

fn (mut g Gen) emit_u64(v u64) {
	g.emit_u32(u32(v))
	g.emit_u32(u32(v >> 32))
}

fn (mut g Gen) record_pending_label(blk int) {
	off := g.elf.text_data.len - g.curr_offset
	g.pending_labels[blk] << off
}

fn (mut g Gen) write_u32(off int, v u32) {
	binary.little_endian_put_u32(mut g.elf.text_data[off..off + 4], v)
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}

// Register Allocation Logic

fn (mut g Gen) allocate_registers(func mir.Function) {
	mut intervals := map[int]&Interval{}
	mut instr_idx := 0

	// Track which values are alloca results - don't register allocate these
	// as they hold addresses that may be needed across the function
	mut alloca_vals := map[int]bool{}

	for i, pid in func.params {
		if i < func.abi_param_class.len && func.abi_param_class[i] == .indirect {
			alloca_vals[pid] = true
			continue
		}
		intervals[pid] = &Interval{
			val_id: pid
			start:  0
			end:    0
		}
	}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
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
			// Mark alloca results as non-register-allocatable
			if instr.op in [.alloca, .call_sret] {
				alloca_vals[val_id] = true
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
	}

	mut sorted := []&Interval{}
	for _, i in intervals {
		sorted << i
	}
	sorted.sort(a.start < b.start)

	mut active := []&Interval{}
	// Use callee-saved registers: RBX(3), R12(12), R13(13), R14(14), R15(15)
	regs := [3, 12, 13, 14, 15]

	for i in sorted {
		// Skip alloca results - they must stay on stack to preserve addresses
		if alloca_vals[i.val_id] {
			continue
		}
		for j := 0; j < active.len; j++ {
			if active[j].end < i.start {
				active.delete(j)
				j--
			}
		}
		if active.len < regs.len {
			mut used := []bool{len: 16, init: false}
			for a in active {
				used[g.reg_map[a.val_id]] = true
			}
			for r in regs {
				if !used[r] {
					g.reg_map[i.val_id] = r
					active << i
					if r !in g.used_regs {
						g.used_regs << r
					}
					break
				}
			}
		}
	}
	g.used_regs.sort()
}

fn (g Gen) type_align(typ_id ssa.TypeID) int {
	if typ_id > 0 && typ_id < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[typ_id]
		if typ.kind == .array_t {
			return g.type_align(typ.elem_type)
		}
	}
	size := g.type_size(typ_id)
	if size >= 8 {
		return 8
	}
	if size >= 4 {
		return 4
	}
	if size >= 2 {
		return 2
	}
	return 1
}

fn (g Gen) struct_field_offset_bytes(struct_typ_id ssa.TypeID, field_idx int) int {
	if struct_typ_id <= 0 || struct_typ_id >= g.mod.type_store.types.len {
		return field_idx * 8
	}
	typ := g.mod.type_store.types[struct_typ_id]
	if typ.kind != .struct_t || field_idx < 0 || field_idx >= typ.fields.len {
		return field_idx * 8
	}
	mut offset := 0
	for i, field_typ in typ.fields {
		align := g.type_align(field_typ)
		if align > 1 && offset % align != 0 {
			offset = (offset + align - 1) & ~(align - 1)
		}
		if i == field_idx {
			return offset
		}
		field_size := g.type_size(field_typ)
		offset += if field_size > 0 { field_size } else { 8 }
	}
	return field_idx * 8
}

fn (g &Gen) large_struct_stack_value_is_pointer(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.typ <= 0 || val.typ >= g.mod.type_store.types.len {
		return false
	}
	val_typ := g.mod.type_store.types[val.typ]
	if val_typ.kind != .struct_t || g.type_size(val.typ) <= 16 {
		return false
	}
	if val.kind == .string_literal {
		return false
	}
	if val.kind == .instruction {
		instr := g.mod.instrs[val.index]
		op := g.selected_opcode(instr)
		return op !in [.call, .call_sret, .inline_string_init, .insertvalue, .struct_init,
			.extractvalue, .assign, .phi, .bitcast, .load]
	}
	return false
}

fn (g &Gen) large_aggregate_stack_value_is_pointer(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.typ <= 0 || val.typ >= g.mod.type_store.types.len {
		return false
	}
	val_typ := g.mod.type_store.types[val.typ]
	if g.type_size(val.typ) <= 16 {
		return false
	}
	if val_typ.kind == .struct_t {
		return g.large_struct_stack_value_is_pointer(val_id)
	}
	if val_typ.kind == .array_t {
		if val.kind == .instruction {
			instr := g.mod.instrs[val.index]
			op := g.selected_opcode(instr)
			return op !in [.call, .call_sret, .insertvalue, .extractvalue, .assign, .phi, .bitcast,
				.load]
		}
	}
	return false
}
