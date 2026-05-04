// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import v2.mir
import v2.ssa
import encoding.binary
import math.bits

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
		if func.is_c_extern {
			continue
		}
		if func.blocks.len == 0 {
			continue
		}
		g.gen_func(func)
	}

	// Generate Globals in .data
	for gvar in g.mod.globals {
		if gvar.linkage == .external {
			continue
		}
		for g.elf.data_data.len % 8 != 0 {
			g.elf.data_data << 0
		}
		addr := u64(g.elf.data_data.len)
		g.elf.add_symbol(gvar.name, addr, false, 2)
		if gvar.initial_data.len > 0 {
			g.elf.data_data << gvar.initial_data
		} else if gvar.is_constant {
			size := g.type_size(gvar.typ)
			mut bytes := []u8{len: if size > 0 { size } else { 8 }}
			if bytes.len >= 8 {
				binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
			} else {
				for i := 0; i < bytes.len; i++ {
					bytes[i] = u8(u64(gvar.initial_value) >> (i * 8))
				}
			}
			g.elf.data_data << bytes
		} else {
			// For regular globals, initialize with zeros
			size := g.type_size(gvar.typ)
			data_size := if size > 0 { size } else { 8 }
			for _ in 0 .. data_size {
				g.elf.data_data << 0
			}
		}
	}
}

fn (mut g Gen) gen_func(func mir.Function) {
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

	// Start after callee-saved pushes so locals do not overlap their rbp slots.
	mut slot_offset := g.used_regs.len * 8

	// Hidden sret pointer slot (SysV: incoming in RDI)
	if func.abi_ret_indirect {
		off, next_offset := reserve_stack_bytes(slot_offset, 8, 1)
		g.sret_save_offset = off
		slot_offset = next_offset
	}

	for pi, pid in func.params {
		param_typ := g.mod.values[pid].typ
		param_size := g.type_size(param_typ)
		is_indirect_param := pi < func.abi_param_class.len && func.abi_param_class[pi] == .indirect
		if (is_indirect_param || g.value_is_aggregate(pid) || param_size > 8) && param_size > 0 {
			off, next_offset := reserve_stack_bytes(slot_offset, param_size, 16)
			g.stack_map[pid] = off
			slot_offset = next_offset
		} else {
			off, next_offset := reserve_stack_bytes(slot_offset, 8, 1)
			g.stack_map[pid] = off
			slot_offset = next_offset
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
				alloc_size := g.alloc_size_from_uses(val_id, g.type_size(ptr_type.elem_type))

				off, next_offset := reserve_stack_bytes(slot_offset, alloc_size, 16)
				g.alloca_offsets[val_id] = off
				slot_offset = next_offset
			}

			if g.value_needs_stack_storage(val_id) {
				result_size := g.stack_storage_size(val_id)
				off, next_offset := reserve_stack_bytes(slot_offset, result_size, 16)
				g.stack_map[val_id] = off
				slot_offset = next_offset
				continue
			}

			for operand in instr.operands {
				if operand > 0 && operand < g.mod.values.len && g.value_needs_stack_storage(operand)
					&& operand !in g.stack_map {
					lit_size := g.stack_storage_size(operand)
					off, next_offset := reserve_stack_bytes(slot_offset, lit_size, 16)
					g.stack_map[operand] = off
					slot_offset = next_offset
				}
			}

			if val_id in g.reg_map {
				continue
			}
			off, next_offset := reserve_stack_bytes(slot_offset, 8, 1)
			g.stack_map[val_id] = off
			slot_offset = next_offset
		}
	}

	g.stack_size = (slot_offset + 16) & ~0xF
	if g.used_regs.len % 2 == 1 {
		g.stack_size += 8
	}

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
	mut reg_arg_idx := arg_reg_base
	mut sse_arg_idx := 0
	if func.abi_ret_indirect && g.sret_save_offset != 0 {
		asm_store_rbp_disp_reg(mut g, g.sret_save_offset, rdi)
	}
	mut stack_param_offset := 16
	for i, pid in func.params {
		is_indirect_param := i < func.abi_param_class.len && func.abi_param_class[i] == .indirect
		param_size := g.type_size(g.mod.values[pid].typ)
		if g.value_is_float_type(pid) {
			g.ensure_float_abi_scalar(pid, 'parameter')
			if sse_arg_idx >= 8 {
				g.unsupported_float_abi('stack parameter', pid)
			}
			asm_store_xmm_rbp_disp(mut g, sse_arg_idx, g.stack_map[pid], param_size)
			sse_arg_idx++
			continue
		}
		param_chunks := if !is_indirect_param && g.value_is_aggregate(pid) && param_size > 8
			&& param_size <= 16 {
			(param_size + 7) / 8
		} else {
			1
		}
		if reg_arg_idx + param_chunks <= 6 {
			src := abi_regs[reg_arg_idx]
			if is_indirect_param {
				g.copy_indirect_param_from_reg(pid, src)
			} else if param_chunks > 1 {
				offset := g.stack_map[pid]
				for chunk := 0; chunk < param_chunks; chunk++ {
					chunk_size := if chunk == param_chunks - 1 {
						param_size - chunk * 8
					} else {
						8
					}
					g.store_reg_to_rbp_exact(Reg(abi_regs[reg_arg_idx + chunk]),
						offset + chunk * 8, chunk_size)
				}
			} else if g.value_needs_raw_abi_reg_bytes(pid, param_size) {
				g.store_reg_to_rbp_exact(Reg(src), g.stack_map[pid], param_size)
			} else if reg := g.reg_map[pid] {
				asm_mov_reg_reg(mut g, Reg(reg), Reg(src))
			} else {
				offset := g.stack_map[pid]
				asm_store_rbp_disp_reg(mut g, offset, Reg(src))
			}
			reg_arg_idx += param_chunks
		} else {
			// Stack parameters start at [rbp+16].
			if is_indirect_param {
				// Load pointer from stack into RAX, then copy through it.
				asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
				g.copy_indirect_param_from_reg(pid, int(rax))
			} else if param_chunks > 1 {
				g.copy_memory(int(rbp), g.stack_map[pid], int(rbp), stack_param_offset, param_size)
			} else if g.value_needs_raw_abi_reg_bytes(pid, param_size) {
				asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
				g.store_reg_to_rbp_exact(rax, g.stack_map[pid], param_size)
			} else if reg := g.reg_map[pid] {
				asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
				asm_mov_reg_reg(mut g, Reg(reg), rax)
			} else {
				asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
				offset := g.stack_map[pid]
				asm_store_rbp_disp_reg(mut g, offset, rax)
			}
			stack_param_offset += g.param_stack_slots(is_indirect_param, param_chunks, param_size) * 8
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

// slot_offset is the number of bytes already reserved below rbp.
fn reserve_stack_bytes(slot_offset int, size int, align int) (int, int) {
	mut next_offset := slot_offset
	if align > 1 && slot_offset % align != 0 {
		next_offset = ((slot_offset + align - 1) / align) * align
	}
	alloc_size := if size > 0 { size } else { 8 }
	next_offset += alloc_size
	return -next_offset, next_offset
}

fn (mut g Gen) gen_instr(val_id int) {
	instr := g.mod.instrs[g.mod.values[val_id].index]
	op := g.selected_opcode(instr)

	// Temps: 0=RAX, 1=RCX

	match op {
		.add, .sub, .mul, .sdiv, .udiv, .srem, .urem, .and_, .or_, .xor, .shl, .ashr, .lshr, .eq,
		.ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
			if op in [.eq, .ne, .lt, .gt, .le, .ge] && g.value_is_float_type(instr.operands[0]) {
				g.emit_float_compare(op, instr.operands[0], instr.operands[1], val_id)
				return
			}
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
				.udiv {
					asm_xor_edx_edx(mut g)
					asm_div_rcx(mut g)
				}
				.srem {
					asm_cqo(mut g)
					asm_idiv_rcx(mut g)
					asm_mov_rax_rdx(mut g)
				}
				.urem {
					asm_xor_edx_edx(mut g)
					asm_div_rcx(mut g)
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
				.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
					asm_cmp_rax_rcx(mut g)
					cc := match op {
						.eq { cc_e }
						.ne { cc_ne }
						.lt { cc_l }
						.gt { cc_g }
						.le { cc_le }
						.ge { cc_ge }
						.ult { cc_b }
						.ugt { cc_a }
						.ule { cc_be }
						.uge { cc_ae }
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
			if src_type_info.kind in [.struct_t, .array_t] || src_size > 8 {
				g.load_struct_src_address_to_reg(int(r10), src_id, src_typ)
				g.load_val_to_reg(int(r11), instr.operands[1])
				g.copy_memory(int(r11), 0, int(r10), 0, src_size)
			} else {
				g.load_val_to_reg(0, src_id) // Val -> RAX
				g.load_val_to_reg(1, instr.operands[1]) // Ptr -> RCX
				asm_store_mem_base_disp_reg_size(mut g, rcx, 0, rax, src_size)
			}
		}
		.load {
			g.load_val_to_reg(1, instr.operands[0]) // Ptr -> RCX
			load_size := g.type_size(instr.typ)
			load_type_info := g.mod.type_store.types[instr.typ]
			if load_type_info.kind in [.struct_t, .array_t] || load_size > 8 {
				g.copy_memory(int(rbp), g.stack_map[val_id], int(rcx), 0, load_size)
			} else {
				g.load_typed_mem_to_reg(rax, rcx, 0, instr.typ, load_size)
				g.store_reg_to_val(0, val_id)
			}
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
		.heap_alloc {
			alloc_size := g.heap_alloc_size(val_id)
			asm_mov_reg_imm32(mut g, rdi, 1)
			asm_mov_reg_imm64(mut g, rsi, u64(alloc_size))
			asm_xor_eax_eax(mut g)
			asm_call_rel32(mut g)
			sym_idx := g.elf.add_undefined('calloc')
			g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 4, -4)
			g.emit_u32(0)
			g.store_reg_to_val(0, val_id)
		}
		.get_element_ptr {
			g.load_val_to_reg(0, instr.operands[0]) // Base -> RAX
			offset := g.gep_const_offset(instr.operands[0], instr.operands[1])
			if offset >= 0 {
				if offset > 0 {
					asm_mov_reg_imm64(mut g, rcx, u64(offset))
					asm_add_rax_rcx(mut g)
				}
			} else {
				g.load_val_to_reg(1, instr.operands[1]) // Index -> RCX
				elem_size := g.gep_elem_size(instr.operands[0])
				if elem_size == 8 {
					asm_shl_rcx_3(mut g)
				} else if elem_size > 1 {
					asm_mov_reg_reg(mut g, rax, rcx)
					asm_mov_reg_imm64(mut g, rcx, u64(elem_size))
					asm_imul_rax_rcx(mut g)
					asm_mov_reg_reg(mut g, rcx, rax)
					g.load_val_to_reg(0, instr.operands[0])
				}
				asm_add_rax_rcx(mut g)
			}
			g.store_reg_to_val(0, val_id)
		}
		.call {
			abi_regs := [7, 6, 2, 1, 8, 9]
			num_args := instr.operands.len - 1
			stack_args := g.call_stack_arg_mask(instr, abi_regs.len)
			stack_slots := g.call_stack_slots(instr, stack_args)

			if stack_slots > 0 {
				if stack_slots % 2 == 1 {
					asm_push(mut g, rax)
				}
				for arg_idx := num_args - 1; arg_idx >= 0; arg_idx-- {
					if stack_args[arg_idx] {
						g.push_call_stack_arg(instr.operands[arg_idx + 1], arg_idx, instr)
					}
				}
			}

			// Stack arguments were pushed above; this pass only loads register arguments.
			sse_arg_idx := g.load_call_register_args(instr, abi_regs, stack_args)
			fn_val := g.mod.values[instr.operands[0]]

			// AL carries the number of SSE argument registers for variadic calls.
			g.emit_sse_arg_count(sse_arg_idx)

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
			if stack_slots > 0 {
				cleanup := (stack_slots + (stack_slots % 2)) * 8
				if cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(cleanup))
				}
			}

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_call_result(val_id)
			}
		}
		.call_sret {
			// SysV x86_64: hidden sret pointer in RDI, then user args in RSI,RDX,RCX,R8,R9.
			abi_regs := [6, 2, 1, 8, 9]
			num_args := instr.operands.len - 1
			stack_args := g.call_stack_arg_mask(instr, abi_regs.len)
			stack_slots := g.call_stack_slots(instr, stack_args)

			// Load destination pointer to RDI.
			g.load_address_of_val_to_reg(7, val_id)

			if stack_slots > 0 {
				if stack_slots % 2 == 1 {
					asm_push(mut g, rax)
				}
				for arg_idx := num_args - 1; arg_idx >= 0; arg_idx-- {
					if stack_args[arg_idx] {
						g.push_call_stack_arg(instr.operands[arg_idx + 1], arg_idx, instr)
					}
				}
			}

			// Stack arguments were pushed above; this pass only loads register arguments.
			sse_arg_idx := g.load_call_register_args(instr, abi_regs, stack_args)

			fn_val := g.mod.values[instr.operands[0]]

			// AL carries the number of SSE argument registers for variadic calls.
			g.emit_sse_arg_count(sse_arg_idx)

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
			if stack_slots > 0 {
				cleanup := (stack_slots + (stack_slots % 2)) * 8
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
			stack_args := g.call_stack_arg_mask(instr, abi_regs.len)
			stack_slots := g.call_stack_slots(instr, stack_args)

			if stack_slots > 0 {
				if stack_slots % 2 == 1 {
					asm_push(mut g, rax)
				}
				for arg_idx := num_args - 1; arg_idx >= 0; arg_idx-- {
					if stack_args[arg_idx] {
						g.push_call_stack_arg(instr.operands[arg_idx + 1], arg_idx, instr)
					}
				}
			}

			// Stack arguments were pushed above; this pass only loads register arguments.
			sse_arg_idx := g.load_call_register_args(instr, abi_regs, stack_args)

			// Load function pointer to r10 (caller-saved, not used for args)
			g.load_val_to_reg(10, instr.operands[0])

			// AL carries the number of SSE argument registers for variadic calls.
			g.emit_sse_arg_count(sse_arg_idx)

			// call *r10
			asm_call_r10(mut g)

			// Clean up stack arguments
			if stack_slots > 0 {
				cleanup := (stack_slots + (stack_slots % 2)) * 8
				if cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(cleanup))
				}
			}

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_call_result(val_id)
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
						g.copy_memory(int(rdi), 0, int(r10), 0, ret_size)
					}
				}
				// SysV returns sret pointer in RAX.
				asm_mov_reg_reg(mut g, rax, rdi)
			} else if instr.operands.len > 0 {
				ret_val_id := instr.operands[0]
				if g.value_is_float_type(ret_val_id) {
					g.ensure_float_abi_scalar(ret_val_id, 'return')
					g.load_float_val_to_xmm(0, ret_val_id,
						g.type_size(g.mod.values[ret_val_id].typ))
				} else {
					g.load_val_to_reg(0, ret_val_id)
				}
			}
			g.emit_epilogue()
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
			dest_size := g.type_size(g.mod.values[dest_id].typ)
			if g.value_is_aggregate(dest_id) || dest_size > 8 {
				g.copy_value_bytes(dest_id, src_id, dest_size)
			} else {
				g.load_val_to_reg(0, src_id)
				g.store_reg_to_val(0, dest_id)
			}
		}
		.bitcast, .trunc, .zext, .sext {
			if instr.operands.len > 0 {
				src_typ := g.mod.values[instr.operands[0]].typ
				dst_typ := instr.typ
				src_info := g.mod.type_store.types[src_typ]
				dst_info := g.mod.type_store.types[dst_typ]
				src_size := g.type_size(src_typ)
				dst_size := g.type_size(dst_typ)
				if op in [.trunc, .zext] && src_info.kind == .float_t && dst_info.kind == .float_t {
					g.load_float_val_to_xmm(0, instr.operands[0], src_size)
					if op == .trunc && src_size == 8 && dst_size == 4 {
						asm_cvtsd2ss_xmm0_xmm0(mut g)
					} else if op == .zext && src_size == 4 && dst_size == 8 {
						asm_cvtss2sd_xmm0_xmm0(mut g)
					} else {
						g.unsupported_numeric_conversion(op, src_size, dst_size, val_id)
					}
					asm_store_xmm0_rbp_disp(mut g, g.stack_map[val_id], dst_size)
				} else if op in [.trunc, .zext, .sext] && src_info.kind == .int_t
					&& dst_info.kind == .int_t {
					g.load_val_to_reg(0, instr.operands[0])
					if op == .trunc {
						g.mask_rax_to_size(dst_size, op, val_id)
					} else if op == .zext {
						g.mask_rax_to_size(src_size, op, val_id)
					} else if op == .sext {
						if src_size == 1 {
							asm_movsx_rax_al(mut g)
						} else if src_size == 2 {
							asm_movsx_rax_ax(mut g)
						} else if src_size == 4 {
							asm_movsxd_rax_eax(mut g)
						} else if src_size != 8 {
							g.unsupported_numeric_conversion(op, src_size, dst_size, val_id)
						}
					}
					g.store_reg_to_val(0, val_id)
				} else if op == .bitcast {
					g.load_val_to_reg(0, instr.operands[0])
					g.store_reg_to_val(0, val_id)
				} else {
					g.unsupported_numeric_conversion(op, src_size, dst_size, val_id)
				}
			}
		}
		.sitofp, .uitofp {
			if instr.operands.len > 0 {
				g.load_val_to_reg(0, instr.operands[0])
				src_size := g.type_size(g.mod.values[instr.operands[0]].typ)
				if op == .uitofp {
					g.emit_unsigned_int_to_float(src_size, g.type_size(instr.typ), val_id)
				} else {
					g.emit_signed_int_to_float(g.type_size(instr.typ), op, val_id)
				}
				asm_store_xmm0_rbp_disp(mut g, g.stack_map[val_id], g.type_size(instr.typ))
			}
		}
		.fptosi, .fptoui {
			if instr.operands.len > 0 {
				src_size := g.type_size(g.mod.values[instr.operands[0]].typ)
				dst_size := g.type_size(instr.typ)
				g.load_float_val_to_xmm(0, instr.operands[0], src_size)
				if op == .fptoui {
					g.emit_float_to_unsigned_int(src_size, dst_size, val_id)
				} else {
					g.emit_float_to_signed_int(src_size, op, val_id)
				}
				g.store_reg_to_val(0, val_id)
			}
		}
		.fadd, .fsub, .fmul, .fdiv {
			result_size := g.type_size(instr.typ)
			g.load_float_val_to_xmm(0, instr.operands[0], result_size)
			g.load_float_val_to_xmm(1, instr.operands[1], result_size)
			opcode := match op {
				.fadd { u8(0x58) }
				.fsub { u8(0x5C) }
				.fmul { u8(0x59) }
				.fdiv { u8(0x5E) }
				else { u8(0x58) }
			}

			asm_float_binop_xmm0_xmm1(mut g, opcode, result_size)
			asm_store_xmm0_rbp_disp(mut g, g.stack_map[val_id], result_size)
		}
		.inline_string_init {
			g.zero_value_bytes(val_id, g.stack_storage_size(val_id))
			for fi, field_id in instr.operands {
				g.store_field_value(val_id, instr.typ, fi, field_id,
					g.type_size(g.mod.values[field_id].typ))
			}
		}
		.struct_init {
			g.zero_value_bytes(val_id, g.type_size(instr.typ))
			for fi, field_id in instr.operands {
				field_typ := g.struct_field_type(instr.typ, fi, g.mod.values[field_id].typ)
				g.store_field_value(val_id, instr.typ, fi, field_id, g.type_size(field_typ))
			}
		}
		.insertvalue {
			tuple_id := instr.operands[0]
			elem_id := instr.operands[1]
			idx := g.const_int_operand(instr.operands[2])
			total_size := g.type_size(instr.typ)
			if !(g.mod.values[tuple_id].kind == .constant && g.mod.values[tuple_id].name == 'undef') {
				g.copy_value_bytes(val_id, tuple_id, total_size)
			} else {
				g.zero_value_bytes(val_id, total_size)
			}
			elem_typ := g.struct_field_type(instr.typ, idx, g.mod.values[elem_id].typ)
			g.store_field_value(val_id, instr.typ, idx, elem_id, g.type_size(elem_typ))
		}
		.extractvalue {
			tuple_id := instr.operands[0]
			idx := g.const_int_operand(instr.operands[1])
			field_off := g.struct_field_offset_bytes(g.mod.values[tuple_id].typ, idx)
			result_size := g.type_size(instr.typ)
			g.load_struct_src_address_to_reg(int(r10), tuple_id, g.mod.values[tuple_id].typ)
			if result_size > 8 || g.value_is_aggregate(val_id) {
				g.copy_memory(int(rbp), g.stack_map[val_id], int(r10), field_off, result_size)
			} else {
				g.load_typed_mem_to_reg(rax, r10, field_off, instr.typ, result_size)
				g.store_reg_to_val(0, val_id)
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
			eprintln('x64: unsupported op ${op} (${instr.selected_op}) in value ${val_id}')
			exit(1)
		}
	}
}

fn (mut g Gen) mask_rax_to_size(size int, op ssa.OpCode, val_id int) {
	match size {
		1 {
			asm_mov_reg_imm64(mut g, rcx, 0xff)
			asm_and_rax_rcx(mut g)
		}
		2 {
			asm_mov_reg_imm64(mut g, rcx, 0xffff)
			asm_and_rax_rcx(mut g)
		}
		4 {
			asm_mov_reg_imm64(mut g, rcx, 0xffffffff)
			asm_and_rax_rcx(mut g)
		}
		8 {}
		else {
			g.unsupported_numeric_conversion(op, size, size, val_id)
		}
	}
}

fn (mut g Gen) emit_float_compare(op ssa.OpCode, lhs int, rhs int, val_id int) {
	lhs_size := g.type_size(g.mod.values[lhs].typ)
	rhs_size := g.type_size(g.mod.values[rhs].typ)
	if lhs_size !in [4, 8] || rhs_size !in [4, 8] || !g.value_is_float_type(rhs) {
		g.unsupported_numeric_conversion(op, lhs_size, rhs_size, val_id)
	}
	g.load_float_val_to_xmm(0, lhs, lhs_size)
	g.load_float_val_to_xmm(1, rhs, rhs_size)
	size := if lhs_size == 8 || rhs_size == 8 { 8 } else { 4 }
	if lhs_size == 4 && size == 8 {
		asm_cvtss2sd_xmm0_xmm0(mut g)
	}
	if rhs_size == 4 && size == 8 {
		asm_cvtss2sd_xmm1_xmm1(mut g)
	}
	asm_ucomis_xmm0_xmm1(mut g, size)
	match op {
		.eq {
			asm_setcc_al_movzx(mut g, cc_e)
			asm_setcc_cl_movzx(mut g, cc_np)
			asm_and_rax_rcx(mut g)
		}
		.ne {
			asm_setcc_al_movzx(mut g, cc_ne)
			asm_setcc_cl_movzx(mut g, cc_p)
			asm_or_rax_rcx(mut g)
		}
		.lt {
			asm_setcc_al_movzx(mut g, cc_b)
			asm_setcc_cl_movzx(mut g, cc_np)
			asm_and_rax_rcx(mut g)
		}
		.gt {
			asm_setcc_al_movzx(mut g, cc_a)
		}
		.le {
			asm_setcc_al_movzx(mut g, cc_be)
			asm_setcc_cl_movzx(mut g, cc_np)
			asm_and_rax_rcx(mut g)
		}
		.ge {
			asm_setcc_al_movzx(mut g, cc_ae)
		}
		else {
			g.unsupported_numeric_conversion(op, size, size, val_id)
		}
	}

	g.store_reg_to_val(0, val_id)
}

fn (mut g Gen) emit_signed_int_to_float(result_size int, op ssa.OpCode, val_id int) {
	if result_size == 4 {
		asm_cvtsi2ss_xmm0_rax(mut g)
	} else if result_size == 8 {
		asm_cvtsi2sd_xmm0_rax(mut g)
	} else {
		g.unsupported_numeric_conversion(op, 8, result_size, val_id)
	}
}

fn (mut g Gen) emit_unsigned_int_to_float(src_size int, result_size int, val_id int) {
	if src_size < 8 {
		g.mask_rax_to_size(src_size, .uitofp, val_id)
		g.emit_signed_int_to_float(result_size, .uitofp, val_id)
		return
	}
	if src_size != 8 {
		g.unsupported_numeric_conversion(.uitofp, src_size, result_size, val_id)
	}
	asm_test_rax_rax(mut g)
	asm_jns_rel32(mut g)
	normal_patch := g.elf.text_data.len
	g.emit_u32(0)
	asm_mov_reg_reg(mut g, rcx, rax)
	asm_and_rcx_imm8(mut g, 1)
	asm_shr_rax_1(mut g)
	asm_or_rax_rcx(mut g)
	g.emit_signed_int_to_float(result_size, .uitofp, val_id)
	asm_add_float_xmm0_xmm0(mut g, result_size)
	asm_jmp_rel32(mut g)
	end_patch := g.elf.text_data.len
	g.emit_u32(0)
	g.patch_rel32(normal_patch)
	g.emit_signed_int_to_float(result_size, .uitofp, val_id)
	g.patch_rel32(end_patch)
}

fn (mut g Gen) emit_float_to_signed_int(src_size int, op ssa.OpCode, val_id int) {
	if src_size == 4 {
		asm_cvttss2si_rax_xmm0(mut g)
	} else if src_size == 8 {
		asm_cvttsd2si_rax_xmm0(mut g)
	} else {
		g.unsupported_numeric_conversion(op, src_size, 8, val_id)
	}
}

fn (mut g Gen) emit_float_to_unsigned_int(src_size int, dst_size int, val_id int) {
	if dst_size != 8 {
		g.emit_float_to_signed_int(src_size, .fptoui, val_id)
		g.mask_rax_to_size(dst_size, .fptoui, val_id)
		return
	}
	g.load_fp_2p63_to_xmm1(src_size, val_id)
	asm_ucomis_xmm0_xmm1(mut g, src_size)
	asm_jae_rel32(mut g)
	big_patch := g.elf.text_data.len
	g.emit_u32(0)
	g.emit_float_to_signed_int(src_size, .fptoui, val_id)
	asm_jmp_rel32(mut g)
	end_patch := g.elf.text_data.len
	g.emit_u32(0)
	g.patch_rel32(big_patch)
	asm_sub_float_xmm0_xmm1(mut g, src_size)
	g.emit_float_to_signed_int(src_size, .fptoui, val_id)
	asm_mov_reg_imm64(mut g, rcx, 0x8000000000000000)
	asm_or_rax_rcx(mut g)
	g.patch_rel32(end_patch)
}

fn (mut g Gen) load_fp_2p63_to_xmm1(size int, val_id int) {
	mut bytes := []u8{len: size}
	if size == 4 {
		binary.little_endian_put_u32(mut bytes, 0x5f000000)
	} else if size == 8 {
		binary.little_endian_put_u64(mut bytes, 0x43e0000000000000)
	} else {
		g.unsupported_numeric_conversion(.fptoui, size, 8, val_id)
	}
	str_offset := g.elf.rodata.len
	g.elf.rodata << bytes
	sym_name := 'L_fp_${g.curr_offset}_${str_offset}'
	sym_idx := g.elf.add_symbol(sym_name, u64(str_offset), false, 3)
	asm_lea_reg_rip(mut g, r10)
	g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
	g.emit_u32(0)
	asm_load_xmm_mem_base_disp_size(mut g, 1, r10, 0, size)
}

fn (mut g Gen) patch_rel32(patch_pos int) {
	target := g.elf.text_data.len
	rel := target - (patch_pos + 4)
	g.write_u32(patch_pos, u32(rel))
}

fn (g Gen) unsupported_numeric_conversion(op ssa.OpCode, src_size int, dst_size int, val_id int) {
	eprintln('x64: unsupported numeric conversion ${op} from ${src_size * 8}-bit to ${dst_size * 8}-bit in value ${val_id}')
	exit(1)
}

fn (g Gen) const_int_operand(val_id int) int {
	if val_id > 0 && val_id < g.mod.values.len {
		return int(g.mod.values[val_id].name.i64())
	}
	return 0
}

fn (g Gen) heap_alloc_size(val_id int) int {
	val := g.mod.values[val_id]
	mut min_size := 8
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		ptr_typ := g.mod.type_store.types[val.typ]
		if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0 {
			size := g.type_size(ptr_typ.elem_type)
			min_size = if size > 0 { size } else { 8 }
		}
	}
	return g.alloc_size_from_uses(val_id, min_size)
}

fn (g Gen) alloc_size_from_uses(ptr_id int, min_size int) int {
	mut size := if min_size > 0 { min_size } else { 8 }
	if ptr_id <= 0 || ptr_id >= g.mod.values.len {
		return size
	}
	for use_id in g.mod.values[ptr_id].uses {
		if use_id <= 0 || use_id >= g.mod.values.len || g.mod.values[use_id].kind != .instruction {
			continue
		}
		use_instr := g.mod.instrs[g.mod.values[use_id].index]
		if use_instr.op == .store && use_instr.operands.len >= 2 && use_instr.operands[1] == ptr_id {
			src_size := g.type_size(g.mod.values[use_instr.operands[0]].typ)
			if src_size > size {
				size = src_size
			}
		}
		if use_instr.op != .get_element_ptr || use_instr.operands.len < 2
			|| use_instr.operands[0] != ptr_id {
			continue
		}
		offset := g.gep_const_offset(ptr_id, use_instr.operands[1])
		if offset < 0 {
			continue
		}
		access_size := g.pointer_access_size(use_id)
		end := offset + if access_size > 0 { access_size } else { g.gep_elem_size(ptr_id) }
		if end > size {
			size = end
		}
	}
	return size
}

fn (g Gen) pointer_access_size(ptr_id int) int {
	if ptr_id <= 0 || ptr_id >= g.mod.values.len {
		return 0
	}
	mut size := 0
	for use_id in g.mod.values[ptr_id].uses {
		if use_id <= 0 || use_id >= g.mod.values.len || g.mod.values[use_id].kind != .instruction {
			continue
		}
		use_instr := g.mod.instrs[g.mod.values[use_id].index]
		if use_instr.op == .store && use_instr.operands.len >= 2 && use_instr.operands[1] == ptr_id {
			src_size := g.type_size(g.mod.values[use_instr.operands[0]].typ)
			if src_size > size {
				size = src_size
			}
		} else if use_instr.op == .load && use_instr.operands.len >= 1
			&& use_instr.operands[0] == ptr_id {
			load_size := g.type_size(use_instr.typ)
			if load_size > size {
				size = load_size
			}
		}
	}
	return size
}

fn (g Gen) struct_field_type(struct_typ_id int, field_idx int, fallback int) int {
	if struct_typ_id > 0 && struct_typ_id < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[struct_typ_id]
		if typ.kind == .struct_t && field_idx >= 0 && field_idx < typ.fields.len {
			return typ.fields[field_idx]
		}
		if typ.kind == .array_t {
			return typ.elem_type
		}
	}
	return fallback
}

fn (g Gen) struct_field_offset_bytes(struct_typ_id int, field_idx int) int {
	if struct_typ_id <= 0 || struct_typ_id >= g.mod.type_store.types.len {
		return field_idx * 8
	}
	typ := g.mod.type_store.types[struct_typ_id]
	if typ.kind == .array_t {
		return field_idx * g.type_size(typ.elem_type)
	}
	if typ.kind != .struct_t {
		return field_idx * 8
	}
	mut off := 0
	for i, field_typ in typ.fields {
		align := g.type_align(field_typ)
		if align > 1 && off % align != 0 {
			off = (off + align - 1) & ~(align - 1)
		}
		if i == field_idx {
			return off
		}
		off += g.type_size(field_typ)
	}
	return field_idx * 8
}

fn (g Gen) gep_const_offset(base_id int, idx_id int) int {
	if idx_id <= 0 || idx_id >= g.mod.values.len || g.mod.values[idx_id].kind != .constant {
		return -1
	}
	idx := g.const_int_operand(idx_id)
	if base_id <= 0 || base_id >= g.mod.values.len {
		return idx * 8
	}
	base_typ_id := g.mod.values[base_id].typ
	if base_typ_id <= 0 || base_typ_id >= g.mod.type_store.types.len {
		return idx * 8
	}
	base_typ := g.mod.type_store.types[base_typ_id]
	if base_typ.kind != .ptr_t {
		return idx * 8
	}
	elem_typ := g.mod.type_store.types[base_typ.elem_type]
	if elem_typ.kind == .struct_t {
		return g.struct_field_offset_bytes(base_typ.elem_type, idx)
	}
	return idx * g.type_size(base_typ.elem_type)
}

fn (g Gen) gep_elem_size(base_id int) int {
	if base_id > 0 && base_id < g.mod.values.len {
		base_typ_id := g.mod.values[base_id].typ
		if base_typ_id > 0 && base_typ_id < g.mod.type_store.types.len {
			base_typ := g.mod.type_store.types[base_typ_id]
			if base_typ.kind == .ptr_t {
				size := g.type_size(base_typ.elem_type)
				return if size > 0 { size } else { 8 }
			}
		}
	}
	return 8
}

fn (mut g Gen) zero_value_bytes(val_id int, size int) {
	if size <= 0 {
		return
	}
	dst_off := g.stack_map[val_id]
	asm_xor_reg_reg(mut g, rax)
	g.store_repeated_zero(int(rbp), dst_off, size)
}

fn (mut g Gen) store_repeated_zero(base int, off int, size int) {
	mut done := 0
	for done + 8 <= size {
		asm_store_mem_base_disp_reg_size(mut g, Reg(base), off + done, rax, 8)
		done += 8
	}
	for done < size {
		chunk := raw_memory_chunk_size(size - done)
		asm_store_mem_base_disp_reg_size(mut g, Reg(base), off + done, rax, chunk)
		done += chunk
	}
}

fn (mut g Gen) copy_value_bytes(dst_id int, src_id int, size int) {
	if size <= 0 {
		return
	}
	g.load_struct_src_address_to_reg(int(r10), src_id, g.mod.values[src_id].typ)
	g.copy_memory(int(rbp), g.stack_map[dst_id], int(r10), 0, size)
}

fn (mut g Gen) copy_memory(dst_base int, dst_off int, src_base int, src_off int, size int) {
	mut done := 0
	for done + 8 <= size {
		asm_load_reg_mem_base_disp_size(mut g, rax, Reg(src_base), src_off + done, 8)
		asm_store_mem_base_disp_reg_size(mut g, Reg(dst_base), dst_off + done, rax, 8)
		done += 8
	}
	for done < size {
		chunk := raw_memory_chunk_size(size - done)
		asm_load_reg_mem_base_disp_size(mut g, rax, Reg(src_base), src_off + done, chunk)
		asm_store_mem_base_disp_reg_size(mut g, Reg(dst_base), dst_off + done, rax, chunk)
		done += chunk
	}
}

fn raw_memory_chunk_size(size int) int {
	if size >= 4 {
		return 4
	}
	if size >= 2 {
		return 2
	}
	return 1
}

fn is_raw_abi_reg_size(size int) bool {
	return size !in [1, 2, 4, 8]
}

fn (mut g Gen) load_typed_mem_to_reg(reg Reg, base Reg, disp int, typ_id int, size int) {
	typ := g.mod.type_store.types[typ_id]
	if typ.kind == .int_t && !typ.is_unsigned {
		asm_load_reg_mem_base_disp_size_signed(mut g, reg, base, disp, size)
		return
	}
	asm_load_reg_mem_base_disp_size(mut g, reg, base, disp, size)
}

fn (mut g Gen) load_raw_mem_to_reg(reg Reg, base Reg, disp int, size int) {
	if size in [1, 2, 4, 8] {
		asm_load_reg_mem_base_disp_size(mut g, reg, base, disp, size)
		return
	}
	if size <= 0 || size > 8 {
		eprintln('x64: unsupported raw memory size ${size}')
		exit(1)
	}
	asm_xor_reg_reg(mut g, rax)
	mut done := 0
	for done < size {
		chunk := raw_memory_chunk_size(size - done)
		asm_load_reg_mem_base_disp_size(mut g, r11, base, disp + done, chunk)
		if done > 0 {
			asm_shl_r11_imm8(mut g, u8(done * 8))
		}
		asm_or_rax_r11(mut g)
		done += chunk
	}
	if reg != rax {
		asm_mov_reg_reg(mut g, reg, rax)
	}
}

fn (mut g Gen) store_reg_to_rbp_exact(reg Reg, off int, size int) {
	if size in [1, 2, 4, 8] {
		asm_store_rbp_disp_reg_size(mut g, off, reg, size)
		return
	}
	if size <= 0 || size > 8 {
		eprintln('x64: unsupported raw register spill size ${size}')
		exit(1)
	}
	if reg != rax {
		asm_mov_reg_reg(mut g, rax, reg)
	}
	mut done := 0
	for done < size {
		chunk := raw_memory_chunk_size(size - done)
		asm_store_rbp_disp_reg_size(mut g, off + done, rax, chunk)
		done += chunk
		if done < size {
			asm_shr_rax_imm8(mut g, u8(chunk * 8))
		}
	}
}

fn (mut g Gen) store_field_value(dst_id int, dst_typ int, field_idx int, src_id int, size int) {
	field_off := g.struct_field_offset_bytes(dst_typ, field_idx)
	dst_off := g.stack_map[dst_id] + field_off
	if size > 8 || g.value_is_aggregate(src_id) {
		if g.value_is_zero_constant(src_id) {
			asm_xor_reg_reg(mut g, rax)
			g.store_repeated_zero(int(rbp), dst_off, size)
			return
		}
		g.load_struct_src_address_to_reg(int(r10), src_id, g.mod.values[src_id].typ)
		g.copy_memory(int(rbp), dst_off, int(r10), 0, size)
		return
	}
	g.load_val_to_reg(0, src_id)
	asm_store_rbp_disp_reg_size(mut g, dst_off, rax, size)
}

fn (mut g Gen) emit_epilogue() {
	if g.stack_size > 0 {
		if g.stack_size <= 127 {
			asm_add_rsp_imm8(mut g, u8(g.stack_size))
		} else {
			asm_add_rsp_imm32(mut g, u32(g.stack_size))
		}
	}
	for i := g.used_regs.len - 1; i >= 0; i-- {
		asm_pop(mut g, Reg(g.used_regs[i]))
	}
	asm_pop_rbp(mut g)
	asm_ret(mut g)
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
	size := g.type_size(g.mod.values[val_id].typ)
	if g.value_needs_raw_abi_reg_bytes(val_id, size) {
		if g.value_is_zero_constant(val_id) {
			asm_xor_reg_reg(mut g, Reg(reg))
			return
		}
		g.load_struct_src_address_to_reg(int(r10), val_id, g.mod.values[val_id].typ)
		g.load_raw_mem_to_reg(Reg(reg), r10, 0, size)
		return
	}
	g.load_val_to_reg(reg, val_id)
}

fn (mut g Gen) load_call_register_args(instr mir.Instruction, abi_regs []int, stack_args []bool) int {
	mut reg_arg_idx := 0
	mut sse_arg_idx := 0
	for i in 1 .. instr.operands.len {
		arg_idx := i - 1
		arg_id := instr.operands[i]
		if g.value_is_float_type(arg_id) {
			g.load_float_call_arg_to_xmm(sse_arg_idx, arg_id)
			sse_arg_idx++
			continue
		}
		if stack_args[arg_idx] {
			continue
		}
		arg_chunks := g.call_arg_reg_chunks(arg_id, arg_idx, instr)
		if reg_arg_idx + arg_chunks <= abi_regs.len {
			if arg_chunks > 1 {
				g.load_aggregate_arg_to_regs(arg_id,
					abi_regs[reg_arg_idx..reg_arg_idx + arg_chunks],
					g.type_size(g.mod.values[arg_id].typ))
			} else {
				g.load_call_arg_to_reg(abi_regs[reg_arg_idx], arg_id, arg_idx, instr)
			}
			reg_arg_idx += arg_chunks
		}
	}
	return sse_arg_idx
}

fn (mut g Gen) load_float_call_arg_to_xmm(xmm int, val_id int) {
	g.ensure_float_abi_scalar(val_id, 'argument')
	if xmm >= 8 {
		g.unsupported_float_abi('stack argument', val_id)
	}
	g.load_float_val_to_xmm(xmm, val_id, g.type_size(g.mod.values[val_id].typ))
}

fn (mut g Gen) store_call_result(val_id int) {
	if g.value_is_float_type(val_id) {
		g.ensure_float_abi_scalar(val_id, 'call result')
		asm_store_xmm0_rbp_disp(mut g, g.stack_map[val_id], g.type_size(g.mod.values[val_id].typ))
		return
	}
	g.store_reg_to_val(0, val_id)
}

fn (mut g Gen) emit_sse_arg_count(count int) {
	if count == 0 {
		asm_xor_eax_eax(mut g)
	} else {
		asm_mov_reg_imm32(mut g, rax, u32(count))
	}
}

fn (g Gen) param_stack_slots(is_indirect bool, reg_chunks int, size int) int {
	if is_indirect {
		return 1
	}
	if reg_chunks > 1 {
		return reg_chunks
	}
	if size > 8 {
		return (size + 7) / 8
	}
	return 1
}

fn (g Gen) call_arg_reg_chunks(val_id int, arg_idx int, instr mir.Instruction) int {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	if is_indirect || !g.value_is_aggregate(val_id) {
		return 1
	}
	size := g.type_size(g.mod.values[val_id].typ)
	if size > 8 && size <= 16 {
		return (size + 7) / 8
	}
	return 1
}

fn (g Gen) call_arg_stack_slots(val_id int, arg_idx int, instr mir.Instruction) int {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	if is_indirect {
		return 1
	}
	size := g.type_size(g.mod.values[val_id].typ)
	if g.value_is_aggregate(val_id) || size > 8 {
		return (size + 7) / 8
	}
	return 1
}

fn (g Gen) call_stack_arg_mask(instr mir.Instruction, abi_reg_count int) []bool {
	num_args := instr.operands.len - 1
	mut stack_args := []bool{len: num_args}
	mut reg_arg_idx := 0
	mut sse_arg_idx := 0
	for arg_idx := 0; arg_idx < num_args; arg_idx++ {
		arg_id := instr.operands[arg_idx + 1]
		if g.value_is_float_type(arg_id) {
			g.ensure_float_abi_scalar(arg_id, 'argument')
			if sse_arg_idx >= 8 {
				g.unsupported_float_abi('stack argument', arg_id)
			}
			sse_arg_idx++
			continue
		}
		arg_chunks := g.call_arg_reg_chunks(arg_id, arg_idx, instr)
		if reg_arg_idx + arg_chunks <= abi_reg_count {
			reg_arg_idx += arg_chunks
		} else {
			stack_args[arg_idx] = true
		}
	}
	return stack_args
}

fn (g Gen) call_stack_slots(instr mir.Instruction, stack_args []bool) int {
	mut slots := 0
	for arg_idx, is_stack in stack_args {
		if is_stack {
			slots += g.call_arg_stack_slots(instr.operands[arg_idx + 1], arg_idx, instr)
		}
	}
	return slots
}

fn (mut g Gen) push_call_stack_arg(val_id int, arg_idx int, instr mir.Instruction) {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	size := g.type_size(g.mod.values[val_id].typ)
	slots := g.call_arg_stack_slots(val_id, arg_idx, instr)
	if !is_indirect && slots > 1 {
		g.load_struct_src_address_to_reg(int(r10), val_id, g.mod.values[val_id].typ)
		for chunk := slots - 1; chunk >= 0; chunk-- {
			chunk_size := if chunk == slots - 1 { size - chunk * 8 } else { 8 }
			g.load_raw_mem_to_reg(rax, r10, chunk * 8, chunk_size)
			asm_push(mut g, rax)
		}
		return
	}
	g.load_call_arg_to_reg(0, val_id, arg_idx, instr)
	asm_push(mut g, rax)
}

fn (mut g Gen) load_aggregate_arg_to_regs(val_id int, regs []int, size int) {
	g.load_struct_src_address_to_reg(int(r10), val_id, g.mod.values[val_id].typ)
	for chunk, reg in regs {
		chunk_size := if chunk == regs.len - 1 { size - chunk * 8 } else { 8 }
		g.load_raw_mem_to_reg(Reg(reg), r10, chunk * 8, chunk_size)
	}
}

fn (mut g Gen) load_struct_src_address_to_reg(reg int, val_id int, expected_struct_typ int) {
	val := g.mod.values[val_id]
	if val.kind == .string_literal {
		g.materialize_string_literal(reg, val_id)
		return
	}
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
	g.copy_memory(int(r11), 0, int(r10), 0, param_size)
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
	} else if val.kind == .string_literal {
		g.materialize_string_literal(reg, val_id)
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

fn (mut g Gen) materialize_string_literal(reg int, val_id int) {
	val := g.mod.values[val_id]
	str_offset := g.elf.rodata.len
	g.elf.rodata << val.name.bytes()
	g.elf.rodata << 0
	sym_name := 'L_str_${g.curr_offset}_${str_offset}'
	sym_idx := g.elf.add_symbol(sym_name, u64(str_offset), false, 3)

	slot_off := g.stack_map[val_id]
	asm_lea_reg_rip(mut g, rax)
	g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
	g.emit_u32(0)
	asm_store_rbp_disp_reg_size(mut g, slot_off + g.struct_field_offset_bytes(val.typ, 0), rax, 8)
	asm_mov_reg_imm32(mut g, rax, u32(val.index))
	asm_store_rbp_disp_reg_size(mut g, slot_off + g.struct_field_offset_bytes(val.typ, 1), rax, 4)
	asm_mov_reg_imm32(mut g, rax, 1)
	asm_store_rbp_disp_reg_size(mut g, slot_off + g.struct_field_offset_bytes(val.typ, 2), rax, 4)
	if slot_off >= -128 && slot_off <= 127 {
		asm_lea_rax_rbp_disp8(mut g, i8(slot_off))
	} else {
		asm_lea_rax_rbp_disp32(mut g, slot_off)
	}
	if reg != 0 {
		asm_mov_reg_reg(mut g, Reg(reg), rax)
	}
}

fn (mut g Gen) load_float_val_to_xmm(xmm int, val_id int, size int) {
	val := g.mod.values[val_id]
	if val.kind == .constant {
		if size == 4 {
			asm_mov_reg_imm32(mut g, rax, bits.f32_bits(val.name.f32()))
		} else {
			asm_mov_reg_imm64(mut g, rax, bits.f64_bits(val.name.f64()))
		}
		asm_store_rbp_disp_reg_size(mut g, g.stack_map[val_id], rax, size)
	}
	asm_load_xmm_rbp_disp(mut g, xmm, g.stack_map[val_id], size)
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
			mut max_align := 1
			for field_typ in typ.fields {
				align := g.type_align(field_typ)
				if align > max_align {
					max_align = align
				}
				if align > 1 && total % align != 0 {
					total = (total + align - 1) & ~(align - 1)
				}
				total += g.type_size(field_typ)
			}
			if max_align > 1 && total % max_align != 0 {
				total = (total + max_align - 1) & ~(max_align - 1)
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
	mut total_instrs := 0

	for blk_id in func.blocks {
		total_instrs += g.mod.blocks[blk_id].instrs.len
	}

	// Track which values are alloca results - don't register allocate these
	// as they hold addresses that may be needed across the function
	mut alloca_vals := map[int]bool{}

	for i, pid in func.params {
		if i < func.abi_param_class.len && func.abi_param_class[i] == .indirect {
			alloca_vals[pid] = true
			continue
		}
		if g.value_is_float_type(pid) {
			alloca_vals[pid] = true
			continue
		}
		intervals[pid] = &Interval{
			val_id: pid
			start:  0
			// ABI lowering can hide original parameter uses inside selected call sequences.
			// Keep incoming parameters live across the function to avoid reusing their
			// callee-saved register while later calls still need the parameter value.
			end: total_instrs
		}
	}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind == .instruction || val.kind == .argument {
				if unsafe { intervals[val_id] == nil } {
					if val.kind == .instruction && g.value_needs_stack_storage(val_id) {
						continue
					}
					intervals[val_id] = &Interval{
						val_id: val_id
						start:  instr_idx
						end:    instr_idx
					}
				}
			}
			instr := g.mod.instrs[val.index]
			// Mark alloca results as non-register-allocatable
			if instr.op in [.alloca, .call_sret] || g.value_needs_stack_storage(val_id) {
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

fn (g Gen) value_is_aggregate(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	typ_id := g.mod.values[val_id].typ
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return false
	}
	typ := g.mod.type_store.types[typ_id]
	return typ.kind in [.struct_t, .array_t]
}

fn (g Gen) value_is_float_type(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	typ_id := g.mod.values[val_id].typ
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return false
	}
	return g.mod.type_store.types[typ_id].kind == .float_t
}

fn (g Gen) ensure_float_abi_scalar(val_id int, context string) {
	size := g.type_size(g.mod.values[val_id].typ)
	if size == 4 || size == 8 {
		return
	}
	g.unsupported_float_abi(context, val_id)
}

fn (g Gen) unsupported_float_abi(context string, val_id int) {
	size := g.type_size(g.mod.values[val_id].typ)
	eprintln('x64: unsupported float ABI ${context} with ${size * 8}-bit type in value ${val_id}')
	exit(1)
}

fn (g Gen) value_is_zero_constant(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	return val.kind == .constant && val.name == '0'
}

fn (g Gen) value_needs_raw_abi_reg_bytes(val_id int, size int) bool {
	return size > 0 && size <= 8 && (g.value_is_aggregate(val_id) || is_raw_abi_reg_size(size))
}

fn (g Gen) stack_storage_size(val_id int) int {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return 8
	}
	val := g.mod.values[val_id]
	size := g.type_size(val.typ)
	if val.kind == .string_literal && size < 16 {
		return 16
	}
	return if size > 0 { size } else { 8 }
}

fn (g Gen) value_needs_stack_storage(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.kind == .string_literal {
		return true
	}
	if val.typ <= 0 || val.typ >= g.mod.type_store.types.len {
		return false
	}
	typ := g.mod.type_store.types[val.typ]
	if typ.kind == .float_t {
		return true
	}
	if typ.kind in [.struct_t, .array_t] || g.type_size(val.typ) > 8 {
		return true
	}
	if val.kind != .instruction {
		return false
	}
	instr := g.mod.instrs[val.index]
	return instr.op in [.call_sret, .inline_string_init, .struct_init, .insertvalue, .extractvalue]
}
