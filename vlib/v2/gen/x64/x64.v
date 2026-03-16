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

	// Type layout caches
	type_size_cache  []int
	type_align_cache []int

	// Cache for struct field offset calculations (key: typ_id << 16 | field_idx)
	struct_field_offset_cache map[int]int

	// Lookup caches for O(1) name resolution
	func_by_name   map[string]int
	global_by_name map[string]int
}

struct Interval {
mut:
	val_id   int
	start    int
	end      int
	has_call bool
}

pub fn Gen.new(mod &mir.Module) &Gen {
	n_types := mod.type_store.types.len
	return &Gen{
		mod:              mod
		elf:              ElfObject.new()
		type_size_cache:  []int{len: n_types}
		type_align_cache: []int{len: n_types}
	}
}

pub fn (mut g Gen) gen() {
	g.gen_pre_pass()
	for func in g.mod.funcs {
		g.gen_func(func)
	}
	g.gen_post_pass()
}

fn (mut g Gen) gen_pre_pass() {
	// Build lookup caches
	for fi, func in g.mod.funcs {
		g.func_by_name[func.name] = fi
	}
	for gi, gvar in g.mod.globals {
		g.global_by_name[gvar.name] = gi
	}
	// Pre-populate type size/align caches
	for tid in 0 .. g.mod.type_store.types.len {
		if g.type_size_cache[tid] == 0 {
			sz := g.type_size(tid)
			if sz > 0 {
				g.type_size_cache[tid] = sz
			}
		}
	}
}

fn (mut g Gen) gen_post_pass() {
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
			for b in gvar.initial_data {
				g.elf.data_data << b
			}
		} else if gvar.is_constant {
			mut bytes := []u8{len: 8}
			binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
			for b in bytes {
				g.elf.data_data << b
			}
		} else {
			size := g.type_size(gvar.typ)
			actual_size := if size > 0 { size } else { 8 }
			for _ in 0 .. actual_size {
				g.elf.data_data << 0
			}
		}
	}
}

fn (mut g Gen) gen_func(func mir.Function) {
	if func.is_c_extern {
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
	// Start local variables after callee-saved register save area.
	// Callee-saved regs are pushed after `mov rbp, rsp`, occupying
	// [rbp-8], [rbp-16], ... [rbp-N*8], so locals start at rbp-(N+1)*8.
	mut slot_offset := 8 + g.used_regs.len * 8

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
			// Advance past the indirect data base so the next regular 8-byte
			// slot (assigned at -slot_offset before +=8) doesn't overlap byte 0
			// of this indirect area.
			slot_offset += 8
		} else {
			g.stack_map[pid] = -slot_offset
			slot_offset += 8
		}
	}

	// Pre-pass: allocate stack slots for string_literal values used in this function
	mut seen_str_lits := map[int]bool{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			for op in instr.operands {
				op_val := g.mod.values[op]
				if op_val.kind == .string_literal && op !in seen_str_lits {
					seen_str_lits[op] = true
					str_size := g.type_size(op_val.typ)
					actual_size := if str_size > 0 { str_size } else { 24 }
					slot_offset = (slot_offset + 7) & ~0x7
					slot_offset += actual_size
					g.stack_map[op] = -slot_offset
				}
			}
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
				elem_size := g.type_size(ptr_type.elem_type)
				mut alloc_size := if elem_size > 0 { elem_size } else { 8 }
				// Check for array alloca: operand[0] is element count
				if instr.operands.len > 0 {
					count_val := g.mod.values[instr.operands[0]]
					count := count_val.name.int()
					if count > 1 {
						alloc_size = elem_size * count
					}
				}

				// Align to 16 bytes
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += alloc_size
				g.alloca_offsets[val_id] = -slot_offset
				slot_offset += 8 // Slot for the pointer
			}

			if instr.op == .inline_string_init {
				mut string_size := g.type_size(instr.typ)
				if string_size <= 0 {
					string_size = 24
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += string_size
				slot_offset += 8 // pointer slot
				g.stack_map[val_id] = -slot_offset
				continue
			}

			if instr.op == .insertvalue || instr.op == .struct_init {
				mut tuple_size := g.type_size(instr.typ)
				if tuple_size <= 0 {
					tuple_typ := g.mod.type_store.types[instr.typ]
					tuple_size = tuple_typ.fields.len * 8
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += tuple_size
				g.stack_map[val_id] = -slot_offset
				slot_offset += 8
				continue
			}

			if instr.op == .call_sret {
				result_typ := g.mod.type_store.types[val.typ]
				if result_typ.kind == .struct_t {
					result_size := g.type_size(val.typ)
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += result_size
					g.stack_map[val_id] = -slot_offset
					slot_offset += 8
					continue
				}
			}

			// Keep full stack storage for struct/array values
			val_typ := g.mod.type_store.types[val.typ]
			if val_typ.kind == .struct_t || val_typ.kind == .array_t {
				mut struct_size := g.type_size(val.typ)
				if struct_size <= 0 {
					struct_size = if val_typ.fields.len > 0 { val_typ.fields.len * 8 } else { 8 }
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += struct_size
				g.stack_map[val_id] = -slot_offset
				slot_offset += 8
				continue
			}

			if instr.op == .call {
				result_typ := g.mod.type_store.types[val.typ]
				if result_typ.kind == .struct_t && result_typ.fields.len > 1 {
					mut call_size := g.type_size(val.typ)
					if call_size <= 0 {
						call_size = 16
					}
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += call_size
					g.stack_map[val_id] = -slot_offset
					slot_offset += 8
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

	// The C entry point `main` receives argc in RDI and argv in RSI.
	// Persist them to builtin globals so `os.args` / `arguments()` work.
	// Must happen before param spilling overwrites RDI/RSI.
	if func.name == 'main' {
		g.store_entry_arg_to_global(int(rdi), 'g_main_argc', 4)
		g.store_entry_arg_to_global(int(rsi), 'g_main_argv', 8)
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
		.fadd, .fsub, .fmul, .fdiv, .frem {
			// Float operations using SSE2
			// Load LHS f64 bits to xmm0, RHS to xmm1
			g.load_val_to_reg(0, instr.operands[0]) // RAX = f64 bits
			asm_movq_xmm0_rax(mut g)
			g.load_val_to_reg(1, instr.operands[1]) // RCX = f64 bits
			asm_movq_xmm1_rcx(mut g)

			match op {
				.fadd { asm_addsd_xmm0_xmm1(mut g) }
				.fsub { asm_subsd_xmm0_xmm1(mut g) }
				.fmul { asm_mulsd_xmm0_xmm1(mut g) }
				.fdiv { asm_divsd_xmm0_xmm1(mut g) }
				.frem {
					// xmm0 = xmm0 - trunc(xmm0/xmm1) * xmm1
					asm_movsd_xmm2_xmm0(mut g)
					asm_divsd_xmm2_xmm1(mut g)
					asm_roundsd_xmm2_trunc(mut g)
					asm_mulsd_xmm2_xmm1(mut g)
					asm_subsd_xmm0_xmm2(mut g)
				}
				else {}
			}
			// Move result bits back to RAX
			asm_movq_rax_xmm0(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.fptosi {
			// Float to signed integer conversion
			g.load_val_to_reg(0, instr.operands[0])
			asm_movq_xmm0_rax(mut g)
			asm_cvttsd2si_rax_xmm0(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.fptoui {
			// Float to unsigned integer conversion (same as fptosi for most cases)
			g.load_val_to_reg(0, instr.operands[0])
			asm_movq_xmm0_rax(mut g)
			asm_cvttsd2si_rax_xmm0(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.sitofp {
			// Signed integer to float conversion
			g.load_val_to_reg(0, instr.operands[0])
			asm_cvtsi2sd_xmm0_rax(mut g)
			// Check if result is f32
			result_is_f32 := g.mod.values[val_id].typ > 0
				&& g.mod.values[val_id].typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				&& g.mod.type_store.types[g.mod.values[val_id].typ].width == 32
			if result_is_f32 {
				asm_cvtsd2ss_xmm0(mut g)
				asm_movd_eax_xmm0(mut g)
			} else {
				asm_movq_rax_xmm0(mut g)
			}
			g.store_reg_to_val(0, val_id)
		}
		.uitofp {
			// Unsigned integer to float conversion
			// For unsigned, we use the same cvtsi2sd but the value is already positive
			g.load_val_to_reg(0, instr.operands[0])
			asm_cvtsi2sd_xmm0_rax(mut g)
			result_is_f32 := g.mod.values[val_id].typ > 0
				&& g.mod.values[val_id].typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				&& g.mod.type_store.types[g.mod.values[val_id].typ].width == 32
			if result_is_f32 {
				asm_cvtsd2ss_xmm0(mut g)
				asm_movd_eax_xmm0(mut g)
			} else {
				asm_movq_rax_xmm0(mut g)
			}
			g.store_reg_to_val(0, val_id)
		}
		.add, .sub, .mul, .sdiv, .udiv, .srem, .urem, .and_, .or_, .xor, .shl, .ashr, .lshr,
		.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
			// Check if this is a float comparison
			lhs_typ := g.mod.values[instr.operands[0]].typ
			is_float_cmp := op in [.eq, .ne, .lt, .gt, .le, .ge] && lhs_typ > 0
				&& lhs_typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[lhs_typ].kind == .float_t

			if is_float_cmp {
				// Float comparison using SSE2
				g.load_val_to_reg(0, instr.operands[0])
				asm_movq_xmm0_rax(mut g)
				g.load_val_to_reg(1, instr.operands[1])
				asm_movq_xmm1_rcx(mut g)
				asm_ucomisd_xmm0_xmm1(mut g)
				// ucomisd sets ZF/PF/CF flags (unsigned-style)
				cc := match op {
					.eq { cc_e }
					.ne { cc_ne }
					.lt { cc_b } // below (CF=1)
					.gt { cc_a } // above (CF=0, ZF=0)
					.le { cc_be } // below or equal
					.ge { cc_ae } // above or equal
					else { cc_e }
				}
				asm_setcc_al_movzx(mut g, cc)
			} else {
				g.load_val_to_reg(0, instr.operands[0]) // RAX
				g.load_val_to_reg(1, instr.operands[1]) // RCX

				match op {
					.add { asm_add_rax_rcx(mut g) }
					.sub { asm_sub_rax_rcx(mut g) }
					.mul { asm_imul_rax_rcx(mut g) }
					.sdiv {
						asm_cqo(mut g)
						asm_idiv_rcx(mut g)
					}
					.udiv {
						asm_xor_rdx_rdx(mut g)
						asm_div_rcx(mut g)
					}
					.srem {
						asm_cqo(mut g)
						asm_idiv_rcx(mut g)
						asm_mov_rax_rdx(mut g)
					}
					.urem {
						asm_xor_rdx_rdx(mut g)
						asm_div_rcx(mut g)
						asm_mov_rax_rdx(mut g)
					}
					.and_ { asm_and_rax_rcx(mut g) }
					.or_ { asm_or_rax_rcx(mut g) }
					.xor { asm_xor_rax_rcx(mut g) }
					.shl { asm_shl_rax_cl(mut g) }
					.ashr { asm_sar_rax_cl(mut g) }
					.lshr { asm_shr_rax_cl(mut g) }
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
			}
			// Narrow integer canonicalization
			result_typ_id := g.mod.values[val_id].typ
			if result_typ_id > 0 && result_typ_id < g.mod.type_store.types.len {
				result_typ := g.mod.type_store.types[result_typ_id]
				if result_typ.kind == .int_t && result_typ.width > 0 && result_typ.width < 64 {
					g.canonicalize_narrow_int(result_typ)
				}
			}
			g.store_reg_to_val(0, val_id)
		}
		.store {
			src_id := instr.operands[0]
			ptr_id := instr.operands[1]
			src_typ := g.mod.values[src_id].typ
			src_type_info := g.mod.type_store.types[src_typ]
			src_size := g.type_size(src_typ)

			// Determine destination element type and size from pointer type
			mut dst_elem_size := 0
			mut dst_is_struct := false
			ptr_val := g.mod.values[ptr_id]
			if ptr_val.typ > 0 && ptr_val.typ < g.mod.type_store.types.len {
				ptr_typ := g.mod.type_store.types[ptr_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
					&& ptr_typ.elem_type < g.mod.type_store.types.len {
					elem_typ := g.mod.type_store.types[ptr_typ.elem_type]
					dst_elem_size = g.type_size(ptr_typ.elem_type)
					dst_is_struct = elem_typ.kind in [.struct_t, .array_t]
				}
			}

			if (src_type_info.kind in [.struct_t, .array_t] && src_size > 16)
				|| (dst_is_struct && dst_elem_size > 16) {
				// Large struct copy
				copy_size := if dst_elem_size > 16 { dst_elem_size } else { src_size }
				g.load_struct_src_address_to_reg(int(r10), src_id, src_typ)
				g.load_val_to_reg(int(r11), ptr_id)
				num_chunks := (copy_size + 7) / 8
				for i := 0; i < num_chunks; i++ {
					disp := i * 8
					asm_mov_rax_mem_base_disp(mut g, r10, disp)
					asm_mov_mem_base_disp_rax(mut g, r11, disp)
				}
			} else if dst_is_struct && dst_elem_size > 0 && dst_elem_size <= 16 {
				// Small struct copy (2 words or less)
				num_chunks := (dst_elem_size + 7) / 8
				src_has_storage := src_id in g.reg_map || src_id in g.stack_map
				if src_has_storage && num_chunks > 1 {
					g.load_address_of_val_to_reg(int(r10), src_id)
					g.load_val_to_reg(int(r11), ptr_id)
					for i := 0; i < num_chunks; i++ {
						disp := i * 8
						asm_mov_rax_mem_base_disp(mut g, r10, disp)
						asm_mov_mem_base_disp_rax(mut g, r11, disp)
					}
				} else {
					g.load_val_to_reg(0, src_id)
					g.load_val_to_reg(1, ptr_id)
					asm_mov_mem_rcx_rax(mut g)
				}
			} else {
				// Sized scalar store
				store_size := if dst_elem_size > 0 { dst_elem_size } else { src_size }
				g.load_val_to_reg(0, src_id) // Val -> RAX
				g.load_val_to_reg(1, ptr_id) // Ptr -> RCX
				match store_size {
					1 { asm_store_mem_rcx_al(mut g) }
					2 { asm_store_mem_rcx_ax(mut g) }
					4 { asm_store_mem_rcx_eax(mut g) }
					else { asm_mov_mem_rcx_rax(mut g) }
				}
			}
		}
		.load {
			ptr_id := instr.operands[0]
			// Determine load size from pointer element type
			mut load_size := 8
			mut is_signed := false
			ptr_val := g.mod.values[ptr_id]
			if ptr_val.typ > 0 && ptr_val.typ < g.mod.type_store.types.len {
				ptr_typ := g.mod.type_store.types[ptr_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
					&& ptr_typ.elem_type < g.mod.type_store.types.len {
					elem_typ := g.mod.type_store.types[ptr_typ.elem_type]
					elem_size := g.type_size(ptr_typ.elem_type)
					if elem_typ.kind == .int_t && elem_size in [1, 2, 4] {
						load_size = elem_size
						is_signed = !elem_typ.is_unsigned
					} else if elem_typ.kind in [.struct_t, .array_t] && elem_size > 8 {
						// Large struct load: copy bytes to stack slot
						g.load_val_to_reg(int(r10), ptr_id)
						if result_off := g.stack_map[val_id] {
							num_chunks := (elem_size + 7) / 8
							for i := 0; i < num_chunks; i++ {
								disp := i * 8
								asm_mov_rax_mem_base_disp(mut g, r10, disp)
								asm_store_rbp_disp_reg(mut g, result_off + disp, rax)
							}
						} else {
							// Single qword fallback
							asm_mov_rax_mem_base_disp(mut g, r10, 0)
							g.store_reg_to_val(0, val_id)
						}
						return
					}
				}
			}
			g.load_val_to_reg(1, ptr_id) // Ptr -> RCX
			match load_size {
				1 {
					if is_signed {
						// movsx rax, byte [rcx]
						g.emit(0x48)
						g.emit(0x0F)
						g.emit(0xBE)
						g.emit(0x01)
					} else {
						asm_load_byte_mem_rcx(mut g)
					}
				}
				2 {
					if is_signed {
						// movsx rax, word [rcx]
						g.emit(0x48)
						g.emit(0x0F)
						g.emit(0xBF)
						g.emit(0x01)
					} else {
						asm_load_word_mem_rcx(mut g)
					}
				}
				4 {
					if is_signed {
						asm_load_dword_sx_mem_rcx(mut g)
					} else {
						asm_load_dword_mem_rcx(mut g)
					}
				}
				else {
					asm_mov_rax_mem_rcx(mut g)
				}
			}
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
		.heap_alloc {
			// Heap-allocate memory: calloc(1, sizeof(T))
			mut alloc_size := 8
			ha_val := g.mod.values[val_id]
			if ha_val.typ > 0 && ha_val.typ < g.mod.type_store.types.len {
				ptr_typ := g.mod.type_store.types[ha_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0 {
					alloc_size = g.type_size(ptr_typ.elem_type)
					if alloc_size <= 0 {
						alloc_size = 8
					}
				}
			}
			// SysV: rdi=1, rsi=size
			asm_mov_reg_imm32(mut g, rdi, 1)
			asm_mov_reg_imm32(mut g, rsi, u32(alloc_size))
			asm_xor_eax_eax(mut g)
			asm_call_rel32(mut g)
			sym_idx := g.elf.add_undefined('calloc')
			g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 4, -4)
			g.emit_u32(0)
			g.store_reg_to_val(0, val_id)
		}
		.get_element_ptr {
			// GEP: Base + scaled index
			idx_id := instr.operands[1]
			base_typ_id := g.mod.values[instr.operands[0]].typ

			// Determine element size from base pointer type
			mut elem_size := 8
			mut is_struct_gep := false
			mut is_array_gep := false

			// If result type == base pointer type, it's array indexing
			base_val_typ := g.mod.values[instr.operands[0]].typ
			if instr.typ == base_val_typ {
				is_array_gep = true
			}

			if base_typ_id > 0 && base_typ_id < g.mod.type_store.types.len {
				base_typ := g.mod.type_store.types[base_typ_id]
				if base_typ.kind == .ptr_t && base_typ.elem_type > 0
					&& base_typ.elem_type < g.mod.type_store.types.len {
					pointee_typ := g.mod.type_store.types[base_typ.elem_type]
					if !is_array_gep && pointee_typ.kind == .struct_t {
						is_struct_gep = true
					}
					// For array types, use array element size, not total array size
					if pointee_typ.kind == .array_t {
						elem_size = g.type_size(pointee_typ.elem_type)
					} else {
						elem_size = g.type_size(base_typ.elem_type)
					}
					if elem_size <= 0 {
						elem_size = 8
					}
				}
			}

			// Struct field GEP with constant index
			idx_val := g.mod.values[idx_id]
			if is_struct_gep && idx_val.kind == .constant {
				field_idx := idx_val.name.int()
				// Get real struct field byte offset
				mut pointee_typ_id := ssa.TypeID(0)
				if base_typ_id > 0 && base_typ_id < g.mod.type_store.types.len {
					pointee_typ_id = g.mod.type_store.types[base_typ_id].elem_type
				}
				field_off := g.struct_field_offset_bytes(pointee_typ_id, field_idx)
				g.load_val_to_reg(0, instr.operands[0])
				if field_off != 0 {
					asm_mov_reg_imm32(mut g, rcx, u32(field_off))
					asm_add_rax_rcx(mut g)
				}
			} else if elem_size == 8 {
				g.load_val_to_reg(0, instr.operands[0]) // Base -> RAX
				g.load_val_to_reg(1, idx_id) // Index -> RCX
				asm_shl_rcx_3(mut g) // rcx *= 8
				asm_add_rax_rcx(mut g)
			} else {
				// Generic: base + index * elem_size
				g.load_val_to_reg(0, idx_id) // RAX = index
				asm_mov_reg_imm32(mut g, rcx, u32(elem_size))
				asm_imul_rax_rcx(mut g) // RAX = index * elem_size
				asm_mov_reg_reg(mut g, rcx, rax) // RCX = scaled offset
				g.load_val_to_reg(0, instr.operands[0]) // RAX = base
				asm_add_rax_rcx(mut g) // RAX = base + offset
			}
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
				ret_typ := g.mod.type_store.types[g.mod.values[val_id].typ]
				ret_size := g.type_size(g.mod.values[val_id].typ)
				if ret_typ.kind == .struct_t && ret_size > 8 && ret_size <= 16 {
					// Multi-register return: RAX holds lower 8 bytes, RDX holds upper
					if result_off := g.stack_map[val_id] {
						asm_store_rbp_disp_reg(mut g, result_off, rax)
						asm_store_rbp_disp_reg(mut g, result_off + 8, rdx)
					}
				} else {
					g.store_reg_to_val(0, val_id)
				}
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
			if off := g.block_offsets[false_blk] {
				// Backward reference: block already emitted, compute displacement.
				rel := off - (g.elf.text_data.len - g.curr_offset + 4)
				g.emit_u32(u32(rel))
			} else {
				// Forward reference: record pending label for later patching.
				g.record_pending_label(false_blk)
				g.emit_u32(0)
			}
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
			src_typ := g.mod.values[src_id].typ
			src_size := g.type_size(src_typ)
			src_type_info := g.mod.type_store.types[src_typ]
			// Multi-word copy for structs/strings (e.g. phi-lowered copies).
			// Both source and dest must have addressable stack slots.
			src_has_slot := src_id in g.stack_map || src_id in g.alloca_offsets
				|| g.mod.values[src_id].kind == .string_literal
			dest_has_slot := dest_id in g.stack_map || dest_id in g.alloca_offsets
			if src_size > 8 && (src_type_info.kind in [.struct_t, .array_t])
				&& src_has_slot && dest_has_slot {
				g.load_address_of_val_to_reg(int(r10), src_id)
				g.load_address_of_val_to_reg(int(r11), dest_id)
				num_chunks := (src_size + 7) / 8
				for ci := 0; ci < num_chunks; ci++ {
					disp := ci * 8
					asm_mov_rax_mem_base_disp(mut g, r10, disp)
					asm_mov_mem_base_disp_rax(mut g, r11, disp)
				}
			} else {
				g.load_val_to_reg(0, src_id)
				g.store_reg_to_val(0, dest_id)
			}
		}
		.trunc, .zext {
			if instr.operands.len > 0 {
				// Check if float-to-float conversion
				src_val := g.mod.values[instr.operands[0]]
				src_is_float := src_val.typ > 0 && src_val.typ < g.mod.type_store.types.len
					&& g.mod.type_store.types[src_val.typ].kind == .float_t
				dst_is_float := g.mod.values[val_id].typ > 0
					&& g.mod.values[val_id].typ < g.mod.type_store.types.len
					&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t

				if src_is_float && dst_is_float {
					g.load_val_to_reg(0, instr.operands[0])
					asm_movq_xmm0_rax(mut g)
					if op == .trunc {
						// f64 -> f32
						asm_cvtsd2ss_xmm0(mut g)
						asm_movd_eax_xmm0(mut g)
					} else {
						// f32 -> f64: load as f32 bits, convert
						asm_movd_xmm0_eax(mut g)
						asm_cvtss2sd_xmm0(mut g)
						asm_movq_rax_xmm0(mut g)
					}
				} else {
					// Integer trunc/zext: just copy (64-bit regs)
					g.load_val_to_reg(0, instr.operands[0])
					if op == .zext {
						// Zero-extend narrow source
						src_typ_id := src_val.typ
						if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
							src_typ := g.mod.type_store.types[src_typ_id]
							if src_typ.kind == .int_t && src_typ.width > 0
								&& src_typ.width < 64 {
								if src_typ.width <= 8 {
									asm_movzx_rax_al(mut g)
								} else if src_typ.width <= 16 {
									asm_movzx_rax_ax(mut g)
								} else if src_typ.width <= 32 {
									// mov eax, eax (zero-extends to 64-bit)
									g.emit(0x89)
									g.emit(0xC0)
								}
							}
						}
					} else if op == .trunc {
						// Truncation: mask to target width
						dst_typ_id := g.mod.values[val_id].typ
						if dst_typ_id > 0 && dst_typ_id < g.mod.type_store.types.len {
							dst_typ := g.mod.type_store.types[dst_typ_id]
							if dst_typ.kind == .int_t && dst_typ.width > 0
								&& dst_typ.width < 64 {
								if dst_typ.width == 1 {
									asm_and_rax_imm32(mut g, 1)
								} else if dst_typ.width <= 8 {
									asm_movzx_rax_al(mut g)
								} else if dst_typ.width <= 16 {
									asm_movzx_rax_ax(mut g)
								} else if dst_typ.width <= 32 {
									g.emit(0x89)
									g.emit(0xC0)
								}
							}
						}
					}
				}
				g.store_reg_to_val(0, val_id)
			}
		}
		.bitcast, .sext {
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				// Check for aggregate bitcast that needs byte copy
				dest_typ_id := g.mod.values[val_id].typ
				mut copied_aggregate := false
				if dest_typ_id > 0 && dest_typ_id < g.mod.type_store.types.len {
					dest_typ := g.mod.type_store.types[dest_typ_id]
					dest_size := g.type_size(dest_typ_id)
					if dest_typ.kind in [.struct_t, .array_t] && dest_size > 8 {
						if dest_off := g.stack_map[val_id] {
							if src_off := g.stack_map[src_id] {
								num_chunks := (dest_size + 7) / 8
								for i := 0; i < num_chunks; i++ {
									asm_load_reg_rbp_disp(mut g, rax, src_off + i * 8)
									asm_store_rbp_disp_reg(mut g, dest_off + i * 8, rax)
								}
								copied_aggregate = true
							}
						}
					}
				}
				if !copied_aggregate {
					g.load_val_to_reg(0, src_id)
					// For sext, sign-extend narrow source
					if op == .sext {
						src_typ_id := g.mod.values[src_id].typ
						if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
							src_typ := g.mod.type_store.types[src_typ_id]
							if src_typ.kind == .int_t && src_typ.width > 0
								&& src_typ.width < 64 {
								if src_typ.width <= 8 {
									asm_movsx_rax_al(mut g)
								} else if src_typ.width <= 16 {
									asm_movsx_rax_ax(mut g)
								} else if src_typ.width <= 32 {
									asm_movsxd_rax_eax(mut g)
								}
							}
						}
					}
					g.store_reg_to_val(0, val_id)
				}
			}
		}
		.extractvalue {
			// Extract element from tuple/struct
			tuple_id := instr.operands[0]
			idx_val := g.mod.values[instr.operands[1]]
			idx := idx_val.name.int()

			tuple_val := g.mod.values[tuple_id]

			// String/C-string literals need materialization before field extraction.
			// load_val_to_reg writes {str, len, is_lit} into the stack slot;
			// without this, extractvalue reads uninitialized memory.
			if tuple_val.kind in [.string_literal, .c_string_literal] {
				g.load_val_to_reg(0, tuple_id)
			}

			mut field_byte_off := idx * 8
			mut field_elem_size := 8

			if tuple_val.typ > 0 && tuple_val.typ < g.mod.type_store.types.len {
				tuple_typ := g.mod.type_store.types[tuple_val.typ]
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

			// Also check if tuple is a load from a struct pointer
			mut scalar_load_agg_src_ptr_id := 0
			if tuple_val.kind == .instruction {
				tuple_ins := g.mod.instrs[tuple_val.index]
				if g.selected_opcode(tuple_ins) == .load && tuple_ins.operands.len > 0 {
					load_ptr_id := tuple_ins.operands[0]
					if load_ptr_id > 0 && load_ptr_id < g.mod.values.len {
						load_ptr_val := g.mod.values[load_ptr_id]
						if load_ptr_val.typ > 0
							&& load_ptr_val.typ < g.mod.type_store.types.len {
							load_ptr_typ := g.mod.type_store.types[load_ptr_val.typ]
							if load_ptr_typ.kind == .ptr_t && load_ptr_typ.elem_type > 0
								&& load_ptr_typ.elem_type < g.mod.type_store.types.len {
								load_elem_typ := g.mod.type_store.types[load_ptr_typ.elem_type]
								if load_elem_typ.kind in [.struct_t, .array_t] {
									scalar_load_agg_src_ptr_id = load_ptr_id
									if load_elem_typ.kind == .struct_t && idx >= 0 {
										field_byte_off = g.struct_field_offset_bytes(load_ptr_typ.elem_type,
											idx)
										if idx < load_elem_typ.fields.len {
											field_elem_size = g.type_size(load_elem_typ.fields[idx])
											if field_elem_size <= 0 {
												field_elem_size = 8
											}
										}
									}
								}
							}
						}
					}
				}
			}

			// Check if result is aggregate
			mut result_is_agg := false
			mut result_size := 0
			if instr.typ > 0 && instr.typ < g.mod.type_store.types.len {
				result_typ := g.mod.type_store.types[instr.typ]
				result_size = g.type_size(instr.typ)
				result_is_agg = result_typ.kind in [.struct_t, .array_t]
			}

			if scalar_load_agg_src_ptr_id > 0 {
				// Load from struct pointer + field offset
				g.load_val_to_reg(int(r10), scalar_load_agg_src_ptr_id)
				if result_is_agg && result_size > 8 {
					if result_off := g.stack_map[val_id] {
						num_chunks := (result_size + 7) / 8
						for i := 0; i < num_chunks; i++ {
							asm_mov_rax_mem_base_disp(mut g, r10, field_byte_off + i * 8)
							asm_store_rbp_disp_reg(mut g, result_off + i * 8, rax)
						}
					}
				} else {
					g.emit_sized_load_base_disp(r10, field_byte_off, field_elem_size)
					g.store_reg_to_val(0, val_id)
				}
			} else if tuple_off := g.stack_map[tuple_id] {
				// Load from stack slot + field offset
				if result_is_agg && result_size > 8 {
					if result_off := g.stack_map[val_id] {
						num_chunks := (result_size + 7) / 8
						for i := 0; i < num_chunks; i++ {
							asm_load_reg_rbp_disp(mut g, rax, tuple_off + field_byte_off + i * 8)
							asm_store_rbp_disp_reg(mut g, result_off + i * 8, rax)
						}
					}
				} else {
					// Use sized load for struct fields to avoid reading adjacent fields
					g.emit_sized_load_base_disp(Reg(5), tuple_off + field_byte_off,
						field_elem_size)
					g.store_reg_to_val(0, val_id)
				}
			} else if tuple_id in g.reg_map {
				// Tuple in register - treat as pointer to struct
				tuple_reg := g.reg_map[tuple_id]
				g.emit_sized_load_base_disp(Reg(tuple_reg), field_byte_off, field_elem_size)
				g.store_reg_to_val(0, val_id)
			} else {
				// Fallback
				g.load_val_to_reg(0, tuple_id)
				g.store_reg_to_val(0, val_id)
			}
		}
		.insertvalue {
			// Insert element into tuple/struct
			tuple_id := instr.operands[0]
			elem_id := instr.operands[1]
			idx_val := g.mod.values[instr.operands[2]]
			idx := idx_val.name.int()

			result_offset := g.stack_map[val_id]
			tuple_typ := g.mod.type_store.types[instr.typ]
			tuple_size := g.type_size(instr.typ)
			num_chunks := if tuple_size > 0 { (tuple_size + 7) / 8 } else { 1 }
			mut elem_off := idx * 8
			if tuple_typ.kind == .struct_t && idx >= 0 && idx < tuple_typ.fields.len {
				elem_off = g.struct_field_offset_bytes(instr.typ, idx)
			}

			// Copy existing tuple data
			tuple_val := g.mod.values[tuple_id]
			if !(tuple_val.kind == .constant && tuple_val.name == 'undef') {
				if tuple_offset := g.stack_map[tuple_id] {
					for i in 0 .. num_chunks {
						asm_load_reg_rbp_disp(mut g, rax, tuple_offset + i * 8)
						asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
					}
				} else {
					// Zero-init
					asm_xor_reg_reg(mut g, rax)
					for i in 0 .. num_chunks {
						asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
					}
				}
			} else {
				// Zero-init for undef
				asm_xor_reg_reg(mut g, rax)
				for i in 0 .. num_chunks {
					asm_store_rbp_disp_reg(mut g, result_offset + i * 8, rax)
				}
			}

			// Store the new element
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
				store_off := result_offset + elem_off
				match elem_size {
					1 { asm_store_rbp_disp8_reg8(mut g, store_off, rax) }
					2 { asm_store_rbp_disp16_reg16(mut g, store_off, rax) }
					4 { asm_store_rbp_disp32_reg32(mut g, store_off, rax) }
					else { asm_store_rbp_disp_reg(mut g, store_off, rax) }
				}
			} else {
				// Multi-word element
				elem_chunks := (elem_size + 7) / 8
				if elem_offset := g.stack_map[elem_id] {
					for i in 0 .. elem_chunks {
						asm_load_reg_rbp_disp(mut g, rax, elem_offset + i * 8)
						asm_store_rbp_disp_reg(mut g, result_offset + elem_off + i * 8,
							rax)
					}
				} else {
					g.load_val_to_reg(0, elem_id)
					asm_store_rbp_disp_reg(mut g, result_offset + elem_off, rax)
					if elem_chunks > 1 {
						asm_xor_reg_reg(mut g, rax)
						for i in 1 .. elem_chunks {
							asm_store_rbp_disp_reg(mut g, result_offset + elem_off + i * 8,
								rax)
						}
					}
				}
			}
		}
		.struct_init {
			// Create struct from field values
			result_offset := g.stack_map[val_id]
			struct_typ := g.mod.type_store.types[instr.typ]
			struct_size := g.type_size(instr.typ)
			zero_size := if struct_size > 0 { struct_size } else { 8 }

			// Zero-initialize the entire struct first
			asm_xor_reg_reg(mut g, rax)
			mut off := 0
			for off + 8 <= zero_size {
				asm_store_rbp_disp_reg(mut g, result_offset + off, rax)
				off += 8
			}
			if off < zero_size {
				// Store remaining bytes
				asm_store_rbp_disp_reg(mut g, result_offset + off, rax)
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

				// Skip zero constants (already zeroed)
				field_val := g.mod.values[field_id]
				if field_val.kind == .constant && field_val.name == '0' {
					continue
				}

				if field_size <= 8 {
					g.load_val_to_reg(0, field_id)
					store_off := result_offset + field_off
					match field_size {
						1 { asm_store_rbp_disp8_reg8(mut g, store_off, rax) }
						2 { asm_store_rbp_disp16_reg16(mut g, store_off, rax) }
						4 { asm_store_rbp_disp32_reg32(mut g, store_off, rax) }
						else { asm_store_rbp_disp_reg(mut g, store_off, rax) }
					}
				} else {
					// Multi-word field (nested struct)
					field_chunks := (field_size + 7) / 8
					if field_offset := g.stack_map[field_id] {
						for w in 0 .. field_chunks {
							asm_load_reg_rbp_disp(mut g, rax, field_offset + w * 8)
							asm_store_rbp_disp_reg(mut g, result_offset + field_off + w * 8,
								rax)
						}
					} else {
						g.load_val_to_reg(0, field_id)
						asm_store_rbp_disp_reg(mut g, result_offset + field_off, rax)
						if field_chunks > 1 {
							asm_xor_reg_reg(mut g, rax)
							for w in 1 .. field_chunks {
								asm_store_rbp_disp_reg(mut g, result_offset + field_off + w * 8,
									rax)
							}
						}
					}
				}
			}
		}
		.inline_string_init {
			// Create string struct by value: { str, len, is_lit }
			str_ptr_id := instr.operands[0]
			len_id := instr.operands[1]
			is_lit_id := instr.operands[2]

			base_offset := g.stack_map[val_id]
			struct_offset := base_offset + 8 // string data lives above pointer slot
			str_off, _ := g.struct_field_offset_and_size(instr.typ, 0, 0, 8)
			len_off, len_size := g.struct_field_offset_and_size(instr.typ, 1, 8, 8)
			is_lit_off, is_lit_size := g.struct_field_offset_and_size(instr.typ, 2, 16, 8)

			// Store str field
			g.load_val_to_reg(0, str_ptr_id)
			asm_store_rbp_disp_reg(mut g, struct_offset + str_off, rax)

			// Store len field
			g.load_val_to_reg(0, len_id)
			g.emit_sized_store_rbp(struct_offset + len_off, len_size)

			// Store is_lit field
			g.load_val_to_reg(0, is_lit_id)
			g.emit_sized_store_rbp(struct_offset + is_lit_off, is_lit_size)

			// Return pointer to struct
			asm_lea_rax_rbp_disp32(mut g, struct_offset)
			g.store_reg_to_val(0, val_id)
		}
		.phi {
			// Phi nodes are eliminated by optimization (converted to assignments)
		}
		.unreachable {
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
	} else if val.kind == .c_string_literal {
		// C string literal: raw char* pointer to string data in .rodata
		raw_bytes := val.name.bytes()
		str_offset := g.elf.rodata.len
		mut i := 0
		for i < raw_bytes.len {
			if raw_bytes[i] == `\\` && i + 1 < raw_bytes.len {
				match raw_bytes[i + 1] {
					`n` { g.elf.rodata << 0x0A }
					`t` { g.elf.rodata << 0x09 }
					`r` { g.elf.rodata << 0x0D }
					`0` { g.elf.rodata << 0x00 }
					`\\` { g.elf.rodata << 0x5C }
					`'` { g.elf.rodata << 0x27 }
					`"` { g.elf.rodata << 0x22 }
					else { g.elf.rodata << raw_bytes[i + 1] }
				}
				i += 2
			} else {
				g.elf.rodata << raw_bytes[i]
				i++
			}
		}
		g.elf.rodata << 0 // null terminator
		sym_name := 'L_cstr_${str_offset}'
		sym_idx := g.elf.add_symbol(sym_name, u64(str_offset), false, 3) // shndx 3 = .rodata
		asm_lea_reg_rip(mut g, Reg(reg))
		g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4) // R_X86_64_PC32
		g.emit_u32(0)
	} else if val.kind == .string_literal {
		// V string literal: create { str, len, is_lit } struct on stack
		str_content := val.name
		str_len := val.index

		base_offset := g.stack_map[val_id]

		// Put string data in .rodata
		str_data_offset := g.elf.rodata.len
		g.elf.rodata << str_content.bytes()
		g.elf.rodata << 0

		sym_name := 'L_str_${str_data_offset}'
		sym_idx := g.elf.add_symbol(sym_name, u64(str_data_offset), false, 3)

		// Store str pointer: lea rax, [rip + str_data]; mov [rbp + base], rax
		asm_lea_reg_rip(mut g, rax)
		g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
		g.emit_u32(0)

		str_off, _ := g.struct_field_offset_and_size(val.typ, 0, 0, 8)
		len_off, len_size := g.struct_field_offset_and_size(val.typ, 1, 8, 8)
		is_lit_off, is_lit_size := g.struct_field_offset_and_size(val.typ, 2, 16, 8)

		asm_store_rbp_disp_reg(mut g, base_offset + str_off, rax)

		// Store len
		asm_mov_reg_imm64(mut g, rax, u64(str_len))
		g.emit_sized_store_rbp(base_offset + len_off, len_size)

		// Store is_lit = 1
		asm_mov_reg_imm32(mut g, rax, 1)
		g.emit_sized_store_rbp(base_offset + is_lit_off, is_lit_size)

		// Load pointer to struct into reg
		asm_lea_rbp_disp(mut g, Reg(reg), base_offset)
	} else if val.kind == .func_ref {
		// Function pointer: load address of the function
		sym_idx := g.elf.add_undefined(val.name)
		asm_lea_reg_rip(mut g, Reg(reg))
		g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
		g.emit_u32(0)
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

// Store entry-point argument register to a named global.
// `src_reg` is the hardware register index (e.g. rdi=7, rsi=6).
// `size` is 4 for i32 (argc) or 8 for pointer (argv).
fn (mut g Gen) store_entry_arg_to_global(src_reg int, global_name string, size int) {
	// mov rax, <src_reg>
	asm_mov_reg_reg(mut g, rax, Reg(src_reg))
	// lea rcx, [rip + global]
	asm_lea_reg_rip(mut g, rcx)
	sym_idx := g.elf.add_undefined(global_name)
	g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
	g.emit_u32(0)
	// Store rax/eax to [rcx]
	if size == 4 {
		asm_store_mem_rcx_eax(mut g)
	} else {
		asm_mov_mem_rcx_rax(mut g)
	}
}

fn (mut g Gen) type_size(typ_id ssa.TypeID) int {
	if typ_id <= 0 {
		return 0
	}
	if typ_id >= g.mod.type_store.types.len {
		return 8
	}
	if typ_id < g.type_size_cache.len && g.type_size_cache[typ_id] != 0 {
		return g.type_size_cache[typ_id]
	}
	typ := g.mod.type_store.types[typ_id]
	size := match typ.kind {
		.void_t { 0 }
		.int_t { if typ.width > 0 { (typ.width + 7) / 8 } else { 8 } }
		.float_t { if typ.width > 0 { (typ.width + 7) / 8 } else { 8 } }
		.ptr_t { 8 }
		.array_t { typ.len * g.type_size(typ.elem_type) }
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
			if total > 0 { total } else { 8 }
		}
		.func_t { 8 }
		.label_t, .metadata_t { 0 }
	}
	if typ_id < g.type_size_cache.len && size > 0 {
		g.type_size_cache[typ_id] = size
	}
	return size
}

fn (mut g Gen) type_align(typ_id ssa.TypeID) int {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return if typ_id == 0 { 1 } else { 8 }
	}
	if typ_id < g.type_align_cache.len && g.type_align_cache[typ_id] != 0 {
		return g.type_align_cache[typ_id] - 1
	}
	typ := g.mod.type_store.types[typ_id]
	mut align := 1
	if typ.kind == .array_t {
		align = g.type_align(typ.elem_type)
	} else {
		size := g.type_size(typ_id)
		if size >= 8 {
			align = 8
		} else if size >= 4 {
			align = 4
		} else if size >= 2 {
			align = 2
		}
	}
	if typ_id < g.type_align_cache.len {
		g.type_align_cache[typ_id] = align + 1
	}
	return align
}

fn (mut g Gen) struct_field_offset_bytes(struct_typ_id ssa.TypeID, field_idx int) int {
	if struct_typ_id <= 0 || struct_typ_id >= g.mod.type_store.types.len {
		return field_idx * 8
	}
	cache_key := (struct_typ_id << 16) | field_idx
	if cache_key in g.struct_field_offset_cache {
		return g.struct_field_offset_cache[cache_key]
	}
	typ := g.mod.type_store.types[struct_typ_id]
	if typ.kind != .struct_t || field_idx < 0 || field_idx >= typ.fields.len {
		return field_idx * 8
	}
	if typ.is_union {
		return 0
	}
	mut offset := 0
	for i, field_typ in typ.fields {
		align := g.type_align(field_typ)
		if align > 1 && offset % align != 0 {
			offset = (offset + align - 1) & ~(align - 1)
		}
		if i == field_idx {
			g.struct_field_offset_cache[cache_key] = offset
			return offset
		}
		field_size := g.type_size(field_typ)
		offset += if field_size > 0 { field_size } else { 8 }
	}
	return field_idx * 8
}

fn (mut g Gen) struct_field_offset_and_size(struct_typ_id ssa.TypeID, field_idx int, default_off int, default_size int) (int, int) {
	mut off := default_off
	mut size := default_size
	if struct_typ_id > 0 && struct_typ_id < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[struct_typ_id]
		if typ.kind == .struct_t && field_idx >= 0 && field_idx < typ.fields.len {
			off = g.struct_field_offset_bytes(struct_typ_id, field_idx)
			field_size := g.type_size(typ.fields[field_idx])
			if field_size > 0 {
				size = field_size
			}
		}
	}
	if size !in [1, 2, 4, 8] {
		size = 8
	}
	return off, size
}

fn (mut g Gen) canonicalize_narrow_int(typ ssa.Type) {
	if typ.width == 1 {
		asm_and_rax_imm32(mut g, 1)
	} else if typ.is_unsigned {
		if typ.width <= 8 {
			asm_movzx_rax_al(mut g)
		} else if typ.width <= 16 {
			asm_movzx_rax_ax(mut g)
		} else if typ.width <= 32 {
			// mov eax, eax (zero-extends)
			g.emit(0x89)
			g.emit(0xC0)
		}
	} else {
		if typ.width <= 8 {
			asm_movsx_rax_al(mut g)
		} else if typ.width <= 16 {
			asm_movsx_rax_ax(mut g)
		} else if typ.width <= 32 {
			asm_movsxd_rax_eax(mut g)
		}
	}
}

// Emit sized store from rax to [rbp + disp]
fn (mut g Gen) emit_sized_store_rbp(disp int, size int) {
	match size {
		1 { asm_store_rbp_disp8_reg8(mut g, disp, rax) }
		2 { asm_store_rbp_disp16_reg16(mut g, disp, rax) }
		4 { asm_store_rbp_disp32_reg32(mut g, disp, rax) }
		else { asm_store_rbp_disp_reg(mut g, disp, rax) }
	}
}

// Emit sized load from [base + disp] to rax
fn (mut g Gen) emit_sized_load_base_disp(base Reg, disp int, size int) {
	match size {
		1 { asm_load_byte_base_disp(mut g, base, disp) }
		2 { asm_load_word_base_disp(mut g, base, disp) }
		4 {
			// mov eax, [base + disp] (zero-extend 32-bit)
			base_hw := g.map_reg(int(base))
			if base_hw >= 8 {
				g.emit(0x41)
			}
			g.emit(0x8B)
			rm := base_hw & 7
			needs_sib := rm == 4
			if disp == 0 && rm != 5 {
				g.emit(rm)
				if needs_sib {
					g.emit(0x24)
				}
			} else if disp >= -128 && disp <= 127 {
				g.emit(0x40 | rm)
				if needs_sib {
					g.emit(0x24)
				}
				g.emit(u8(disp))
			} else {
				g.emit(0x80 | rm)
				if needs_sib {
					g.emit(0x24)
				}
				g.emit_u32(u32(disp))
			}
		}
		else { asm_mov_rax_mem_base_disp(mut g, base, disp) }
	}
}

fn (mut g Gen) load_address_of_val_to_reg(reg int, val_id int) {
	val := g.mod.values[val_id]
	// string_literal values need full materialization before their address
	// can be taken — the struct {str, len, is_lit} must be built on stack.
	if val.kind == .string_literal {
		g.load_val_to_reg(reg, val_id)
		return
	}
	if val.kind == .instruction {
		instr := g.mod.instrs[val.index]
		if instr.op == .alloca {
			if data_off := g.alloca_offsets[val_id] {
				asm_lea_rbp_disp(mut g, Reg(reg), data_off)
				return
			}
		}
	}
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
	// Register allocation is disabled for correctness — the codegen uses
	// callee-saved registers (rbx, r12-r15) as temporaries in generated
	// instruction sequences, which conflicts with values held in those
	// registers across basic blocks.  All values use stack slots instead.
	g.reg_map = map[int]int{}
	g.used_regs = []int{}
}
