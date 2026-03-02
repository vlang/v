// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

import v2.mir
import v2.ssa
import v2.types
import encoding.binary

pub struct Gen {
	mod &mir.Module
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

	// Current function's return type (for handling struct returns)
	cur_func_ret_type int
	cur_func_name     string

	// Stack offset where x8 (indirect return pointer) is saved for large struct returns
	x8_save_offset int
	// Cache for deduplicating string data in cstring section (content -> offset)
	string_data_cache map[string]int
}

pub fn Gen.new(mod &mir.Module) &Gen {
	return &Gen{
		mod:   mod
		macho: MachOObject.new()
	}
}

pub fn (mut g Gen) gen() {
	// Pre-register global symbols BEFORE generating functions
	// This ensures add_undefined() finds existing symbols instead of creating undefined ones
	mut data_offset := u64(0)
	for gvar in g.mod.globals {
		// Skip external globals (defined elsewhere, e.g. __stdoutp)
		if gvar.linkage == .external {
			continue
		}
		// Align to 8 bytes
		data_offset = (data_offset + 7) & ~7
		g.macho.add_symbol('_' + gvar.name, data_offset, true, 3)
		size := if gvar.initial_data.len > 0 {
			gvar.initial_data.len
		} else {
			g.type_size(gvar.typ)
		}
		data_offset += u64(size)
	}

	for func in g.mod.funcs {
		g.gen_func(func)
	}

	// Add return-zero stub for unresolved symbols.
	// When the linker can't resolve a symbol, it redirects calls here instead of
	// letting them jump to the Mach-O header which corrupts memory.
	g.macho.add_symbol('___unresolved_stub', u64(g.macho.text_data.len), false, 1)
	g.emit(0xD2800000) // mov x0, #0
	g.emit(0xD2800001) // mov x1, #0
	g.emit(0xD65F03C0) // ret

	// Globals in __data (Section 3) - emit actual data
	for gvar in g.mod.globals {
		// Skip external globals (defined elsewhere)
		if gvar.linkage == .external {
			continue
		}
		for g.macho.data_data.len % 8 != 0 {
			g.macho.data_data << 0
		}
		// Constant arrays: emit raw element data directly
		if gvar.initial_data.len > 0 {
			g.macho.data_data << gvar.initial_data
			continue
		}
		// Calculate actual size of the global variable based on its type.
		size := g.type_size(gvar.typ)
		if gvar.is_constant {
			match size {
				1 {
					g.macho.data_data << u8(gvar.initial_value)
				}
				2 {
					mut bytes := []u8{len: 2}
					binary.little_endian_put_u16(mut bytes, u16(gvar.initial_value))
					g.macho.data_data << bytes
				}
				4 {
					mut bytes := []u8{len: 4}
					binary.little_endian_put_u32(mut bytes, u32(gvar.initial_value))
					g.macho.data_data << bytes
				}
				8 {
					mut bytes := []u8{len: 8}
					binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
					g.macho.data_data << bytes
				}
				else {
					// For struct constants (e.g., sum types), emit initial_value as first 8 bytes
					// (the tag for sum types), then zeros for the rest.
					if gvar.initial_value != 0 && size >= 8 {
						mut bytes := []u8{len: 8}
						binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
						g.macho.data_data << bytes
						for _ in 0 .. size - 8 {
							g.macho.data_data << 0
						}
					} else {
						for _ in 0 .. size {
							g.macho.data_data << 0
						}
					}
				}
			}
		} else {
			// For regular globals, initialize with zeros.
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

fn (mut g Gen) gen_func(func mir.Function) {
	if func.is_c_extern {
		// C extern functions are provided by external libraries (libc, etc.).
		// Don't emit any local symbol — let the linker resolve them as undefined externals.
		return
	}
	if func.blocks.len == 0 {
		// Emit a minimal stub: just a ret instruction
		// This is needed for functions like __v_init_consts that are called but have no body
		g.curr_offset = g.macho.text_data.len
		sym_name := '_' + func.name
		g.macho.add_symbol(sym_name, u64(g.curr_offset), false, 1)
		g.emit(0xd65f03c0) // ret
		return
	}
	g.curr_offset = g.macho.text_data.len
	g.stack_map = map[int]int{}
	g.alloca_offsets = map[int]int{}
	g.block_offsets = map[int]int{}
	g.pending_labels = map[int][]int{}
	g.reg_map = map[int]int{}
	g.used_regs = []int{}
	g.string_literal_offsets = map[int]int{}
	g.const_cache = map[int]i64{}
	g.cur_func_ret_type = func.typ
	g.cur_func_name = func.name
	g.x8_save_offset = 0
	g.allocate_registers(func)

	// Check if function requires indirect return pointer preservation in x8.
	fn_ret_typ := g.mod.type_store.types[func.typ]
	fn_ret_size := g.type_size(func.typ)
	needs_x8_save := func.abi_ret_indirect || (fn_ret_typ.kind == .struct_t && fn_ret_size > 16)

	// Callee-saved registers are pushed at [fp - 8], [fp - 16], etc.
	// We need to account for this when computing stack offsets
	callee_saved_size := ((g.used_regs.len + 1) / 2) * 16

	// Stack Frame - start after callee-saved register area
	mut slot_offset := 8 + callee_saved_size

	// If function returns large struct, reserve slot for saving x8
	if needs_x8_save {
		g.x8_save_offset = -slot_offset
		slot_offset += 8
	}

	for pi, pid in func.params {
		// For struct parameters, allocate full struct size on the stack.
		// On ARM64, structs > 16 bytes are passed by pointer (indirect),
		// and structs 9-16 bytes are passed in 2 consecutive registers.
		param_typ := g.mod.values[pid].typ
		param_type_info := g.mod.type_store.types[param_typ]
		param_size := g.type_size(param_typ)
		is_indirect_param := pi < func.abi_param_class.len && func.abi_param_class[pi] == .indirect
		if is_indirect_param || (param_type_info.kind == .struct_t && param_size > 16) {
			// Align to 16 bytes and allocate full struct size
			slot_offset = (slot_offset + 15) & ~0xF
			slot_offset += param_size
			g.stack_map[pid] = -slot_offset
			// Reserve one more scalar slot so following values do not overlap
			// with the first field at the base offset.
			slot_offset += 8
		} else if param_type_info.kind == .struct_t && param_size > 8 {
			// Small struct (9-16 bytes) passed in 2 registers - allocate full size
			slot_offset = (slot_offset + 7) & ~0x7
			slot_offset += param_size
			g.stack_map[pid] = -slot_offset
			slot_offset += 8
		} else {
			g.stack_map[pid] = -slot_offset
			slot_offset += 8
		}
	}

	// Pre-pass: find string_literal values used in this function and allocate stack for them
	mut used_string_literals := map[int]bool{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			// Check all operands for string_literal references
			for op in instr.operands {
				op_val := g.mod.values[op]
				if op_val.kind == .string_literal {
					used_string_literals[op] = true
				}
			}
		}
	}
	// Also check return values - if function returns a string_literal directly
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind == .instruction {
				instr := g.mod.instrs[val.index]
				if instr.op == .ret && instr.operands.len > 0 {
					ret_val := g.mod.values[instr.operands[0]]
					if ret_val.kind == .string_literal {
						used_string_literals[instr.operands[0]] = true
					}
				}
			}
		}
	}

	// Allocate stack slots for used string_literal values
	for str_lit_id, _ in used_string_literals {
		// String struct needs 24 bytes (str ptr + len + is_lit)
		slot_offset = (slot_offset + 15) & ~0xF
		slot_offset += 24
		g.stack_map[str_lit_id] = -slot_offset
		// Keep subsequent scalar slots below the aggregate base.
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
				// Plus 8 bytes for the result pointer, stored separately
				// so that store_reg_to_val doesn't overwrite field 0.
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += 24 // struct data
				slot_offset += 8 // pointer slot (separate from struct)
				g.stack_map[val_id] = -slot_offset
				continue
			}

			if instr.op == .insertvalue || instr.op == .struct_init {
				// Tuple/struct needs full ABI size, not just fields.len * 8.
				tuple_typ := g.mod.type_store.types[instr.typ]
				mut tuple_size := g.type_size(instr.typ)
				if tuple_size <= 0 {
					tuple_size = tuple_typ.fields.len * 8
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += tuple_size
				g.stack_map[val_id] = -slot_offset
				// Keep following scalar slots from overlapping field 0.
				slot_offset += 8
				continue
			}

			// Keep full stack storage for struct/array values so aggregate copies have
			// stable backing bytes even when values are register-allocated.
			val_typ := g.mod.type_store.types[val.typ]
			if val_typ.kind == .struct_t || val_typ.kind == .array_t {
				mut struct_size := g.type_size(val.typ)
				if struct_size <= 0 {
					struct_size = if val_typ.fields.len > 0 { val_typ.fields.len * 8 } else { 8 }
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += struct_size
				g.stack_map[val_id] = -slot_offset
				// Keep following scalar slots below the aggregate base.
				slot_offset += 8
				continue
			}

			if instr.op == .call {
				// Check if call returns a tuple
				result_typ := g.mod.type_store.types[val.typ]
				mut is_multi_reg_call := result_typ.kind == .struct_t && result_typ.fields.len > 1
				mut call_tuple_size := g.type_size(val.typ)
				// Also check callee's registered return type
				if !is_multi_reg_call && instr.operands.len > 0 {
					callee_val := g.mod.values[instr.operands[0]]
					if callee_val.kind == .func_ref {
						for f in g.mod.funcs {
							if f.name == callee_val.name {
								callee_ret_typ := g.mod.type_store.types[f.typ]
								callee_ret_size := g.type_size(f.typ)
								if callee_ret_typ.kind == .struct_t && callee_ret_size > 8
									&& callee_ret_size <= 16 {
									is_multi_reg_call = true
									call_tuple_size = callee_ret_size
								}
								break
							}
						}
					}
				}
				if is_multi_reg_call {
					if call_tuple_size <= 0 {
						call_tuple_size = 16
					}
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += call_tuple_size
					g.stack_map[val_id] = -slot_offset
					// Keep following scalar slots below the aggregate base.
					slot_offset += 8
					continue
				}
			} else if instr.op == .call_sret {
				// call_sret returns an aggregate indirectly into the destination slot.
				result_typ := g.mod.type_store.types[val.typ]
				if result_typ.kind == .struct_t {
					result_size := g.type_size(val.typ)
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += result_size
					g.stack_map[val_id] = -slot_offset
					// Keep following scalar slots below the aggregate base.
					slot_offset += 8
					continue
				}
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

	// Save callee-saved regs (pushed below fp using pre-decrement)
	for i := 0; i < g.used_regs.len; i += 2 {
		r1 := g.used_regs[i]
		mut r2 := 31 // xzr
		if i + 1 < g.used_regs.len {
			r2 = g.used_regs[i + 1]
		}
		g.emit(asm_stp_pair_pre(Reg(r1), Reg(r2)))
	}

	g.emit_sub_sp(g.stack_size)

	// Save x8 if this function returns a large struct
	// x8 contains the indirect return pointer from the caller
	// Save it at a fixed offset from fp (below callee-saved registers)
	if g.x8_save_offset != 0 {
		g.emit_str_reg_offset(8, 29, g.x8_save_offset)
	}

	// The Mach-O LC_MAIN entrypoint invokes `main` with C-style argc/argv in
	// x0/x1. Persist them to builtin globals so `os.args` / `arguments()` work.
	if func.name == 'main' {
		g.store_entry_arg_to_global(0, 'g_main_argc')
		g.store_entry_arg_to_global(1, 'g_main_argv')
		// Call _vinit to initialize dynamic array constants
		g.emit_call_to_named_fn('_vinit')
	}

	// Spill params
	// ARM64 ABI: args in x0..x7, args 8+ on caller stack.
	// Struct params ≤ 16 bytes occupy ceil(size/8) consecutive registers.
	// Struct params > 16 bytes are passed by pointer (one register).
	// ARM64 ABI: integer params in x0-x7, float params in d0-d7 (independent allocation)
	mut reg_idx := 0
	mut float_reg_idx := 0
	for i, pid in func.params {
		param_typ := g.mod.values[pid].typ
		param_type_info := g.mod.type_store.types[param_typ]
		param_size := g.type_size(param_typ)
		is_indirect_param := i < func.abi_param_class.len && func.abi_param_class[i] == .indirect

		// Float parameters arrive in d-registers; move to x-register for storage
		if param_type_info.kind == .float_t && !is_indirect_param {
			if float_reg_idx < 8 {
				// fmov xN, dN to get float bits into integer register
				g.emit(asm_fmov_x_d(Reg(9), float_reg_idx))
				if reg := g.reg_map[pid] {
					g.emit_mov_reg(reg, 9)
				} else {
					offset := g.stack_map[pid]
					g.emit_str_reg_offset(9, 29, offset)
				}
			}
			float_reg_idx++
			continue
		}

		mut src_reg := reg_idx
		if reg_idx >= 8 {
			stack_arg_off := 16 + ((reg_idx - 8) * 8)
			g.emit_ldr_reg_offset(9, 29, stack_arg_off)
			src_reg = 9
		}

		// For large struct parameters (> 16 bytes), the argument value is a pointer.
		// Copy pointed struct bytes into the function-local spill slot.
		if is_indirect_param || (param_type_info.kind == .struct_t && param_size > 16) {
			if src_reg != 9 {
				g.emit_mov_reg(9, src_reg)
			}
			offset := g.stack_map[pid]
			num_fields := (param_size + 7) / 8
			for field_idx in 0 .. num_fields {
				g.emit(asm_ldr_imm(Reg(10), Reg(9), u32(field_idx)))
				g.emit_str_reg_offset(10, 29, offset + field_idx * 8)
			}
			// Large/indirect params are represented as addresses in registers.
			// Materialize the local spill address for any register-allocated uses.
			if reg := g.reg_map[pid] {
				g.emit_add_fp_imm(reg, offset)
			}
			reg_idx += 1
		} else if param_type_info.kind == .struct_t && param_size > 8 {
			// Small struct (9-16 bytes) passed in 2 consecutive registers.
			offset := g.stack_map[pid]
			num_regs := (param_size + 7) / 8
			for ri in 0 .. num_regs {
				mut cur_reg := reg_idx + ri
				if cur_reg >= 8 {
					stack_arg_off := 16 + ((cur_reg - 8) * 8)
					g.emit_ldr_reg_offset(9, 29, stack_arg_off)
					g.emit_str_reg_offset(9, 29, offset + ri * 8)
				} else {
					g.emit_str_reg_offset(cur_reg, 29, offset + ri * 8)
				}
			}
			if reg := g.reg_map[pid] {
				g.emit_add_fp_imm(reg, offset)
			}
			reg_idx += num_regs
		} else if reg := g.reg_map[pid] {
			if reg != src_reg {
				g.emit_mov_reg(reg, src_reg)
			}
			reg_idx += 1
		} else {
			offset := g.stack_map[pid]
			g.emit_str_reg_offset(src_reg, 29, offset)
			reg_idx += 1
		}
	}

	// Run SSA lowered global initializers before entering user main.
	// This mirrors the C backend behavior where __v2_global_init() is invoked from main.
	if func.name == 'main' && g.has_function_named('__v2_global_init') {
		sym_idx := g.macho.add_undefined('_' + '__v2_global_init')
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
		g.emit(asm_bl_reloc())
	}

	for i, blk_id in func.blocks {
		g.next_blk = if i + 1 < func.blocks.len { func.blocks[i + 1] } else { -1 }
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
	op := g.selected_opcode(instr)
	match op {
		.fadd, .fsub, .fmul, .fdiv, .frem {
			// Float operations using scalar SIMD instructions (d0-d7)
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// For now, load operands as float constants or from memory
			// Load LHS to d0
			g.load_float_operand(instr.operands[0], 0) // d0
			// Load RHS to d1
			g.load_float_operand(instr.operands[1], 1) // d1

			// Perform float operation: result in d0
			match op {
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

			// Check if target is f32
			result_is_f32 := g.mod.values[val_id].typ > 0
				&& g.mod.values[val_id].typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				&& g.mod.type_store.types[g.mod.values[val_id].typ].width == 32

			// SCVTF Dd, Xn (convert signed int to double)
			g.emit(asm_scvtf_d_x(0, Reg(src_reg)))

			if result_is_f32 {
				// Convert f64→f32 and move to integer register as 32-bit pattern
				g.emit(asm_fcvt_s_d(0, 0))
				g.emit(asm_fmov_w_s(Reg(dest_reg), 0))
			} else {
				// FMOV Xd, D0 (copy f64 bit pattern to integer reg for storage)
				g.emit(asm_fmov_x_d(Reg(dest_reg), 0))
			}

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.uitofp {
			// Unsigned integer to float conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load integer operand to x8
			src_reg := g.get_operand_reg(instr.operands[0], 8)

			// Check if target is f32
			result_is_f32 := g.mod.values[val_id].typ > 0
				&& g.mod.values[val_id].typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				&& g.mod.type_store.types[g.mod.values[val_id].typ].width == 32

			// UCVTF Dd, Xn (convert unsigned int to double)
			g.emit(asm_ucvtf_d_x(0, Reg(src_reg)))

			if result_is_f32 {
				// Convert f64→f32 and move to integer register as 32-bit pattern
				g.emit(asm_fcvt_s_d(0, 0))
				g.emit(asm_fmov_w_s(Reg(dest_reg), 0))
			} else {
				// FMOV Xd, D0 (copy float bit pattern to integer reg for storage)
				g.emit(asm_fmov_x_d(Reg(dest_reg), 0))
			}

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.fptoui {
			// Float to unsigned integer conversion
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }

			// Load float operand to d0
			g.load_float_operand(instr.operands[0], 0)

			// FCVTZU Xd, Dn (convert to unsigned int, truncate toward zero)
			g.emit(asm_fcvtzu_x_d(Reg(dest_reg), 0))

			if val_id !in g.reg_map {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.add, .sub, .mul, .sdiv, .udiv, .srem, .urem, .and_, .or_, .xor, .shl, .ashr, .lshr, .eq,
		.ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
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
			if op1.kind == .constant && op in [.add, .sub] {
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

			match op {
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
				.udiv {
					g.emit(asm_udiv(Reg(dest_reg), Reg(lhs_reg), Reg(rhs_reg)))
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
				.urem {
					// Unsigned modulo: a % b = a - (a / b) * b
					mut temp_reg := 10
					if lhs_reg == 10 || rhs_reg == 10 {
						temp_reg = 11
						if lhs_reg == 11 || rhs_reg == 11 {
							temp_reg = 12
						}
					}
					g.emit(asm_udiv(Reg(temp_reg), Reg(lhs_reg), Reg(rhs_reg)))
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
				.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
					lhs_typ := g.mod.values[instr.operands[0]].typ
					is_float := lhs_typ > 0 && lhs_typ < g.mod.type_store.types.len
						&& g.mod.type_store.types[lhs_typ].kind == .float_t
					if is_float {
						// Float comparison: load to FP regs, use FCMP
						g.load_float_operand(instr.operands[0], 0) // d0
						g.load_float_operand(instr.operands[1], 1) // d1
						g.emit(asm_fcmp_d(Reg(0), Reg(1)))
					} else {
						// Integer comparison
						// Use 32-bit CMP for i32 operands to preserve sign semantics.
						use_32bit := lhs_typ > 0 && lhs_typ < g.mod.type_store.types.len
							&& g.mod.type_store.types[lhs_typ].kind == .int_t
							&& g.mod.type_store.types[lhs_typ].width == 32
						if use_32bit {
							g.emit(asm_cmp_reg_w(Reg(lhs_reg), Reg(rhs_reg)))
						} else {
							g.emit(asm_cmp_reg(Reg(lhs_reg), Reg(rhs_reg)))
						}
					}

					// CSET Rd, cond (works for both integer and float NZCV flags)
					match op {
						.eq { g.emit(asm_cset_eq(Reg(dest_reg))) }
						.ne { g.emit(asm_cset_ne(Reg(dest_reg))) }
						.lt { g.emit(asm_cset_lt(Reg(dest_reg))) }
						.gt { g.emit(asm_cset_gt(Reg(dest_reg))) }
						.le { g.emit(asm_cset_le(Reg(dest_reg))) }
						.ge { g.emit(asm_cset_ge(Reg(dest_reg))) }
						.ugt { g.emit(asm_cset_hi(Reg(dest_reg))) }
						.uge { g.emit(asm_cset_hs(Reg(dest_reg))) }
						.ult { g.emit(asm_cset_lo(Reg(dest_reg))) }
						.ule { g.emit(asm_cset_ls(Reg(dest_reg))) }
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
			src_id := instr.operands[0]
			ptr_id := instr.operands[1]

			// ValueID 0 is the SSA null/invalid sentinel.
			if src_id <= 0 || src_id >= g.mod.values.len {
				return
			}
			if ptr_id <= 0 || ptr_id >= g.mod.values.len {
				return
			}
			mut src_addr_override_id := 0
			if src_id > 0 && src_id < g.mod.values.len {
				src_val2 := g.mod.values[src_id]
				if src_val2.kind == .instruction {
					src_instr2 := g.mod.instrs[src_val2.index]
					if src_instr2.op == .bitcast && src_instr2.operands.len > 0 {
						bitcast_src := src_instr2.operands[0]
						if bitcast_src > 0 && bitcast_src < g.mod.values.len {
							bitcast_src_val := g.mod.values[bitcast_src]
							if bitcast_src_val.kind == .instruction {
								extract_instr := g.mod.instrs[bitcast_src_val.index]
								if extract_instr.op == .extractvalue
									&& extract_instr.operands.len >= 2 {
									idx_val_id := extract_instr.operands[1]
									if idx_val_id > 0 && idx_val_id < g.mod.values.len {
										idx_val := g.mod.values[idx_val_id]
										if idx_val.kind == .constant && idx_val.name == '0' {
											base_id := extract_instr.operands[0]
											if base_id > 0 && base_id < g.mod.values.len {
												base_val := g.mod.values[base_id]
												if base_val.kind == .instruction {
													load_instr := g.mod.instrs[base_val.index]
													if load_instr.op == .load
														&& load_instr.operands.len > 0 {
														load_src := load_instr.operands[0]
														if load_src > 0
															&& load_src < g.mod.values.len
															&& g.mod.values[load_src].kind == .string_literal {
															// Sumtype string payload lowering can arrive as:
															// bitcast(extractvalue(load(string_literal), 0)).
															// Preserve pointer-to-string-struct, not string.str.
															src_addr_override_id = load_src
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}

			// Check if we're storing a large struct value (> 16 bytes)
			// In this case, the value is a pointer to the struct and we need to copy
			val_val := g.mod.values[src_id]
			val_typ := g.mod.type_store.types[val_val.typ]
			val_size := g.type_size(val_val.typ)
			is_undef_aggregate := val_val.kind == .constant && val_val.name == 'undef'
			src_has_storage := src_id in g.reg_map || src_id in g.stack_map
				|| val_val.kind in [.global, .string_literal]
			mut dst_struct_size := 0
			mut dst_is_large_struct := false
			mut dst_is_small_struct := false
			mut dst_struct_typ_id := ssa.TypeID(0)
			mut dst_elem_is_ptrlike := false
			ptr_val := g.mod.values[ptr_id]
			if ptr_val.typ > 0 && ptr_val.typ < g.mod.type_store.types.len {
				ptr_typ := g.mod.type_store.types[ptr_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
					&& ptr_typ.elem_type < g.mod.type_store.types.len {
					elem_typ := g.mod.type_store.types[ptr_typ.elem_type]
					elem_size := g.type_size(ptr_typ.elem_type)
					if elem_typ.kind in [.ptr_t, .func_t] {
						dst_elem_is_ptrlike = true
					}
					if elem_typ.kind == .struct_t || elem_typ.kind == .array_t {
						dst_struct_typ_id = ptr_typ.elem_type
						if elem_size > 16 {
							dst_is_large_struct = true
							dst_struct_size = elem_size
						} else if elem_size > 0 {
							dst_is_small_struct = true
							dst_struct_size = elem_size
						}
					}
				}
			}
			mut should_zero_large_store := is_undef_aggregate
			if !should_zero_large_store && (dst_is_large_struct
				|| (val_typ.kind in [.struct_t, .array_t] && val_size > 16 && !dst_elem_is_ptrlike))
				&& !src_has_storage {
				should_zero_large_store = true
			}

			// Load source first, then preserve it in a register that will not be clobbered
			// when loading the destination pointer (which may use x9 plus x11/x12 scratch).
			mut val_reg := if src_addr_override_id > 0 {
				g.get_operand_reg(src_addr_override_id, 8)
			} else {
				g.get_operand_reg(src_id, 8)
			}
			if val_reg == 9 || val_reg == 11 || val_reg == 12 {
				if val_reg != 8 {
					g.emit_mov_reg(8, val_reg)
				}
				val_reg = 8
			}
			ptr_reg := g.get_operand_reg(ptr_id, 9)

			if dst_is_large_struct {
				// Destination expects a large struct by value.
				// Large structs are represented as pointers in registers, so copy pointee bytes.
				num_fields := (dst_struct_size + 7) / 8
				if should_zero_large_store {
					g.emit_mov_reg(10, 31)
					for i in 0 .. num_fields {
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else {
					for i in 0 .. num_fields {
						g.emit(asm_ldr_imm(Reg(10), Reg(val_reg), u32(i)))
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				}
			} else if dst_is_small_struct {
				// Destination expects a small multi-field struct by value.
				num_fields := (dst_struct_size + 7) / 8
				mut src_points_to_struct := false
				if dst_struct_typ_id > 0 && val_typ.kind == .ptr_t && val_typ.elem_type > 0
					&& val_typ.elem_type < g.mod.type_store.types.len {
					src_elem_typ := g.mod.type_store.types[val_typ.elem_type]
					src_elem_size := g.type_size(val_typ.elem_type)
					if val_typ.elem_type == dst_struct_typ_id
						|| (src_elem_typ.kind == .struct_t && src_elem_size == dst_struct_size) {
						src_points_to_struct = true
					}
				}
				if !src_has_storage && !src_points_to_struct {
					g.emit_mov_reg(10, 31)
					for i in 0 .. num_fields {
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else {
					mut can_copy_from_src_ptr := false
					mut src_ptr_reg := 11
					if src_points_to_struct {
						if val_reg != src_ptr_reg {
							g.emit_mov_reg(src_ptr_reg, val_reg)
						}
						can_copy_from_src_ptr = true
					} else if src_off := g.stack_map[src_id] {
						g.emit_add_fp_imm(src_ptr_reg, src_off)
						can_copy_from_src_ptr = true
					}
					if can_copy_from_src_ptr {
						for i in 0 .. num_fields {
							g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(i)))
							g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
						}
					} else if num_fields == 1 {
						// Single-slot struct values in registers can be stored directly.
						g.emit(asm_str(Reg(val_reg), Reg(ptr_reg)))
					} else if val_typ.kind in [.struct_t, .array_t] && src_id > 0
						&& src_id < g.mod.values.len {
						// Source is a struct/array value whose register holds a pointer
						// (e.g., from a .load of a struct without a stack slot).
						// Use the register as a source pointer for field-by-field copy.
						src_val_info := g.mod.values[src_id]
						if src_val_info.kind == .instruction
							&& g.mod.instrs[src_val_info.index].op == .load {
							if val_reg != 11 {
								g.emit_mov_reg(11, val_reg)
							}
							for i in 0 .. num_fields {
								g.emit(asm_ldr_imm(Reg(10), Reg(11), u32(i)))
								g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
							}
						} else {
							g.emit_mov_reg(10, 31)
							for i in 0 .. num_fields {
								g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
							}
						}
					} else {
						// Keep behavior deterministic when aggregate source bytes are unavailable.
						g.emit_mov_reg(10, 31)
						for i in 0 .. num_fields {
							g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
						}
					}
				}
			} else if dst_struct_typ_id > 0 && val_typ.kind in [.struct_t, .array_t]
				&& val_size > 16 && !dst_elem_is_ptrlike {
				// Large struct source with non-pointer destination slot:
				// copy pointee bytes into destination memory.
				num_fields := (val_size + 7) / 8
				if should_zero_large_store {
					g.emit_mov_reg(10, 31)
					for i in 0 .. num_fields {
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else {
					for i in 0 .. num_fields {
						// LDR x10, [val_reg, #i*8]
						g.emit(asm_ldr_imm(Reg(10), Reg(val_reg), u32(i)))
						// STR x10, [ptr_reg, #i*8]
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				}
			} else if val_typ.kind == .struct_t && val_typ.fields.len > 1 && val_size <= 16
				&& !dst_elem_is_ptrlike {
				// Small multi-field struct: copy all fields by value.
				num_fields := val_typ.fields.len
				if !src_has_storage {
					g.emit_mov_reg(10, 31)
					for i in 0 .. num_fields {
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else if src_off := g.stack_map[src_id] {
					for i in 0 .. num_fields {
						g.emit_ldr_reg_offset(10, 29, src_off + i * 8)
						g.emit(asm_str_imm(Reg(10), Reg(ptr_reg), u32(i)))
					}
				} else {
					// Fallback: preserve previous scalar behavior when no structured source exists.
					g.emit(asm_str(Reg(val_reg), Reg(ptr_reg)))
				}
			} else {
				store_size := g.mem_access_size_bytes(val_val.typ, ptr_id)
				match store_size {
					1 { g.emit(asm_str_b(Reg(val_reg), Reg(ptr_reg))) }
					2 { g.emit(asm_str_h(Reg(val_reg), Reg(ptr_reg))) }
					4 { g.emit(asm_str_w(Reg(val_reg), Reg(ptr_reg))) }
					else { g.emit(asm_str(Reg(val_reg), Reg(ptr_reg))) }
				}
			}
		}
		.load {
			dest_reg := if r := g.reg_map[val_id] { r } else { 8 }
			ptr_id := instr.operands[0]
			mut loaded_into_aggregate_slot := false
			mut force_spill_small_struct := false

			// ValueID 0 is the SSA null/invalid sentinel.
			if ptr_id <= 0 || ptr_id >= g.mod.values.len {
				g.emit_mov_imm64(dest_reg, 0)
			} else {
				ptr_reg := g.get_operand_reg(ptr_id, 9)
				result_typ_id := g.mod.values[val_id].typ
				if result_typ_id > 0 && result_typ_id < g.mod.type_store.types.len {
					result_typ := g.mod.type_store.types[result_typ_id]
					result_size := g.type_size(result_typ_id)
					if (result_typ.kind == .struct_t || result_typ.kind == .array_t)
						&& result_size > 8 && result_size <= 16 {
						if result_offset := g.stack_map[val_id] {
							num_chunks := (result_size + 7) / 8
							for i in 0 .. num_chunks {
								g.emit(asm_ldr_imm(Reg(10), Reg(ptr_reg), u32(i)))
								g.emit_str_reg_offset(10, 29, result_offset + i * 8)
							}
							loaded_into_aggregate_slot = true
						} else if dest_reg != ptr_reg {
							// Fallback when no aggregate slot is available.
							g.emit_mov_reg(dest_reg, ptr_reg)
						}
					} else if (result_typ.kind == .struct_t || result_typ.kind == .array_t)
						&& result_size > 16 {
						if result_offset := g.stack_map[val_id] {
							// Materialize large load results by value in their stack slot.
							num_chunks := (result_size + 7) / 8
							for i in 0 .. num_chunks {
								g.emit(asm_ldr_imm(Reg(10), Reg(ptr_reg), u32(i)))
								g.emit_str_reg_offset(10, 29, result_offset + i * 8)
							}
							if val_id in g.reg_map {
								g.emit_add_fp_imm(dest_reg, result_offset)
							}
							loaded_into_aggregate_slot = true
						} else if dest_reg != ptr_reg {
							// Fallback when no spill slot is available: keep address form.
							g.emit_mov_reg(dest_reg, ptr_reg)
						}
					} else {
						load_size := g.mem_access_size_bytes(result_typ_id, ptr_id)
						match load_size {
							1 { g.emit(asm_ldr_b(Reg(dest_reg), Reg(ptr_reg))) }
							2 { g.emit(asm_ldr_h(Reg(dest_reg), Reg(ptr_reg))) }
							4 { g.emit(asm_ldr_w(Reg(dest_reg), Reg(ptr_reg))) }
							else { g.emit(asm_ldr(Reg(dest_reg), Reg(ptr_reg))) }
						}
						if result_typ.kind == .struct_t && result_size <= 8 && val_id in g.stack_map {
							force_spill_small_struct = true
						}
					}
				} else {
					g.emit(asm_ldr(Reg(dest_reg), Reg(ptr_reg)))
				}
			}

			if !loaded_into_aggregate_slot && (val_id !in g.reg_map || force_spill_small_struct) {
				g.store_reg_to_val(dest_reg, val_id)
			}
		}
		.alloca {
			data_off := g.alloca_offsets[val_id]
			g.emit_add_fp_imm(8, data_off)
			g.store_reg_to_val(8, val_id)
		}
		.heap_alloc {
			// Heap-allocate memory for a struct type.
			// Result type is ptr(T), compute sizeof(T) and call calloc(1, size).
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
			// calloc(1, size) → x0 = 1, x1 = size
			g.emit_mov_imm(0, 1)
			g.emit_mov_imm(1, u64(alloc_size))
			sym_idx := g.macho.add_undefined('_calloc')
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
			g.emit(asm_bl_reloc())
			// calloc returns heap pointer in x0
			g.store_reg_to_val(0, val_id)
		}
		.get_element_ptr {
			// GEP: Base + scaled index (or struct field offset for aggregate pointers)
			base_reg := g.get_operand_reg(instr.operands[0], 8)
			idx_id := instr.operands[1]
			base_typ_id := g.mod.values[instr.operands[0]].typ
			mut pointee_typ_id := ssa.TypeID(0)
			if base_typ_id > 0 && base_typ_id < g.mod.type_store.types.len {
				base_typ := g.mod.type_store.types[base_typ_id]
				if base_typ.kind == .ptr_t {
					pointee_typ_id = base_typ.elem_type
				}
			}

			// Struct field GEP with constant index: use real field byte offsets.
			// Distinguish from array-style GEP: if the GEP result type equals the
			// base pointer type, this is array indexing (ptr(struct)[i] → ptr(struct)),
			// not struct field access (ptr(struct), field_idx → ptr(field_type)).
			mut is_array_gep := false
			base_val_typ := g.mod.values[instr.operands[0]].typ
			if instr.typ == base_val_typ {
				is_array_gep = true
			}
			if !is_array_gep && idx_id > 0 && idx_id < g.mod.values.len && pointee_typ_id > 0
				&& pointee_typ_id < g.mod.type_store.types.len {
				idx_val := g.mod.values[idx_id]
				pointee_typ := g.mod.type_store.types[pointee_typ_id]
				if idx_val.kind == .constant && pointee_typ.kind == .struct_t {
					field_idx := idx_val.name.int()
					field_off := g.struct_field_offset_bytes(pointee_typ_id, field_idx)
					if field_off <= 0xFFF {
						g.emit(asm_add_imm(Reg(8), Reg(base_reg), u32(field_off)))
					} else {
						g.emit_mov_imm64(9, i64(field_off))
						g.emit(asm_add_reg(Reg(8), Reg(base_reg), Reg(9)))
					}
					g.store_reg_to_val(8, val_id)
					return
				}
			}

			// Array/pointer-style GEP: scale by element size.
			mut scale := 8
			mut base_ptr_reg := base_reg
			if pointee_typ_id > 0 && pointee_typ_id < g.mod.type_store.types.len {
				pointee_typ := g.mod.type_store.types[pointee_typ_id]
				elem_size := if pointee_typ.kind == .array_t {
					g.type_size(pointee_typ.elem_type)
				} else {
					g.type_size(pointee_typ_id)
				}
				if elem_size > 0 {
					scale = elem_size
				}
			}
			// Ensure index load doesn't clobber base if base is 8
			idx_scratch := if base_ptr_reg == 8 { 9 } else { 8 }
			idx_reg := g.get_operand_reg(idx_id, idx_scratch)
			if scale == 8 {
				g.emit(asm_add_reg_lsl3(Reg(8), Reg(base_ptr_reg), Reg(idx_reg)))
			} else if scale == 1 {
				g.emit(asm_add_reg(Reg(8), Reg(base_ptr_reg), Reg(idx_reg)))
			} else {
				mut scale_reg := 10
				if scale_reg == base_ptr_reg || scale_reg == idx_reg {
					scale_reg = 11
					if scale_reg == base_ptr_reg || scale_reg == idx_reg {
						scale_reg = 12
					}
				}
				g.emit_mov_imm64(scale_reg, scale)
				g.emit(asm_mul(Reg(scale_reg), Reg(idx_reg), Reg(scale_reg)))
				g.emit(asm_add_reg(Reg(8), Reg(base_ptr_reg), Reg(scale_reg)))
			}

			g.store_reg_to_val(8, val_id)
		}
		.call {
			fn_val := g.mod.values[instr.operands[0]]
			fn_name := fn_val.name

			// Skip calls with empty function names (shouldn't happen, but safety check)
			if fn_name != '' {
				// On ARM64 macOS (Apple Silicon), variadic arguments must be
				// passed on the stack, not in registers.
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

				// Check if return type is a large struct (> 16 bytes) requiring indirect return
				result_typ := g.mod.type_store.types[g.mod.values[val_id].typ]
				result_size := g.type_size(g.mod.values[val_id].typ)
				is_indirect_return := result_typ.kind == .struct_t && result_size > 16

				// For indirect struct returns, set x8 to point to result storage BEFORE the call
				if is_indirect_return {
					result_offset := g.stack_map[val_id]
					g.emit_add_fp_imm(8, result_offset)
				}

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
						g.load_call_arg_to_reg(9, instr.operands[arg_idx], arg_idx - 1,
							instr) // Use x9 to avoid clobbering x8
						// STR x9, [sp, #offset]
						offset := i * 8
						imm12 := u32(offset / 8)
						g.emit(asm_str_imm(Reg(9), sp, imm12))
					}

					// Load fixed arguments to registers (in reverse order to avoid clobbering)
					for i := num_fixed_args; i >= 1; i-- {
						g.load_call_arg_to_reg(i - 1, instr.operands[i], i - 1, instr)
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
					// Non-variadic call:
					// ARM64 ABI: integer args in x0-x7, float args in d0-d7
					// Integer and float registers are allocated independently.

					// Classify each argument as float or integer
					mut is_float_arg := []bool{len: num_args}
					mut arg_int_reg := []int{len: num_args, init: -1}
					mut arg_float_reg := []int{len: num_args, init: -1}
					mut arg_int_cnt := []int{len: num_args}
					mut int_reg_idx := 0
					mut float_reg_idx := 0

					for a in 0 .. num_args {
						arg_val := g.mod.values[instr.operands[a + 1]]
						mut is_float := false
						if arg_val.typ > 0 && int(arg_val.typ) < g.mod.type_store.types.len {
							arg_typ := g.mod.type_store.types[arg_val.typ]
							if arg_typ.kind == .float_t {
								is_float = true
							}
						}
						if is_float {
							is_float_arg[a] = true
							arg_float_reg[a] = float_reg_idx
							float_reg_idx++
						} else {
							cnt := g.call_arg_reg_count(instr.operands[a + 1], a, instr)
							arg_int_reg[a] = int_reg_idx
							arg_int_cnt[a] = cnt
							int_reg_idx += cnt
						}
					}

					// Handle stack-spilled integer args (>8 int regs)
					num_int_stack := if int_reg_idx > 8 { int_reg_idx - 8 } else { 0 }
					num_float_stack := if float_reg_idx > 8 { float_reg_idx - 8 } else { 0 }
					total_stack_slots := num_int_stack + num_float_stack
					stack_space := ((total_stack_slots * 8) + 15) & ~0xF
					if stack_space > 0 {
						g.emit_sub_sp(stack_space)
						mut stack_idx := 0
						for a in 0 .. num_args {
							if is_float_arg[a] {
								if arg_float_reg[a] >= 8 {
									g.load_val_to_reg(9, instr.operands[a + 1])
									g.emit(asm_str_imm(Reg(9), sp, u32(stack_idx)))
									stack_idx++
								}
							} else {
								if arg_int_reg[a] >= 8 {
									cnt := arg_int_cnt[a]
									for ri in 0 .. cnt {
										if cnt > 1 {
											g.load_address_of_val_to_reg(9, instr.operands[a + 1])
											g.emit(asm_ldr_imm(Reg(9), Reg(9), u32(ri)))
										} else {
											g.load_call_arg_to_reg(9, instr.operands[a + 1],
												a, instr)
										}
										g.emit(asm_str_imm(Reg(9), sp, u32(stack_idx)))
										stack_idx++
									}
								}
							}
						}
					}

					// Load integer args to x-registers (reverse order)
					for a := num_args - 1; a >= 0; a-- {
						if is_float_arg[a] {
							continue
						}
						reg := arg_int_reg[a]
						if reg >= 8 {
							continue // stack arg
						}
						if arg_int_cnt[a] == 2 {
							g.load_struct_arg_to_regs(reg, instr.operands[a + 1])
						} else {
							g.load_call_arg_to_reg(reg, instr.operands[a + 1], a, instr)
						}
					}

					// Load float args to d-registers
					for a in 0 .. num_args {
						if !is_float_arg[a] {
							continue
						}
						freg := arg_float_reg[a]
						if freg >= 8 {
							continue // stack float arg
						}
						// Load value bits to x9, then fmov to dN
						g.load_val_to_reg(9, instr.operands[a + 1])
						g.emit(asm_fmov_d_x(freg, Reg(9)))
					}

					sym_idx := g.macho.add_undefined('_' + fn_name)
					g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
						true)
					g.emit(asm_bl_reloc())

					if stack_space > 0 {
						if stack_space <= 0xFFF {
							g.emit(asm_add_imm(sp, sp, u32(stack_space)))
						} else {
							g.emit_mov_imm(10, u64(stack_space))
							g.emit(asm_add_sp_reg(Reg(10)))
						}
					}
				}

				if result_typ.kind != .void_t {
					// Check if this is a float return: result comes in d0 instead of x0
					mut is_float_return := result_typ.kind == .float_t
					if !is_float_return {
						// Also check the callee's registered return type
						callee_fn_val := g.mod.values[instr.operands[0]]
						if callee_fn_val.kind == .func_ref {
							for f in g.mod.funcs {
								if f.name == callee_fn_val.name {
									callee_ret := g.mod.type_store.types[f.typ]
									if callee_ret.kind == .float_t {
										is_float_return = true
									}
									break
								}
							}
						}
					}
					if is_float_return {
						// Float return: move d0 to x0 for integer storage
						g.emit(asm_fmov_x_d(Reg(0), 0))
						g.store_reg_to_val(0, val_id)
					} else {
						// Also check callee's registered return type: when the SSA value type
						// isn't struct_t (e.g. i64 fallback), use the callee's return type.
						mut call_ret_is_multi_reg := result_typ.kind == .struct_t && result_size > 8
						mut actual_call_ret_size := result_size
						if !call_ret_is_multi_reg && !is_indirect_return {
							callee_val2 := g.mod.values[instr.operands[0]]
							if callee_val2.kind == .func_ref {
								for f in g.mod.funcs {
									if f.name == callee_val2.name {
										callee_ret_typ := g.mod.type_store.types[f.typ]
										callee_ret_size := g.type_size(f.typ)
										if callee_ret_typ.kind == .struct_t && callee_ret_size > 8
											&& callee_ret_size <= 16 {
											call_ret_is_multi_reg = true
											actual_call_ret_size = callee_ret_size
										}
										break
									}
								}
							}
						}
						if call_ret_is_multi_reg {
							if !is_indirect_return {
								if val_id in g.stack_map {
									result_offset := g.stack_map[val_id]
									num_chunks := (actual_call_ret_size + 7) / 8
									for i in 0 .. num_chunks {
										if i < 8 {
											g.emit_str_reg_offset(i, 29, result_offset + i * 8)
										}
									}
								} else {
									g.store_reg_to_val(0, val_id)
								}
							}
						} else {
							g.canonicalize_narrow_int_result(0, g.mod.values[val_id].typ)
							g.store_reg_to_val(0, val_id)
						}
					}
				}
			}
		}
		.call_indirect {
			// Indirect call through function pointer
			// operands[0] is the function pointer, rest are arguments
			num_args := instr.operands.len - 1

			// Compute register mapping for multi-register struct args.
			mut ci_arg_reg_start := []int{len: num_args}
			mut ci_arg_reg_cnt := []int{len: num_args}
			mut ci_total_reg_slots := 0
			for a in 0 .. num_args {
				ci_arg_reg_start[a] = ci_total_reg_slots
				cnt := g.call_arg_reg_count(instr.operands[a + 1], a, instr)
				ci_arg_reg_cnt[a] = cnt
				ci_total_reg_slots += cnt
			}
			num_stack_slots := if ci_total_reg_slots > 8 {
				ci_total_reg_slots - 8
			} else {
				0
			}
			stack_space := ((num_stack_slots * 8) + 15) & ~0xF
			if stack_space > 0 {
				g.emit_sub_sp(stack_space)
				mut stack_idx := 0
				for a in 0 .. num_args {
					if ci_arg_reg_start[a] >= 8 {
						cnt := ci_arg_reg_cnt[a]
						for ri in 0 .. cnt {
							if cnt > 1 {
								g.load_address_of_val_to_reg(9, instr.operands[a + 1])
								g.emit(asm_ldr_imm(Reg(9), Reg(9), u32(ri)))
							} else {
								g.load_call_arg_to_reg(9, instr.operands[a + 1], a, instr)
							}
							imm12 := u32(stack_idx)
							g.emit(asm_str_imm(Reg(9), sp, imm12))
							stack_idx++
						}
					}
				}
			}

			for a := num_args - 1; a >= 0; a-- {
				reg := ci_arg_reg_start[a]
				if reg >= 8 {
					continue
				}
				if ci_arg_reg_cnt[a] == 2 {
					g.load_struct_arg_to_regs(reg, instr.operands[a + 1])
				} else {
					g.load_call_arg_to_reg(reg, instr.operands[a + 1], a, instr)
				}
			}

			// Load function pointer to x9 (scratch register).
			// Do not use generic value loading here: it can materialize an address
			// for large struct-like values instead of the actual callable pointer.
			g.load_fnptr_to_reg(9, instr.operands[0])

			// BLR x9 - branch and link to register
			g.emit(asm_blr(Reg(9)))

			if stack_space > 0 {
				if stack_space <= 0xFFF {
					g.emit(asm_add_imm(sp, sp, u32(stack_space)))
				} else {
					g.emit_mov_imm(10, u64(stack_space))
					g.emit(asm_add_sp_reg(Reg(10)))
				}
			}

			ci_result_typ_id := g.mod.values[val_id].typ
			ci_result_typ := g.mod.type_store.types[ci_result_typ_id]
			if ci_result_typ.kind != .void_t {
				ci_result_size := g.type_size(ci_result_typ_id)
				if ci_result_typ.kind == .struct_t && ci_result_size > 8 {
					// Small struct return: store x0, x1 into stack slot
					ci_result_offset := g.stack_map[val_id]
					num_chunks := (ci_result_size + 7) / 8
					for i in 0 .. num_chunks {
						if i < 8 {
							g.emit_str_reg_offset(i, 29, ci_result_offset + i * 8)
						}
					}
				} else {
					g.canonicalize_narrow_int_result(0, ci_result_typ_id)
					g.store_reg_to_val(0, val_id)
				}
			}
		}
		.call_sret {
			// Call with struct return lowered by ABI pass.
			// operands: [fn, arg1, arg2, ...], destination is val_id's stack slot.
			num_args := instr.operands.len - 1

			// Set x8 to destination address for indirect return.
			result_offset := g.stack_map[val_id]
			g.emit_add_fp_imm(8, result_offset)

			// Compute register mapping for multi-register struct args.
			mut sr_arg_reg_start := []int{len: num_args}
			mut sr_arg_reg_cnt := []int{len: num_args}
			mut sr_total_reg_slots := 0
			for a in 0 .. num_args {
				sr_arg_reg_start[a] = sr_total_reg_slots
				cnt := g.call_arg_reg_count(instr.operands[a + 1], a, instr)
				sr_arg_reg_cnt[a] = cnt
				sr_total_reg_slots += cnt
			}
			sr_num_stack_slots := if sr_total_reg_slots > 8 {
				sr_total_reg_slots - 8
			} else {
				0
			}
			stack_space := ((sr_num_stack_slots * 8) + 15) & ~0xF
			if stack_space > 0 {
				g.emit_sub_sp(stack_space)
				mut stack_idx := 0
				for a in 0 .. num_args {
					if sr_arg_reg_start[a] >= 8 {
						cnt := sr_arg_reg_cnt[a]
						for ri in 0 .. cnt {
							if cnt > 1 {
								g.load_address_of_val_to_reg(9, instr.operands[a + 1])
								g.emit(asm_ldr_imm(Reg(9), Reg(9), u32(ri)))
							} else {
								g.load_call_arg_to_reg(9, instr.operands[a + 1], a, instr)
							}
							imm12 := u32(stack_idx)
							g.emit(asm_str_imm(Reg(9), sp, imm12))
							stack_idx++
						}
					}
				}
			}

			for a := num_args - 1; a >= 0; a-- {
				reg := sr_arg_reg_start[a]
				if reg >= 8 {
					continue
				}
				if sr_arg_reg_cnt[a] == 2 {
					g.load_struct_arg_to_regs(reg, instr.operands[a + 1])
				} else {
					g.load_call_arg_to_reg(reg, instr.operands[a + 1], a, instr)
				}
			}

			fn_val := g.mod.values[instr.operands[0]]
			if fn_val.name != '' && fn_val.kind in [.unknown, .func_ref] {
				// Direct call by symbol.
				sym_idx := g.macho.add_undefined('_' + fn_val.name)
				g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26,
					true)
				g.emit(asm_bl_reloc())
			} else {
				// Indirect call through function pointer value.
				g.load_fnptr_to_reg(9, instr.operands[0])
				g.emit(asm_blr(Reg(9)))
			}

			if stack_space > 0 {
				if stack_space <= 0xFFF {
					g.emit(asm_add_imm(sp, sp, u32(stack_space)))
				} else {
					g.emit_mov_imm(10, u64(stack_space))
					g.emit(asm_add_sp_reg(Reg(10)))
				}
			}
		}
		.ret {
			if instr.operands.len > 0 {
				ret_val_id := instr.operands[0]
				ret_val := g.mod.values[ret_val_id]
				ret_typ := g.mod.type_store.types[ret_val.typ]

				// Get the function's declared return type
				fn_ret_type := g.cur_func_ret_type
				fn_ret_typ := g.mod.type_store.types[fn_ret_type]
				fn_ret_size := g.type_size(fn_ret_type)

				// Detect type mismatch: ret value type is larger struct than fn return type
				// This happens when transformer fails to wrap a variant in a sum type
				ret_val_size := g.type_size(ret_val.typ)
				if ret_typ.kind == .struct_t && fn_ret_typ.kind == .struct_t
					&& ret_val_size > fn_ret_size && fn_ret_size > 0 && fn_ret_size <= 16 {
					mut ret_instr_op := ''
					mut ret_instr_detail := ''
					if ret_val.kind == .instruction {
						ri := g.mod.instrs[ret_val.index]
						ret_instr_op = '${ri.op}'
						if ri.op == .call_sret || ri.op == .call {
							if ri.operands.len > 0 {
								ret_instr_detail = g.mod.values[ri.operands[0]].name
							}
						}
					}
					eprintln('MISMATCH .ret: fn=${g.cur_func_name} ret_val_size=${ret_val_size} fn_ret_size=${fn_ret_size} ret_val_kind=${ret_val.kind} ret_instr=${ret_instr_op} callee=${ret_instr_detail} ret_typ_fields=${ret_typ.fields.len}')
				}

				// Check if we're returning a pointer but the function expects a struct
				// This happens when returning local struct variables (expr_init returns pointers)
				mut is_indirect_struct_return := false
				if ret_typ.kind == .ptr_t && fn_ret_typ.kind == .struct_t {
					elem_type := ret_typ.elem_type
					if elem_type == fn_ret_type {
						is_indirect_struct_return = true
					}
				}

				// For large struct returns (> 16 bytes), use indirect return via x8
				// The caller provides the destination address in x8
				if fn_ret_typ.kind == .struct_t && fn_ret_size > 16 {
					// Restore x8 from the saved location (fp-relative)
					if g.x8_save_offset != 0 {
						g.emit_ldr_reg_offset(8, 29, g.x8_save_offset)
					}

					// Check if returning a zero/none value (e.g., `return 0` from `return none`).
					// In this case, zero-fill the return area instead of trying to copy
					// from address 0 (which would be a null pointer dereference).
					is_zero_const := ret_val.kind == .constant && ret_val.name == '0'
					if is_zero_const {
						num_fields := (fn_ret_size + 7) / 8
						for i in 0 .. num_fields {
							// STR xzr, [x8, #i*8]
							g.emit(asm_str_imm(Reg(31), Reg(8), u32(i)))
						}
					} else {
						// string_literal values need to be materialized on the stack
						// before we can copy them to the return pointer.
						if ret_val.kind == .string_literal {
							g.load_val_to_reg(9, ret_val_id)
						}

						// Get the source address of the struct
						if is_indirect_struct_return {
							// Return value is a pointer to struct - use it as source
							g.load_val_to_reg(9, ret_val_id)
						} else if ret_offset := g.stack_map[ret_val_id] {
							if g.large_struct_stack_value_is_pointer(ret_val_id) {
								// Some large-struct temporaries are represented as pointers in stack slots.
								g.emit_ldr_reg_offset(9, 29, ret_offset)
							} else {
								// Struct is materialized by value on stack.
								g.emit_add_fp_imm(9, ret_offset)
							}
						} else {
							// Fallback
							g.load_val_to_reg(9, ret_val_id)
						}
						// Copy struct from [x9] to [x8] (x8 was restored from saved location)
						num_fields := (fn_ret_size + 7) / 8
						for i in 0 .. num_fields {
							// LDR x10, [x9, #i*8]
							g.emit(asm_ldr_imm(Reg(10), Reg(9), u32(i)))
							// STR x10, [x8, #i*8]
							g.emit(asm_str_imm(Reg(10), Reg(8), u32(i)))
						}
					}
				} else if (ret_typ.kind == .struct_t && g.type_size(ret_val.typ) > 8)
					|| is_indirect_struct_return
					|| (fn_ret_typ.kind == .struct_t && fn_ret_size > 8 && fn_ret_size <= 16) {
					// Small struct (≤ 16 bytes) - return in registers x0, x1
					// Use type size (not field count) to determine multi-register returns,
					// since a struct with 1 nested struct field can still span 2 registers.
					// Also handle the case where the SSA value's type doesn't match the
					// function's return type (e.g., after PHI node type merging).
					actual_struct_typ_id := if is_indirect_struct_return
						|| ret_typ.kind != .struct_t {
						fn_ret_type
					} else {
						ret_val.typ
					}
					actual_struct_size := g.type_size(actual_struct_typ_id)
					num_chunks := (actual_struct_size + 7) / 8

					// Ensure string literals are materialized on the stack
					// before we try to load their fields into return registers.
					if ret_val.kind == .string_literal {
						g.load_val_to_reg(9, ret_val_id)
					}

					if is_indirect_struct_return {
						// Return value is a pointer to struct - load each field via the pointer
						g.load_val_to_reg(8, ret_val_id)
						for i in 0 .. num_chunks {
							if i < 8 {
								g.emit(asm_ldr_imm(Reg(i), Reg(8), u32(i)))
							}
						}
					} else if ret_offset := g.stack_map[ret_val_id] {
						for i in 0 .. num_chunks {
							if i < 8 {
								g.emit_ldr_reg_offset(i, 29, ret_offset + i * 8)
							}
						}
					} else {
						g.load_val_to_reg(0, ret_val_id)
					}
				} else if fn_ret_typ.kind == .struct_t && ret_val.kind == .constant
					&& ret_val.name == '0' {
					// Returning zero/none from a function that returns a small struct.
					// Zero all return registers for the struct to avoid garbage in x1+.
					num_ret_chunks := (fn_ret_size + 7) / 8
					for i in 0 .. num_ret_chunks {
						if i < 8 {
							g.emit_mov_reg(i, 31) // xN = xzr (zero)
						}
					}
				} else {
					g.load_val_to_reg(0, ret_val_id)
				}
				// For float return types, move result from x0 to d0
				if fn_ret_typ.kind == .float_t {
					g.emit(asm_fmov_d_x(0, Reg(0))) // fmov d0, x0
				}
			}
			if g.mod.type_store.types[g.cur_func_ret_type].kind == .void_t {
				// Keep void returns deterministic; process entry uses x0 as exit status.
				g.emit_mov_reg(0, 31)
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
			// Load condition value into x8 for branch.
			// For large structs (> 16 bytes), load_val_to_reg returns the *address*
			// (which is always non-zero). For truthiness checks on option struct returns,
			// we need to load the first word of the struct instead.
			cond_val := g.mod.values[instr.operands[0]]
			cond_is_large_struct := cond_val.typ > 0 && cond_val.typ < g.mod.type_store.types.len
				&& g.mod.type_store.types[cond_val.typ].kind == .struct_t
				&& g.type_size(cond_val.typ) > 16
			if cond_is_large_struct {
				// Large struct: load the address, then dereference first word
				g.load_val_to_reg(8, instr.operands[0])
				g.emit(asm_ldr_imm(Reg(8), Reg(8), 0)) // x8 = [x8] (first word)
			} else {
				g.load_val_to_reg(8, instr.operands[0])
			}

			true_blk := g.mod.values[instr.operands[1]].index
			false_blk := g.mod.values[instr.operands[2]].index

			// Fallthrough optimization for False block
			// If false block is next, we only need CBNZ to true block

			if off := g.block_offsets[true_blk] {
				rel := (off - (g.macho.text_data.len - g.curr_offset)) / 4
				if rel >= -262144 && rel < 262144 {
					g.emit(asm_cbnz(Reg(8), rel))
				} else {
					// Branch target too far for CBNZ (19-bit range).
					// Use trampoline: CBZ skip; B target; skip:
					g.emit(asm_cbz(Reg(8), 2)) // skip over next B instruction
					g.emit(asm_b(rel - 1)) // adjust for the extra CBZ instruction
				}
			} else {
				// Forward reference: use trampoline pattern to avoid 19-bit overflow.
				// CBZ x8, skip; B target; skip:
				g.emit(asm_cbz(Reg(8), 2)) // skip over next B instruction
				g.record_pending_label(true_blk)
				g.emit(asm_b(0))
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
					if rel >= -262144 && rel < 262144 {
						g.emit(asm_b_cond(cond_eq, rel))
					} else {
						// Trampoline: b.ne skip; B target; skip:
						g.emit(asm_b_cond(cond_ne, 2)) // skip over next B
						g.emit(asm_b(rel - 1))
					}
				} else {
					// Forward reference: use trampoline for safety
					g.emit(asm_b_cond(cond_ne, 2)) // skip over next B
					g.record_pending_label(target_blk_idx)
					g.emit(asm_b(0))
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
		.trunc, .zext {
			if instr.operands.len > 0 {
				// Check if this is a float-to-float conversion
				src_val := g.mod.values[instr.operands[0]]
				src_is_float := src_val.typ > 0 && src_val.typ < g.mod.type_store.types.len
					&& g.mod.type_store.types[src_val.typ].kind == .float_t
				dst_is_float := g.mod.values[val_id].typ > 0
					&& g.mod.values[val_id].typ < g.mod.type_store.types.len
					&& g.mod.type_store.types[g.mod.values[val_id].typ].kind == .float_t
				if src_is_float && dst_is_float {
					dest_reg := if r := g.reg_map[val_id] { r } else { 8 }
					if instr.op == .trunc {
						// f64 → f32: load f64 into d0, convert to s0, move bits to int reg
						g.load_float_operand(instr.operands[0], 0)
						g.emit(asm_fcvt_s_d(0, 0))
						g.emit(asm_fmov_w_s(Reg(dest_reg), 0))
					} else {
						// f32 → f64: load_float_operand already widens f32→f64
						g.load_float_operand(instr.operands[0], 0)
						g.emit(asm_fmov_x_d(Reg(dest_reg), 0))
					}
					if val_id !in g.reg_map {
						g.store_reg_to_val(dest_reg, val_id)
					}
				} else {
					// Integer conversions: just copy (registers are 64-bit)
					g.load_val_to_reg(8, instr.operands[0])
					g.store_reg_to_val(8, val_id)
				}
			}
		}
		.bitcast, .sext {
			// For arm64: all registers are 64-bit, so integer type conversions
			// are mostly just copies. Sign extension would matter for 8/16 bit
			// values but we operate on full 64-bit registers throughout.
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
			mut handled_aggregate_copy := false
			if dest_id > 0 && dest_id < g.mod.values.len {
				dest_typ_id := g.mod.values[dest_id].typ
				if dest_typ_id > 0 && dest_typ_id < g.mod.type_store.types.len {
					dest_typ := g.mod.type_store.types[dest_typ_id]
					dest_size := g.type_size(dest_typ_id)
					if dest_typ.kind == .struct_t && dest_size > 8 {
						if dest_off := g.stack_map[dest_id] {
							num_chunks := (dest_size + 7) / 8
							// Use x12 as source pointer: emit_str_reg_offset can clobber x11.
							mut src_ptr_reg := 12
							mut can_copy := false
							if src_id > 0 && src_id < g.mod.values.len {
								src_typ_id := g.mod.values[src_id].typ
								if src_typ_id > 0 && src_typ_id < g.mod.type_store.types.len {
									src_typ := g.mod.type_store.types[src_typ_id]
									// Aggregate sources in registers/slots are often address-typed
									// (including bitcasted pointers). Treat pointer sources as
									// source addresses for by-value struct copies.
									if src_typ.kind == .ptr_t {
										if src_reg := g.reg_map[src_id] {
											src_ptr_reg = src_reg
										} else if src_off := g.stack_map[src_id] {
											g.emit_ldr_reg_offset(src_ptr_reg, 29, src_off)
										} else {
											g.load_val_to_reg(src_ptr_reg, src_id)
										}
										can_copy = true
									}
								}
							}
							if !can_copy {
								if src_off := g.stack_map[src_id] {
									// Materialize string_literal values before
									// reading from their stack slot.
									if src_id > 0 && src_id < g.mod.values.len
										&& g.mod.values[src_id].kind == .string_literal
										&& src_id !in g.string_literal_offsets {
										g.load_val_to_reg(src_ptr_reg, src_id)
									}
									if dest_size > 16
										&& g.large_aggregate_stack_value_is_pointer(src_id) {
										g.emit_ldr_reg_offset(src_ptr_reg, 29, src_off)
									} else {
										g.emit_add_fp_imm(src_ptr_reg, src_off)
									}
									can_copy = true
								}
							}
							if can_copy {
								// Determine how many chunks the source actually has
								mut src_chunks := num_chunks
								if src_id > 0 && src_id < g.mod.values.len {
									src_sz := g.type_size(g.mod.values[src_id].typ)
									if src_sz > 0 && src_sz < dest_size {
										src_chunks = (src_sz + 7) / 8
									}
								}
								for i in 0 .. src_chunks {
									g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(i)))
									g.emit_str_reg_offset(10, 29, dest_off + i * 8)
								}
								// Zero-fill remaining chunks if source is smaller
								if src_chunks < num_chunks {
									g.emit_mov_reg(10, 31) // xzr
									for i in src_chunks .. num_chunks {
										g.emit_str_reg_offset(10, 29, dest_off + i * 8)
									}
								}
								handled_aggregate_copy = true
							}
						}
					}
				}
			}
			if handled_aggregate_copy {
				return
			}
			// For multi-word struct destinations with constant sources (undef/0),
			// zero-fill all chunks instead of storing a single register.
			if dest_id > 0 && dest_id < g.mod.values.len {
				d_typ_id := g.mod.values[dest_id].typ
				if d_typ_id > 0 && d_typ_id < g.mod.type_store.types.len {
					d_sz := g.type_size(d_typ_id)
					if d_sz > 8 {
						is_const_src := src_id > 0 && src_id < g.mod.values.len
							&& g.mod.values[src_id].kind == .constant
						if is_const_src {
							if d_off := g.stack_map[dest_id] {
								num_chunks := (d_sz + 7) / 8
								g.emit_mov_reg(10, 31) // xzr
								for ci in 0 .. num_chunks {
									g.emit_str_reg_offset(10, 29, d_off + ci * 8)
								}
								return
							}
						}
					}
				}
			}
			// Check if this single-reg fallback is for a multi-word dest
			if dest_id > 0 && dest_id < g.mod.values.len {
				fb_dt := g.mod.values[dest_id].typ
				if fb_dt > 0 && fb_dt < g.mod.type_store.types.len {
					fb_dsz := g.type_size(fb_dt)
					if fb_dsz > 8 {
						eprintln('WARN ASSIGN single-reg fallback for multi-word dest! dest_sz=${fb_dsz} fn=${g.cur_func_name}')
					}
				}
			}
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

			// stack_map[val_id] points to the 8-byte pointer slot.
			// The 24-byte struct data lives right above it (at +8).
			base_offset := g.stack_map[val_id]
			struct_offset := base_offset + 8

			// Store str field (offset 0)
			g.load_val_to_reg(8, str_ptr_id)
			g.emit_str_reg_offset(8, 29, struct_offset)

			// Store len field (offset 8)
			g.load_val_to_reg(9, len_id)
			g.emit_str_reg_offset(9, 29, struct_offset + 8)

			// Store is_lit field (offset 16)
			g.load_val_to_reg(10, is_lit_id)
			g.emit_str_reg_offset(10, 29, struct_offset + 16)

			// Return pointer to struct (stored at base_offset, separate from struct data)
			g.emit_add_fp_imm(8, struct_offset) // x8 = fp + struct_offset
			g.store_reg_to_val(8, val_id)
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
			} else {
				// typ out of range — use default field_byte_off and field_elem_size
			}

			// If the tuple source is a string_literal (e.g. after mem2reg
			// promotion of alloca → store → load), ensure the string struct
			// has been materialized on the stack before we read from it.
			if tuple_val.kind == .string_literal {
				if _ := g.string_literal_offsets[tuple_id] {
					// already materialized
				} else {
					// Force materialization by loading into a scratch register.
					g.load_val_to_reg(10, tuple_id)
				}
			}

			// Get tuple's stack location and load from offset
			if tuple_offset := g.stack_map[tuple_id] {
				if tuple_is_large_agg && idx >= 0
					&& g.large_aggregate_stack_value_is_pointer(tuple_id) {
					g.emit_ldr_reg_offset(9, 29, tuple_offset)
					if field_elem_size > 8 {
						// Multi-word struct field: copy all words from pointer
						if dst_offset := g.stack_map[val_id] {
							num_words := (field_elem_size + 7) / 8
							for w in 0 .. num_words {
								g.emit_ldr_reg_offset(8, 9, field_byte_off + w * 8)
								g.emit_str_reg_offset(8, 29, dst_offset + w * 8)
							}
						} else {
							g.emit_ldr_reg_offset(8, 9, field_byte_off)
							g.store_reg_to_val(8, val_id)
						}
					} else {
						g.emit_ldr_reg_offset(8, 9, field_byte_off)
						g.store_reg_to_val(8, val_id)
					}
				} else if field_elem_size > 8 {
					// Multi-word struct field stored inline: copy all words
					if dst_offset := g.stack_map[val_id] {
						src_offset := tuple_offset + field_byte_off
						num_words := (field_elem_size + 7) / 8
						for w in 0 .. num_words {
							g.emit_ldr_reg_offset(8, 29, src_offset + w * 8)
							g.emit_str_reg_offset(8, 29, dst_offset + w * 8)
						}
					} else {
						field_offset := tuple_offset + field_byte_off
						g.emit_ldr_reg_offset(8, 29, field_offset)
						g.store_reg_to_val(8, val_id)
					}
				} else if field_elem_size in [1, 2, 4] {
					// Use sized load to avoid reading adjacent packed fields.
					// For signed integer fields, use sign-extending loads.
					field_offset := tuple_offset + field_byte_off
					g.emit_add_fp_imm(9, field_offset)
					field_is_unsigned := if instr.typ > 0 && instr.typ < g.mod.type_store.types.len {
						g.mod.type_store.types[instr.typ].is_unsigned
					} else {
						false
					}
					match field_elem_size {
						1 {
							g.emit(asm_ldr_b(Reg(8), Reg(9)))
						}
						2 {
							g.emit(asm_ldr_h(Reg(8), Reg(9)))
						}
						4 {
							if field_is_unsigned {
								g.emit(asm_ldr_w(Reg(8), Reg(9)))
							} else {
								g.emit(asm_ldrsw(Reg(8), Reg(9)))
							}
						}
						else {}
					}
					g.store_reg_to_val(8, val_id)
				} else {
					field_offset := tuple_offset + field_byte_off
					g.emit_ldr_reg_offset(8, 29, field_offset)
					g.store_reg_to_val(8, val_id)
				}
			} else if reg := g.reg_map[tuple_id] {
				// Large aggregates in registers are represented by their address.
				if tuple_is_large_agg && idx >= 0 {
					if field_elem_size > 8 {
						// Multi-word struct field: copy all words from address
						if dst_offset := g.stack_map[val_id] {
							num_words := (field_elem_size + 7) / 8
							for w in 0 .. num_words {
								g.emit_ldr_reg_offset(8, reg, field_byte_off + w * 8)
								g.emit_str_reg_offset(8, 29, dst_offset + w * 8)
							}
						} else {
							g.emit_ldr_reg_offset(8, reg, field_byte_off)
							g.store_reg_to_val(8, val_id)
						}
					} else {
						g.emit_ldr_reg_offset(8, reg, field_byte_off)
						g.store_reg_to_val(8, val_id)
					}
				} else if idx == 0 {
					// Tuple is in a register (e.g., scalarized first field).
					if reg != 8 {
						g.emit_mov_reg(8, reg)
					}
					// Mask to field width for sub-8-byte fields packed in register.
					if field_elem_size in [1, 2, 4] {
						g.emit(asm_ubfx_lower(Reg(8), Reg(8), u32(field_elem_size * 8)))
					}
					g.store_reg_to_val(8, val_id)
				} else {
					// Higher indices packed in same register — shift then mask.
					g.load_val_to_reg(8, tuple_id)
					if field_byte_off > 0 && field_byte_off < 8 {
						g.emit(asm_lsr_imm(Reg(8), Reg(8), u32(field_byte_off * 8)))
					}
					if field_elem_size in [1, 2, 4] {
						g.emit(asm_ubfx_lower(Reg(8), Reg(8), u32(field_elem_size * 8)))
					}
					g.store_reg_to_val(8, val_id)
				}
			} else {
				// Tuple not in stack_map or reg_map - fallback
				g.load_val_to_reg(8, tuple_id)
				g.store_reg_to_val(8, val_id)
			}
		}
		.struct_init {
			// Create struct from field values: operands are field values in order
			result_offset := g.stack_map[val_id]
			struct_typ := g.mod.type_store.types[instr.typ]
			struct_size := g.type_size(instr.typ)
			num_chunks := if struct_size > 0 { (struct_size + 7) / 8 } else { 1 }

			// Zero-initialize the entire struct first
			g.emit_mov_reg(9, 31) // xzr
			for i in 0 .. num_chunks {
				g.emit_str_reg_offset(9, 29, result_offset + i * 8)
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
					g.load_val_to_reg(8, field_id)
					g.emit_str_reg_offset(8, 29, result_offset + field_off)
				} else {
					// Multi-word field (nested struct)
					// Ensure string_literal values are materialized before reading from stack
					if field_val.kind == .string_literal {
						if _ := g.string_literal_offsets[field_id] {
							// already materialized
						} else {
							// Force materialization by loading into a scratch register
							g.load_val_to_reg(10, field_id)
						}
					}
					field_chunks := (field_size + 7) / 8
					if field_offset := g.stack_map[field_id] {
						mut src_ptr_reg := 12
						if field_size > 16 && g.large_aggregate_stack_value_is_pointer(field_id) {
							g.emit_ldr_reg_offset(src_ptr_reg, 29, field_offset)
						} else {
							g.emit_add_fp_imm(src_ptr_reg, field_offset)
						}
						for w in 0 .. field_chunks {
							g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(w)))
							g.emit_str_reg_offset(10, 29, result_offset + field_off + w * 8)
						}
					} else {
						// Fallback: store first word only
						in_reg := field_id in g.reg_map
						eprintln('WARN: struct_init multi-word field fallback: field_id=${field_id} field_size=${field_size} field_chunks=${field_chunks} in_reg=${in_reg} val_kind=${field_val.kind} val_name="${field_val.name}" fn=${g.cur_func_name}')
						g.load_val_to_reg(8, field_id)
						g.emit_str_reg_offset(8, 29, result_offset + field_off)
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
					if tuple_size > 16 && g.large_aggregate_stack_value_is_pointer(tuple_id) {
						// Keep source pointer in x12 so stores can use x11 scratch safely.
						g.emit_ldr_reg_offset(12, 29, tuple_offset)
						for i in 0 .. num_chunks {
							g.emit_ldr_reg_offset(9, 12, i * 8)
							g.emit_str_reg_offset(9, 29, result_offset + i * 8)
						}
					} else {
						for i in 0 .. num_chunks {
							g.emit_ldr_reg_offset(9, 29, tuple_offset + i * 8)
							g.emit_str_reg_offset(9, 29, result_offset + i * 8)
						}
					}
					copied_tuple = true
				} else if src_reg := g.reg_map[tuple_id] {
					for i in 0 .. num_chunks {
						g.emit(asm_ldr_imm(Reg(9), Reg(src_reg), u32(i)))
						g.emit_str_reg_offset(9, 29, result_offset + i * 8)
					}
					copied_tuple = true
				}
				if !copied_tuple {
					// Deterministic fallback for missing aggregate backing bytes.
					g.emit_mov_reg(9, 31)
					for i in 0 .. num_chunks {
						g.emit_str_reg_offset(9, 29, result_offset + i * 8)
					}
				}
			} else {
				// Start from zeroed storage for `insertvalue(undef, ...)`.
				g.emit_mov_reg(9, 31)
				for i in 0 .. num_chunks {
					g.emit_str_reg_offset(9, 29, result_offset + i * 8)
				}
			}

			// Store the new element at the specified index. Aggregate fields need
			// full-width copies, not a single 8-byte store.
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
				g.load_val_to_reg(8, elem_id)
				g.emit_str_reg_offset(8, 29, result_offset + elem_off)
			} else {
				elem_chunks := (elem_size + 7) / 8
				mut copied_elem := false
				if elem_offset := g.stack_map[elem_id] {
					mut src_ptr_reg := 12
					if elem_size > 16 && g.large_aggregate_stack_value_is_pointer(elem_id) {
						g.emit_ldr_reg_offset(src_ptr_reg, 29, elem_offset)
					} else {
						g.emit_add_fp_imm(src_ptr_reg, elem_offset)
					}
					for i in 0 .. elem_chunks {
						g.emit(asm_ldr_imm(Reg(10), Reg(src_ptr_reg), u32(i)))
						g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
					}
					copied_elem = true
				} else if src_reg := g.reg_map[elem_id] {
					for i in 0 .. elem_chunks {
						g.emit(asm_ldr_imm(Reg(10), Reg(src_reg), u32(i)))
						g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
					}
					copied_elem = true
				}
				if !copied_elem {
					// Best effort fallback: store first word, clear the rest.
					g.load_val_to_reg(8, elem_id)
					g.emit_str_reg_offset(8, 29, result_offset + elem_off)
					g.emit_mov_reg(10, 31)
					for i in 1 .. elem_chunks {
						g.emit_str_reg_offset(10, 29, result_offset + elem_off + i * 8)
					}
				}
			}
		}
		else {
			eprintln('arm64: unknown instruction ${op} (${instr.selected_op})')
			exit(1)
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

fn (g &Gen) has_function_named(name string) bool {
	for f in g.mod.funcs {
		if f.name == name {
			return true
		}
	}
	return false
}

fn (g &Gen) call_param_type(instr mir.Instruction, arg_idx int) ?ssa.TypeID {
	if arg_idx < 0 || instr.operands.len == 0 {
		return none
	}
	fn_id := instr.operands[0]
	if fn_id <= 0 || fn_id >= g.mod.values.len {
		return none
	}
	fn_val := g.mod.values[fn_id]
	if fn_val.name == '' {
		return none
	}
	for f in g.mod.funcs {
		if f.name != fn_val.name {
			continue
		}
		if arg_idx < f.params.len {
			param_id := f.params[arg_idx]
			if param_id > 0 && param_id < g.mod.values.len {
				return g.mod.values[param_id].typ
			}
		}
		return none
	}
	return none
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

// Returns the number of registers a call argument occupies on ARM64.
// Struct args 9-16 bytes use 2 consecutive registers; all others use 1.
fn (g &Gen) call_arg_reg_count(val_id int, arg_idx int, instr mir.Instruction) int {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	if is_indirect {
		return 1 // pointer
	}
	if val_id <= 0 || val_id >= g.mod.values.len {
		return 1
	}
	val := g.mod.values[val_id]
	if val.typ > 0 && val.typ < g.mod.type_store.types.len {
		val_typ := g.mod.type_store.types[val.typ]
		if val_typ.kind == .struct_t {
			size := g.type_size(val.typ)
			if size > 8 && size <= 16 {
				return 2
			}
		}
	}
	return 1
}

// Loads a multi-register struct argument (9-16 bytes) into consecutive regs.
fn (mut g Gen) load_struct_arg_to_regs(start_reg int, val_id int) {
	// Get the stack address of the struct value and load 2 words.
	g.load_address_of_val_to_reg(9, val_id)
	// Load first word to start_reg
	g.emit(asm_ldr_imm(Reg(start_reg), Reg(9), 0))
	// Load second word to start_reg+1
	g.emit(asm_ldr_imm(Reg(start_reg + 1), Reg(9), 1))
}

fn (mut g Gen) load_call_arg_to_reg(reg int, val_id int, arg_idx int, instr mir.Instruction) {
	is_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	if !is_indirect {
		if param_typ_id := g.call_param_type(instr, arg_idx) {
			if param_typ_id > 0 && param_typ_id < g.mod.type_store.types.len && val_id > 0
				&& val_id < g.mod.values.len {
				param_typ := g.mod.type_store.types[param_typ_id]
				arg_val := g.mod.values[val_id]
				if arg_val.typ > 0 && arg_val.typ < g.mod.type_store.types.len {
					arg_typ := g.mod.type_store.types[arg_val.typ]
					// If call lowering asks for a pointer parameter but the argument is
					// a struct value, pass the address of the struct instead of
					// loading its first field. This handles call results, load results,
					// and other struct-producing instructions.
					// Applies to ALL struct sizes (e.g., sumtype=16 bytes, string=24 bytes).
					if param_typ.kind == .ptr_t && arg_typ.kind == .struct_t {
						g.load_address_of_val_to_reg(reg, val_id)
						return
					}
				}
			}
		}
	}
	if is_indirect {
		g.load_address_of_val_to_reg(reg, val_id)
		return
	}
	g.load_val_to_reg(reg, val_id)
}

fn (mut g Gen) canonicalize_narrow_int_result(reg int, typ_id ssa.TypeID) {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return
	}
	typ := g.mod.type_store.types[typ_id]
	if typ.kind != .int_t || typ.width <= 0 || typ.width >= 64 {
		return
	}
	if typ.is_unsigned || typ.width <= 1 {
		// For booleans (width=1) and unsigned types, zero-extend by masking.
		// Bool is get_int(1) which is signed, but sign-extending 1-bit value 1
		// would give -1 (0xFFFFFFFFFFFFFFFF), so always zero-extend.
		if typ.width <= 32 {
			mask := (u64(1) << typ.width) - 1
			g.emit_mov_imm64(11, i64(mask))
			g.emit(asm_and(Reg(reg), Reg(reg), Reg(11)))
		}
		// Unsigned 33-63 bit: no action needed (upper bits already zero)
	} else {
		// For signed types, sign-extend via LSL + ASR
		shift := 64 - typ.width
		mut shreg := 11
		if reg == shreg {
			shreg = 12
		}
		g.emit_mov_imm64(shreg, shift)
		g.emit(asm_lslv(Reg(reg), Reg(reg), Reg(shreg)))
		g.emit(asm_asrv(Reg(reg), Reg(reg), Reg(shreg)))
	}
}

fn (mut g Gen) load_struct_src_address_to_reg(reg int, val_id int, expected_struct_typ ssa.TypeID) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
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

fn (mut g Gen) load_address_of_val_to_reg(reg int, val_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val := g.mod.values[val_id]
	// string_literal values need full materialization (create cstring data,
	// store str/len/is_lit to stack) before their address can be taken.
	// load_val_to_reg handles this and returns the struct address.
	if val.kind == .string_literal {
		g.load_val_to_reg(reg, val_id)
		return
	}
	if val.kind == .instruction {
		instr := g.mod.instrs[val.index]
		if instr.op == .load && instr.operands.len > 0 && val.typ > 0
			&& val.typ < g.mod.type_store.types.len {
			loaded_typ := g.mod.type_store.types[val.typ]
			if loaded_typ.kind == .struct_t {
				// For indirect aggregate args produced via load, forward the original
				// pointer operand rather than taking the address of a scalarized temp.
				g.load_val_to_reg(reg, instr.operands[0])
				return
			}
		}
	}
	if offset := g.stack_map[val_id] {
		g.emit_add_fp_imm(reg, offset)
		return
	}
	// Value already represents or contains a pointer.
	g.load_val_to_reg(reg, val_id)
}

// load_float_operand loads a value into a float register (d0-d7).
// For constants, parses the float value and loads via literal pool or immediate.
// For memory values, loads from stack into integer reg then moves to float reg.
fn (mut g Gen) load_float_operand(val_id int, dreg int) {
	val := g.mod.values[val_id]
	is_f32 := val.typ > 0 && val.typ < g.mod.type_store.types.len
		&& g.mod.type_store.types[val.typ].kind == .float_t
		&& g.mod.type_store.types[val.typ].width == 32
	if val.kind == .constant {
		// Parse float constant and load into float register
		if is_f32 {
			// f32 constant: parse as f64, convert to f32, load via s-register
			f_val := f32(val.name.f64())
			bits := *unsafe { &u32(&f_val) }
			g.emit_mov_imm64(8, i64(bits))
			g.emit(asm_fmov_s_w(dreg, Reg(8)))
			g.emit(asm_fcvt_d_s(dreg, dreg))
		} else {
			// f64 constant: load 64-bit bit pattern
			f_val := val.name.f64()
			bits := *unsafe { &u64(&f_val) }
			g.emit_mov_imm64(8, i64(bits))
			g.emit(asm_fmov_d_x(dreg, Reg(8)))
		}
	} else {
		if is_f32 {
			// f32 from stack: load 32-bit value, move to s-register, convert to double
			g.load_val_to_reg(8, val_id)
			g.emit(asm_fmov_s_w(dreg, Reg(8)))
			g.emit(asm_fcvt_d_s(dreg, dreg))
		} else {
			// f64 from stack: load 64-bit value, move to d-register
			g.load_val_to_reg(8, val_id)
			g.emit(asm_fmov_d_x(dreg, Reg(8)))
		}
	}
}

// Get constant integer value with caching to avoid repeated string parsing
fn (mut g Gen) get_const_int(val_id int) i64 {
	if cached := g.const_cache[val_id] {
		return cached
	}
	// Parse as u64 first to handle hex values with bit 63 set (e.g., 0x800fffffffffffff)
	// that would overflow i64 parsing. Then reinterpret as i64 to preserve the bit pattern.
	name := g.mod.values[val_id].name
	int_val := if name.len > 2 && name[0] == `0` && (name[1] == `x` || name[1] == `X`) {
		i64(name.u64())
	} else {
		name.i64()
	}
	g.const_cache[val_id] = int_val
	return int_val
}

fn (mut g Gen) load_val_to_reg(reg int, val_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val := g.mod.values[val_id]
	// Handle func_ref early: function pointers use ADRP+ADD regardless of their declared type
	if val.kind == .func_ref {
		sym_idx := g.macho.add_undefined('_' + val.name)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(asm_adrp(Reg(reg)))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(asm_add_pageoff(Reg(reg)))
		return
	}
	if val.typ < 0 || val.typ >= g.mod.type_store.types.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val_typ := g.mod.type_store.types[val.typ]
	if val_typ.kind == .void_t {
		g.emit_mov_imm64(reg, 0)
		return
	}
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
		} else if val_typ.kind == .float_t {
			// Float constant: load IEEE 754 bit pattern, not truncated integer
			if val_typ.width == 32 {
				// f32 constant: store 32-bit IEEE 754 pattern
				f_val := f32(val.name.f64())
				bits := *unsafe { &u32(&f_val) }
				g.emit_mov_imm64(reg, i64(bits))
			} else {
				f_val := val.name.f64()
				bits := *unsafe { &u64(&f_val) }
				g.emit_mov_imm64(reg, i64(bits))
			}
		} else {
			int_val := g.get_const_int(val_id)
			g.emit_mov_imm64(reg, int_val)
		}
	} else if val.kind == .global {
		// Check if this is an external global (needs GOT access)
		mut is_external := false
		for gvar in g.mod.globals {
			if gvar.name == val.name && gvar.linkage == .external {
				is_external = true
				break
			}
		}

		if is_external {
			// External global: load address through GOT
			sym_idx := g.macho.add_undefined('_' + val.name)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_got_load_page21,
				true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_got_load_pageoff12,
				false)
			g.emit(asm_ldr_pageoff(Reg(reg)))
		} else {
			// Local global: direct address
			sym_idx := g.macho.add_undefined('_' + val.name)
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
			g.emit(asm_adrp(Reg(reg)))
			g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
			g.emit(asm_add_pageoff(Reg(reg)))
		}
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

			// Create the string data in cstring section (deduplicate identical strings)
			str_offset2 := if existing := g.string_data_cache[str_content] {
				existing
			} else {
				offset := g.macho.str_data.len
				g.macho.str_data << str_content.bytes()
				g.macho.str_data << 0 // null terminator
				g.string_data_cache[str_content] = offset
				offset
			}

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
	} else if val.kind == .c_string_literal {
		// C string literal: just a raw char* pointer to string data
		// Process C escape sequences (\n, \t, etc.) into actual bytes
		raw_bytes := val.name.bytes()
		str_offset2 := g.macho.str_data.len
		mut i2 := 0
		for i2 < raw_bytes.len {
			if raw_bytes[i2] == `\\` && i2 + 1 < raw_bytes.len {
				match raw_bytes[i2 + 1] {
					`n` { g.macho.str_data << 0x0A } // newline
					`t` { g.macho.str_data << 0x09 } // tab
					`r` { g.macho.str_data << 0x0D } // carriage return
					`0` { g.macho.str_data << 0x00 } // null
					`\\` { g.macho.str_data << 0x5C } // backslash
					`'` { g.macho.str_data << 0x27 } // single quote
					`"` { g.macho.str_data << 0x22 } // double quote
					else { g.macho.str_data << raw_bytes[i2 + 1] }
				}
				i2 += 2
			} else {
				g.macho.str_data << raw_bytes[i2]
				i2++
			}
		}
		g.macho.str_data << 0 // null terminator
		sym_idx := g.macho.add_symbol('L_cstr_${str_offset2}', u64(str_offset2), false,
			2)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(asm_adrp(Reg(reg)))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(asm_add_pageoff(Reg(reg)))
	} else if val.kind == .func_ref {
		// Function pointer reference - load address of the function
		// Add symbol and use ADRP + ADD to get the address
		sym_idx := g.macho.add_undefined('_' + val.name)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
		g.emit(asm_adrp(Reg(reg)))
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
		g.emit(asm_add_pageoff(Reg(reg)))
	} else {
		// Handles .instruction, .argument, etc.
		// For large struct types (> 16 bytes like string), ARM64 ABI requires
		// passing by pointer, so load the address instead of the value
		typ := val_typ
		val_size := g.type_size(val.typ)
		if (typ.kind == .struct_t || typ.kind == .array_t) && val_size > 16 {
			// Large structs/arrays can be materialized either by-value (slot contains bytes)
			// or indirectly (slot contains a pointer to bytes). Preserve the producer's
			// representation when loading from stack.
			if reg_idx := g.reg_map[val_id] {
				if reg_idx != reg {
					g.emit_mov_reg(reg, reg_idx)
				}
			} else if offset := g.stack_map[val_id] {
				if typ.kind == .array_t {
					// Array values in stack slots are always by-value (data bytes directly).
					g.emit_add_fp_imm(reg, offset)
				} else if g.large_struct_stack_value_is_pointer(val_id) {
					g.emit_ldr_reg_offset(reg, 29, offset)
				} else {
					g.emit_add_fp_imm(reg, offset)
				}
			} else {
				g.emit_mov_imm64(reg, 0)
			}
		} else if reg_idx := g.reg_map[val_id] {
			if reg_idx != reg {
				g.emit_mov_reg(reg, reg_idx)
			}
		} else {
			if offset := g.stack_map[val_id] {
				g.emit_ldr_reg_offset(reg, 29, offset)
			} else {
				g.emit_mov_imm64(reg, 0)
			}
		}
	}
}

fn (mut g Gen) load_fnptr_to_reg(reg int, val_id int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		g.emit_mov_imm64(reg, 0)
		return
	}
	val := g.mod.values[val_id]
	if val.kind in [.constant, .func_ref, .global] {
		g.load_val_to_reg(reg, val_id)
		return
	}
	if reg_idx := g.reg_map[val_id] {
		if reg_idx != reg {
			g.emit_mov_reg(reg, reg_idx)
		}
		return
	}
	if offset := g.stack_map[val_id] {
		g.emit_ldr_reg_offset(reg, 29, offset)
	} else {
		g.emit_mov_imm64(reg, 0)
	}
}

fn (mut g Gen) store_reg_to_val(reg int, val_id int) {
	mut stored_reg := reg
	if reg_idx := g.reg_map[val_id] {
		if reg_idx != reg {
			g.emit_mov_reg(reg_idx, reg)
		}
		stored_reg = reg_idx
	}
	if offset := g.stack_map[val_id] {
		if val_id > 0 && val_id < g.mod.values.len {
			val_typ_id := g.mod.values[val_id].typ
			if val_typ_id > 0 && val_typ_id < g.mod.type_store.types.len {
				val_typ := g.mod.type_store.types[val_typ_id]
				if val_typ.kind == .struct_t && g.type_size(val_typ_id) <= 8 {
					g.emit_str_reg_offset(stored_reg, 29, offset)
					return
				}
			}
		}
		if val_id !in g.reg_map {
			g.emit_str_reg_offset(stored_reg, 29, offset)
		}
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
	g.emit_str_reg_offset_sized(rt, rn, offset, 8)
}

fn (mut g Gen) emit_str_reg_offset_sized(rt int, rn int, offset int, size int) {
	if offset >= -255 && offset <= 255 {
		match size {
			1 { g.emit(asm_stur_b(Reg(rt), Reg(rn), offset)) }
			2 { g.emit(asm_stur_h(Reg(rt), Reg(rn), offset)) }
			4 { g.emit(asm_stur_w(Reg(rt), Reg(rn), offset)) }
			else { g.emit(asm_stur(Reg(rt), Reg(rn), offset)) }
		}
	} else {
		// Large offset: materialize effective address in a non-conflicting scratch register.
		mut scratch := 11
		if rt == scratch || rn == scratch {
			scratch = 12
		}
		if offset < 0 {
			g.emit_mov_imm64(scratch, i64(-offset))
			g.emit(asm_sub_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		} else {
			g.emit_mov_imm64(scratch, i64(offset))
			g.emit(asm_add_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		}
		match size {
			1 { g.emit(asm_str_b(Reg(rt), Reg(scratch))) }
			2 { g.emit(asm_str_h(Reg(rt), Reg(scratch))) }
			4 { g.emit(asm_str_w(Reg(rt), Reg(scratch))) }
			else { g.emit(asm_str(Reg(rt), Reg(scratch))) }
		}
	}
}

fn (mut g Gen) emit_ldr_reg_offset(rt int, rn int, offset int) {
	g.emit_ldr_reg_offset_sized(rt, rn, offset, 8)
}

fn (mut g Gen) emit_ldr_reg_offset_sized(rt int, rn int, offset int, size int) {
	if offset >= -255 && offset <= 255 {
		match size {
			1 { g.emit(asm_ldur_b(Reg(rt), Reg(rn), offset)) }
			2 { g.emit(asm_ldur_h(Reg(rt), Reg(rn), offset)) }
			4 { g.emit(asm_ldur_w(Reg(rt), Reg(rn), offset)) }
			else { g.emit(asm_ldur(Reg(rt), Reg(rn), offset)) }
		}
	} else {
		// Large offset: materialize effective address in a non-conflicting scratch register.
		mut scratch := 11
		if rt == scratch || rn == scratch {
			scratch = 12
		}
		if offset < 0 {
			g.emit_mov_imm64(scratch, i64(-offset))
			g.emit(asm_sub_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		} else {
			g.emit_mov_imm64(scratch, i64(offset))
			g.emit(asm_add_reg(Reg(scratch), Reg(rn), Reg(scratch)))
		}
		match size {
			1 { g.emit(asm_ldr_b(Reg(rt), Reg(scratch))) }
			2 { g.emit(asm_ldr_h(Reg(rt), Reg(scratch))) }
			4 { g.emit(asm_ldr_w(Reg(rt), Reg(scratch))) }
			else { g.emit(asm_ldr(Reg(rt), Reg(scratch))) }
		}
	}
}

fn (mut g Gen) emit_call_to_named_fn(fn_name string) {
	sym_idx := g.macho.add_undefined('_' + fn_name)
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
	g.emit(asm_bl_reloc())
}

fn (mut g Gen) store_entry_arg_to_global(reg int, global_name string) {
	if global_name == '' {
		return
	}
	sym_idx := g.macho.add_undefined('_' + global_name)
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
	g.emit(asm_adrp(Reg(9)))
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
	g.emit(asm_add_pageoff(Reg(9)))
	g.emit_str_reg_offset(reg, 9, 0)
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

fn (g Gen) ptr_elem_size_bytes(ptr_val_id int) int {
	if ptr_val_id <= 0 || ptr_val_id >= g.mod.values.len {
		return 8
	}
	ptr_typ_id := g.mod.values[ptr_val_id].typ
	if ptr_typ_id <= 0 || ptr_typ_id >= g.mod.type_store.types.len {
		return 8
	}
	ptr_typ := g.mod.type_store.types[ptr_typ_id]
	if ptr_typ.kind != .ptr_t || ptr_typ.elem_type <= 0
		|| ptr_typ.elem_type >= g.mod.type_store.types.len {
		return 8
	}
	elem_size := g.type_size(ptr_typ.elem_type)
	return if elem_size in [1, 2, 4] { elem_size } else { 8 }
}

fn (g Gen) mem_access_size_bytes(val_typ_id ssa.TypeID, ptr_val_id int) int {
	if val_typ_id <= 0 || val_typ_id >= g.mod.type_store.types.len {
		return g.ptr_elem_size_bytes(ptr_val_id)
	}
	val_typ := g.mod.type_store.types[val_typ_id]
	// The pointer element type indicates the actual memory width. When the
	// value was promoted to a wider register type (e.g., i64 for a u8 store),
	// the pointer element size takes precedence.
	ptr_elem_size := g.ptr_elem_size_bytes(ptr_val_id)
	match val_typ.kind {
		.ptr_t, .func_t {
			return 8
		}
		.int_t, .float_t {
			val_size := g.type_size(val_typ_id)
			if val_size in [1, 2, 4, 8] {
				if ptr_elem_size in [1, 2, 4] && ptr_elem_size < val_size {
					return ptr_elem_size
				}
				return val_size
			}
		}
		else {}
	}
	return ptr_elem_size
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
			if typ.is_union {
				// Union: size = max(field_sizes), aligned to largest field
				mut max_size := 0
				for field_typ in typ.fields {
					fs := g.type_size(field_typ)
					if fs > max_size {
						max_size = fs
					}
				}
				return if max_size > 0 { max_size } else { 8 }
			}
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
			// Align struct size to its largest field alignment
			if max_align > 1 && total % max_align != 0 {
				total = (total + max_align - 1) & ~(max_align - 1)
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

fn (g Gen) type_align(typ_id ssa.TypeID) int {
	if typ_id > 0 && typ_id < g.mod.type_store.types.len {
		typ := g.mod.type_store.types[typ_id]
		if typ.kind == .array_t {
			// Array alignment = element alignment (matching C semantics)
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
	// Union types: all fields overlap at offset 0
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
	// Values materialized directly into aggregate storage.
	if val.kind == .string_literal {
		return false
	}
	if val.kind == .instruction {
		instr := g.mod.instrs[val.index]
		op := g.selected_opcode(instr)
		return op !in [.call, .call_sret, .inline_string_init, .insertvalue, .struct_init,
			.extractvalue, .assign, .phi, .bitcast, .load]
	}
	// Arguments and globals are treated as by-value stack storage.
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
			// Array aggregates materialized by calls/insertvalue/extractvalue are by-value.
			return op !in [.call, .call_sret, .insertvalue, .extractvalue, .assign, .phi, .bitcast,
				.load]
		}
	}
	return false
}

// lookup_type_from_env looks up a type by name from the Environment.
// Returns the types.Type if found, none otherwise.
fn (g &Gen) lookup_type_from_env(name string, module_name string) ?types.Type {
	if g.mod.env == unsafe { nil } {
		return none
	}
	mut scope := &types.Scope(unsafe { nil })
	if s := g.mod.env.get_scope(module_name) {
		scope = unsafe { s }
	} else if s := g.mod.env.get_scope('builtin') {
		scope = unsafe { s }
	} else {
		return none
	}
	if obj := scope.lookup_parent(name, 0) {
		if obj is types.Type {
			return obj
		}
	}
	return none
}

// lookup_struct_from_env looks up a struct type by name from the Environment.
fn (g &Gen) lookup_struct_from_env(name string) ?types.Struct {
	if typ := g.lookup_type_from_env(name, 'builtin') {
		if typ is types.Struct {
			return typ
		}
	}
	return none
}

fn (mut g Gen) allocate_registers(func mir.Function) {
	mut intervals := map[int]&Interval{}
	mut call_indices := []int{}
	mut instr_idx := 0
	// Phi elimination lowers edge copies as `.assign dest, src` and leaves
	// placeholder `.bitcast` values for former phis. Keep these carried values
	// on the stack so loop-carried state does not depend on linearized interval
	// approximations in the register allocator.
	mut phi_related_vals := map[int]bool{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]
			if instr.op == .assign {
				phi_related_vals[val_id] = true
				if instr.operands.len > 0 {
					phi_related_vals[instr.operands[0]] = true
				}
				if instr.operands.len > 1 {
					phi_related_vals[instr.operands[1]] = true
				}
			} else if instr.op == .bitcast && instr.operands.len == 0 {
				phi_related_vals[val_id] = true
			}
		}
	}

	// Map block index to instruction range
	mut block_start := map[int]int{}
	mut block_end := map[int]int{}

	for pid in func.params {
		if pid in phi_related_vals {
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
		block_start[blk_id] = instr_idx
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind == .instruction || val.kind == .argument {
				// Check if this is a call with indirect struct return (> 16 bytes)
				// These values must stay on the stack - don't register-allocate them
				mut skip_interval := val_id in phi_related_vals
				if val.kind == .instruction {
					instr := g.mod.instrs[val.index]
					if instr.op in [.call, .call_indirect, .call_sret] {
						result_typ := g.mod.type_store.types[val.typ]
						if result_typ.kind == .struct_t {
							result_size := g.type_size(val.typ)
							if result_size > 16 {
								skip_interval = true
							}
						}
					}
				}
				if val.typ > 0 && val.typ < g.mod.type_store.types.len {
					val_typ := g.mod.type_store.types[val.typ]
					if val_typ.kind == .struct_t && val_typ.fields.len > 1 {
						val_size := g.type_size(val.typ)
						if val_size <= 16 {
							skip_interval = true
						}
					}
				}

				if !skip_interval && unsafe { intervals[val_id] == nil } {
					intervals[val_id] = &Interval{
						val_id: val_id
						start:  instr_idx
						end:    instr_idx
					}
				}
			}

			instr := g.mod.instrs[val.index]
			if instr.op in [.call, .call_indirect, .call_sret, .heap_alloc] {
				call_indices << instr_idx
			}

			for op in instr.operands {
				if op in phi_related_vals {
					continue
				}
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
	// Parameters can be consumed in any CFG block (including loop bodies).
	// Keep them live for the full function so they are not re-used by
	// later temporaries before all dynamic iterations/branches are done.
	for pid in func.params {
		if mut interval := intervals[pid] {
			interval.end = total_instrs
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
			for op in instr.operands {
				if op in phi_related_vals {
					continue
				}
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
	// Reserve x8/x9 as backend data path scratch registers.
	// Reserve x10/x11/x12 for helper temporaries (large offset materialization,
	// address arithmetic, and spill-free internal moves).
	// Temporary conservative mode: keep values stack-resident for correctness.
	// This avoids backend miscompilations caused by incomplete live interval
	// modeling across complex CFG/aggregate paths.
	short_regs := []int{}
	long_regs := []int{}

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
