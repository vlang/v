// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import os
import v.cmdexec
import v.ssa
import encoding.binary

// GenOptions controls optional x64 code-generation optimizations.
pub struct GenOptions {
pub:
	optimize_runtime bool
}

pub struct Gen {
	m &ssa.Module
mut:
	elf              &ElfObject
	optimize_runtime bool

	stack_map      map[int]int
	alloca_offsets map[int]int
	stack_size     int
	curr_offset    int

	block_offsets  map[int]int
	pending_labels map[int][]int

	// Register allocation
	reg_map         map[int]int
	used_regs       []int
	reg_use_counts  []int
	reg_use_stamps  []int
	reg_seen_stamps []int
	reg_stamp       int

	cur_func_ret_type         int
	cur_func_abi_ret_indirect bool
	cur_func_param_indirect   []bool
	sret_save_offset          int
}

struct CCallArg {
	value_id int
	typ      ssa.TypeID
	reg      int
	on_stack bool
}

struct RegCandidate {
	value_id int
	score    int
}

// new creates an x86-64 generator for an SSA module without runtime optimizations.
pub fn Gen.new(m &ssa.Module) &Gen {
	return Gen.new_with_options(m, GenOptions{})
}

// new_with_options creates an x86-64 generator with explicit optimization options.
pub fn Gen.new_with_options(m &ssa.Module, opts GenOptions) &Gen {
	return &Gen{
		m:                m
		elf:              ElfObject.new()
		optimize_runtime: opts.optimize_runtime
	}
}

// gen emits all functions, data, and relocations for the module.
pub fn (mut g Gen) gen() {
	value_count := g.m.values.len
	g.reg_use_counts = []int{len: value_count}
	g.reg_use_stamps = []int{len: value_count}
	g.reg_seen_stamps = []int{len: value_count}
	for func in g.m.funcs {
		g.gen_func(func)
	}
	g.gen_post_pass()
}

fn (g &Gen) is_aggregate_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	return g.m.type_store.types[typ_id].kind in [.struct_t, .array_t]
}

fn (g &Gen) is_float_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	return g.m.type_store.types[typ_id].kind == .float_t
}

fn (g &Gen) is_f32_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .float_t && typ.width == 32
}

fn (g &Gen) is_string_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .struct_t && typ.field_names.len == 3 && typ.field_names[0] == 'str'
		&& typ.field_names[1] == 'len' && typ.field_names[2] == 'is_lit'
}

fn (g &Gen) is_zero_const(val_id ssa.ValueID) bool {
	if val_id <= 0 || val_id >= g.m.values.len {
		return false
	}
	val := g.m.values[val_id]
	if val.kind != .constant {
		return false
	}
	return val.name == '0' || val.name == '0.0' || val.name == 'false' || val.name == 'undef'
}

fn (g &Gen) is_indirect_return(typ_id ssa.TypeID) bool {
	return g.is_aggregate_type(typ_id) && g.m.type_size(typ_id) > 8
}

fn (g &Gen) is_indirect_param(typ_id ssa.TypeID) bool {
	return g.is_aggregate_type(typ_id) && g.m.type_size(typ_id) > 8
}

fn (g &Gen) indirect_params(func ssa.Function) []bool {
	mut result := []bool{len: func.params.len}
	for i, pid in func.params {
		if pid > 0 && pid < g.m.values.len {
			result[i] = g.is_indirect_param(g.m.values[pid].typ)
		}
	}
	return result
}

fn (mut g Gen) gen_post_pass() {
	// `C._wyp` is a generated C constant, not a libc symbol. Native builds must
	// materialize it themselves because map hashing refers to it through SSA.
	g.elf.add_symbol('_wyp', u64(g.elf.data_data.len), false, 2)
	secrets := [u64(0x2d358dccaa6c78a5), u64(0x8bb84b93962eacc9), u64(0x4b33a62ed433d4a3),
		u64(0x4d5a2da51de1aa47)]
	for secret in secrets {
		mut bytes := []u8{len: 8}
		binary.little_endian_put_u64(mut bytes, secret)
		g.elf.data_data << bytes
	}
	// Generate Globals in .data
	for gvar in g.m.globals {
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
		} else {
			// Determine the proper size for this global from its type.
			// Constant scalars (int, bool, etc.) are stored as 8-byte values,
			// but compound types (strings, arrays, structs) need their full size.
			size := g.type_size(gvar.typ)
			actual_size := if size > 0 { size } else { 8 }
			if gvar.is_constant {
				mut bytes := []u8{len: 8}
				binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
				mut index := 0
				for index < actual_size {
					g.elf.data_data << bytes[index]
					index++
				}
			} else {
				for _ in 0 .. actual_size {
					g.elf.data_data << 0
				}
			}
		}
	}
}

fn (mut g Gen) gen_func(func ssa.Function) {
	if func.is_c_extern {
		return
	}
	dump_filter := os.getenv('V_X64_DUMP_SSA')
	if dump_filter.len > 0 && func.name.contains(dump_filter) {
		g.dump_function(func)
	}
	if func.blocks.len == 0 {
		// Leave unsupported bodies undefined so the linker reports the missing implementation.
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
	g.cur_func_abi_ret_indirect = g.is_indirect_return(func.typ)
	g.cur_func_param_indirect = g.indirect_params(func)
	g.sret_save_offset = 0
	g.allocate_scalar_registers(func)

	// Calculate Stack Frame
	// Start local variables after callee-saved register save area.
	// Callee-saved regs are pushed after `mov rbp, rsp`, occupying
	// [rbp-8], [rbp-16], ... [rbp-N*8], so locals start at rbp-(N+1)*8.
	mut slot_offset := 8 + g.used_regs.len * 8

	// Hidden sret pointer slot (SysV: incoming in RDI)
	if g.cur_func_abi_ret_indirect {
		g.sret_save_offset = -slot_offset
		slot_offset += 8
	}

	for pi, pid in func.params {
		param_typ := g.m.values[pid].typ
		param_size := g.type_size(param_typ)
		is_indirect_param := pi < g.cur_func_param_indirect.len
			&& g.cur_func_param_indirect[pi]
		if is_indirect_param && param_size > 0 {
			slot_offset = (slot_offset + 15) & ~0xF
			slot_offset += param_size
			g.stack_map[pid] = -slot_offset
			// Advance past the indirect data base so the next regular 8-byte
			// slot (assigned at -slot_offset before +=8) doesn't overlap byte 0
			// of this indirect area.
			slot_offset += 8
		} else if pid !in g.reg_map {
			g.stack_map[pid] = -slot_offset
			slot_offset += 8
		}
	}

	// Pre-pass: allocate stack slots for string_literal values used in this function
	mut seen_str_lits := map[int]bool{}
	for blk_id in func.blocks {
		blk := g.m.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.m.values[val_id]
			if val.kind == .phi_result {
				if val_id in g.reg_map {
					continue
				}
				val_typ := g.m.type_store.types[val.typ]
				if val_typ.kind in [.struct_t, .array_t] {
					mut result_size := g.type_size(val.typ)
					if result_size <= 0 {
						result_size = 8
					}
					slot_offset = (slot_offset + 15) & ~0xF
					slot_offset += result_size
					g.stack_map[val_id] = -slot_offset
					slot_offset += 8
				} else {
					g.stack_map[val_id] = -slot_offset
					slot_offset += 8
				}
				continue
			}
			if val.kind != .instruction {
				continue
			}
			instr := g.m.instrs[val.index]
			for op in instr.operands {
				op_val := g.m.values[op]
				if op_val.kind == .phi_result && op !in g.reg_map && op !in g.stack_map {
					op_typ := g.m.type_store.types[op_val.typ]
					if op_typ.kind in [.struct_t, .array_t] {
						mut result_size := g.type_size(op_val.typ)
						if result_size <= 0 {
							result_size = 8
						}
						slot_offset = (slot_offset + 15) & ~0xF
						slot_offset += result_size
						g.stack_map[op] = -slot_offset
						slot_offset += 8
					} else {
						g.stack_map[op] = -slot_offset
						slot_offset += 8
					}
				}
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
		blk := g.m.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.m.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.m.instrs[val.index]

			if instr.op == .alloca {
				// Calculate allocation size based on the type
				// The alloca result type is ptr(T), so get the element type
				ptr_type := g.m.type_store.types[val.typ]
				elem_size := g.type_size(ptr_type.elem_type)
				mut alloc_size := if elem_size > 0 { elem_size } else { 8 }
				// Check for array alloca: operand[0] is element count
				if instr.operands.len > 0 {
					count_val := g.m.values[instr.operands[0]]
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
				g.stack_map[val_id] = -slot_offset
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
					tuple_typ := g.m.type_store.types[instr.typ]
					tuple_size = tuple_typ.fields.len * 8
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += tuple_size
				g.stack_map[val_id] = -slot_offset
				slot_offset += 8
				continue
			}

			if instr.op == .call_sret {
				result_typ := g.m.type_store.types[val.typ]
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
			val_typ := g.m.type_store.types[val.typ]
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
				result_typ := g.m.type_store.types[val.typ]
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
		if g.m.globals.any(it.name == 'g_main_argc') {
			g.store_entry_arg_to_global(int(rdi), 'g_main_argc', 4)
		}
		if g.m.globals.any(it.name == 'g_main_argv') {
			g.store_entry_arg_to_global(int(rsi), 'g_main_argv', 8)
		}
	}

	// Move Params (ABI: RDI, RSI, RDX, RCX, R8, R9)
	// For sret functions, hidden pointer consumes RDI.
	abi_regs := [7, 6, 2, 1, 8, 9]
	arg_reg_base := if g.cur_func_abi_ret_indirect { 1 } else { 0 }
	num_reg_args := 6 - arg_reg_base
	if g.cur_func_abi_ret_indirect && g.sret_save_offset != 0 {
		asm_store_rbp_disp_reg(mut g, g.sret_save_offset, rdi)
	}
	for i, pid in func.params {
		is_indirect_param := i < g.cur_func_param_indirect.len
			&& g.cur_func_param_indirect[i]
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
		blk := g.m.blocks[blk_id]
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

fn (g &Gen) dump_function(func ssa.Function) {
	eprintln('SSA function ${func.name} ret=${func.typ}')
	for param in func.params {
		value := g.m.values[param]
		typ := g.m.type_store.types[value.typ]
		eprintln('SSA param ${param} typ=${value.typ} kind=${value.kind} type_kind=${typ.kind} size=${g.type_size(value.typ)} elem=${typ.elem_type} fields=${typ.fields} names=${typ.field_names}')
	}
	for block_id in func.blocks {
		eprintln('SSA block ${block_id}')
		for value_id in g.m.blocks[block_id].instrs {
			value := g.m.values[value_id]
			if value.kind != .instruction {
				eprintln('SSA ${value_id} kind=${value.kind} typ=${value.typ} name=${value.name}')
				continue
			}
			instr := g.m.instrs[value.index]
			mut operands := []string{}
			for i, operand_id in instr.operands {
				operand := g.m.values[operand_id]
				if instr.is_value_operand(i) {
					operands << '${operand_id}:${operand.kind}:${operand.typ}:${operand.name}'
				} else {
					operands << '${operand_id}'
				}
			}
			eprintln('SSA ${value_id} ${instr.op} typ=${value.typ} ops=${operands}')
		}
	}
}

// allocate_scalar_registers keeps the hottest scalar SSA values in
// callee-saved registers during production builds. Four registers preserve the
// required 16-byte stack alignment before calls without extra frame padding.
fn (mut g Gen) allocate_scalar_registers(func ssa.Function) {
	if !g.optimize_runtime {
		return
	}
	g.reg_stamp++
	stamp := g.reg_stamp
	registers := [int(rbx), int(r12), int(r13), int(r14)]
	mut candidates := []RegCandidate{}
	for block_id in func.blocks {
		for value_id in g.m.blocks[block_id].instrs {
			value := g.m.values[value_id]
			if value.kind != .instruction {
				continue
			}
			instr := g.m.instrs[value.index]
			for oi, operand_id in instr.operands {
				if !instr.is_value_operand(oi) || operand_id <= 0
					|| operand_id >= g.reg_use_counts.len {
					continue
				}
				if g.reg_use_stamps[operand_id] != stamp {
					g.reg_use_stamps[operand_id] = stamp
					g.reg_use_counts[operand_id] = 1
				} else {
					g.reg_use_counts[operand_id]++
				}
			}
		}
	}
	for param_id in func.params {
		if param_id <= 0 || param_id >= g.reg_seen_stamps.len
			|| g.reg_seen_stamps[param_id] == stamp {
			continue
		}
		g.reg_seen_stamps[param_id] = stamp
		use_count := if g.reg_use_stamps[param_id] == stamp {
			g.reg_use_counts[param_id]
		} else {
			0
		}
		if score := g.register_candidate_score(param_id, use_count) {
			candidates << RegCandidate{
				value_id: param_id
				score:    score + 2
			}
		}
	}
	for block_id in func.blocks {
		for value_id in g.m.blocks[block_id].instrs {
			if value_id <= 0 || value_id >= g.reg_seen_stamps.len
				|| g.reg_seen_stamps[value_id] == stamp {
				continue
			}
			g.reg_seen_stamps[value_id] = stamp
			use_count := if g.reg_use_stamps[value_id] == stamp {
				g.reg_use_counts[value_id]
			} else {
				0
			}
			if score := g.register_candidate_score(value_id, use_count) {
				candidates << RegCandidate{
					value_id: value_id
					score:    score
				}
			}
			value := g.m.values[value_id]
			if value.kind == .instruction {
				instr := g.m.instrs[value.index]
				for oi, operand_id in instr.operands {
					if !instr.is_value_operand(oi) || operand_id <= 0
						|| operand_id >= g.reg_seen_stamps.len {
						continue
					}
					if g.reg_seen_stamps[operand_id] == stamp {
						continue
					}
					g.reg_seen_stamps[operand_id] = stamp
					operand_use_count := if g.reg_use_stamps[operand_id] == stamp {
						g.reg_use_counts[operand_id]
					} else {
						0
					}
					if score := g.register_candidate_score(operand_id, operand_use_count) {
						candidates << RegCandidate{
							value_id: operand_id
							score:    score
						}
					}
				}
			}
		}
	}
	mut assigned := []bool{len: candidates.len}
	for register in registers {
		mut best := -1
		mut best_score := 0
		for index, candidate in candidates {
			if !assigned[index] && candidate.score > best_score {
				best = index
				best_score = candidate.score
			}
		}
		if best < 0 {
			break
		}
		assigned[best] = true
		g.reg_map[candidates[best].value_id] = register
		g.used_regs << register
	}
}

fn (g &Gen) register_candidate_score(value_id int, use_count int) ?int {
	if value_id <= 0 || value_id >= g.m.values.len {
		return none
	}
	value := g.m.values[value_id]
	if use_count < 1 || value.kind !in [.argument, .instruction, .phi_result] {
		return none
	}
	if value.typ <= 0 || value.typ >= g.m.type_store.types.len {
		return none
	}
	typ := g.m.type_store.types[value.typ]
	if typ.kind !in [.int_t, .float_t, .ptr_t] || g.type_size(value.typ) > 8 {
		return none
	}
	if value.kind == .instruction {
		instr := g.m.instrs[value.index]
		if instr.op in [.alloca, .inline_string_init, .struct_init, .insertvalue, .call_sret, .phi] {
			return none
		}
	}
	return use_count
}

fn (mut g Gen) gen_instr(val_id int) {
	instr := g.m.instrs[g.m.values[val_id].index]
	op := g.selected_opcode(instr)

	// Temps: 0=RAX, 1=RCX

	match op {
		.fadd, .fsub, .fmul, .fdiv, .frem {
			g.gen_float_binop(op, instr.operands[0], instr.operands[1], val_id)
		}
		.fptosi {
			src_id := instr.operands[0]
			g.load_float_bits_to_reg(src_id, 0)
			if g.is_f32_type(g.m.values[src_id].typ) {
				asm_movd_xmm0_eax(mut g)
				asm_cvttss2si_rax_xmm0(mut g)
			} else {
				asm_movq_xmm0_rax(mut g)
				asm_cvttsd2si_rax_xmm0(mut g)
			}
			g.store_reg_to_val(0, val_id)
		}
		.fptoui {
			src_id := instr.operands[0]
			g.load_float_bits_to_reg(src_id, 0)
			if g.is_f32_type(g.m.values[src_id].typ) {
				asm_movd_xmm0_eax(mut g)
				asm_mov_reg_imm32(mut g, rax, u32(0x5f000000)) // 2^63 as f32
				asm_movd_xmm_eax(mut g, 1)
				asm_ucomiss_xmm0_xmm1(mut g)
				asm_jb_rel32(mut g)
				jb_disp := g.elf.text_data.len
				g.emit_u32(0)
				asm_subss_xmm0_xmm1(mut g)
				asm_cvttss2si_rax_xmm0(mut g)
				asm_mov_reg_imm64(mut g, rcx, u64(0x8000000000000000))
				asm_add_rax_rcx(mut g)
				asm_jmp_rel32(mut g)
				jmp_disp := g.elf.text_data.len
				g.emit_u32(0)
				low_path := g.elf.text_data.len
				asm_cvttss2si_rax_xmm0(mut g)
				done := g.elf.text_data.len
				g.write_u32(jb_disp, u32(low_path - (jb_disp + 4)))
				g.write_u32(jmp_disp, u32(done - (jmp_disp + 4)))
			} else {
				asm_movq_xmm0_rax(mut g)
				asm_mov_reg_imm64(mut g, rax, u64(0x43e0000000000000)) // 2^63 as f64
				asm_movq_xmm_rax(mut g, 1)
				asm_ucomisd_xmm0_xmm1(mut g)
				asm_jb_rel32(mut g)
				jb_disp := g.elf.text_data.len
				g.emit_u32(0)
				asm_subsd_xmm0_xmm1(mut g)
				asm_cvttsd2si_rax_xmm0(mut g)
				asm_mov_reg_imm64(mut g, rcx, u64(0x8000000000000000))
				asm_add_rax_rcx(mut g)
				asm_jmp_rel32(mut g)
				jmp_disp := g.elf.text_data.len
				g.emit_u32(0)
				low_path := g.elf.text_data.len
				asm_cvttsd2si_rax_xmm0(mut g)
				done := g.elf.text_data.len
				g.write_u32(jb_disp, u32(low_path - (jb_disp + 4)))
				g.write_u32(jmp_disp, u32(done - (jmp_disp + 4)))
			}
			g.store_reg_to_val(0, val_id)
		}
		.sitofp {
			g.load_val_to_reg(0, instr.operands[0])
			if g.is_f32_type(g.m.values[val_id].typ) {
				asm_cvtsi2ss_xmm0_rax(mut g)
				asm_movd_eax_xmm0(mut g)
			} else {
				asm_cvtsi2sd_xmm0_rax(mut g)
				asm_movq_rax_xmm0(mut g)
			}
			g.store_reg_to_val(0, val_id)
		}
		.uitofp {
			src_id := instr.operands[0]
			src_typ_id := g.m.values[src_id].typ
			src_type := g.m.type_store.types[src_typ_id]
			g.load_val_to_reg(0, src_id)
			if src_type.kind == .int_t && src_type.width == 64 && src_type.is_unsigned {
				asm_test_rax_rax(mut g)
				asm_js_rel32(mut g)
				js_disp := g.elf.text_data.len
				g.emit_u32(0)
				if g.is_f32_type(g.m.values[val_id].typ) {
					asm_cvtsi2ss_xmm0_rax(mut g)
					asm_movd_eax_xmm0(mut g)
				} else {
					asm_cvtsi2sd_xmm0_rax(mut g)
					asm_movq_rax_xmm0(mut g)
				}
				asm_jmp_rel32(mut g)
				jmp_disp := g.elf.text_data.len
				g.emit_u32(0)
				high_path := g.elf.text_data.len
				// Convert (u >> 1) | (u & 1) as a signed value, then double it.
				asm_mov_reg_reg(mut g, rcx, rax)
				g.emit(0x48)
				g.emit(0xD1)
				g.emit(0xE8) // shr rax, 1
				g.emit(0x48)
				g.emit(0x83)
				g.emit(0xE1)
				g.emit(0x01) // and rcx, 1
				asm_or_rax_rcx(mut g)
				if g.is_f32_type(g.m.values[val_id].typ) {
					asm_cvtsi2ss_xmm0_rax(mut g)
					g.emit(0xF3)
					g.emit(0x0F)
					g.emit(0x58)
					g.emit(0xC0) // addss xmm0, xmm0
					asm_movd_eax_xmm0(mut g)
				} else {
					asm_cvtsi2sd_xmm0_rax(mut g)
					g.emit(0xF2)
					g.emit(0x0F)
					g.emit(0x58)
					g.emit(0xC0) // addsd xmm0, xmm0
					asm_movq_rax_xmm0(mut g)
				}
				done := g.elf.text_data.len
				g.write_u32(js_disp, u32(high_path - (js_disp + 4)))
				g.write_u32(jmp_disp, u32(done - (jmp_disp + 4)))
			} else {
				if g.is_f32_type(g.m.values[val_id].typ) {
					asm_cvtsi2ss_xmm0_rax(mut g)
					asm_movd_eax_xmm0(mut g)
				} else {
					asm_cvtsi2sd_xmm0_rax(mut g)
					asm_movq_rax_xmm0(mut g)
				}
			}
			g.store_reg_to_val(0, val_id)
		}
		.add, .sub, .mul, .sdiv, .udiv, .srem, .urem, .and_, .or_, .xor, .shl, .ashr, .lshr, .eq,
		.ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
			// Check if this is a float comparison
			lhs_typ := g.m.values[instr.operands[0]].typ
			is_float_cmp := op in [.eq, .ne, .lt, .gt, .le, .ge] && lhs_typ > 0
				&& lhs_typ < g.m.type_store.types.len
				&& g.m.type_store.types[lhs_typ].kind == .float_t

			if is_float_cmp {
				if g.is_f32_type(lhs_typ) {
					g.load_f32_operand_bits(instr.operands[0], 0)
					asm_movd_xmm0_eax(mut g)
					g.load_f32_operand_bits(instr.operands[1], 1)
					asm_movd_xmm1_ecx(mut g)
					asm_ucomiss_xmm0_xmm1(mut g)
				} else {
					g.load_float_bits_to_reg(instr.operands[0], 0)
					asm_movq_xmm0_rax(mut g)
					g.load_float_bits_to_reg(instr.operands[1], 1)
					asm_movq_xmm1_rcx(mut g)
					asm_ucomisd_xmm0_xmm1(mut g)
				}
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
				asm_setcc_al(mut g, cc)
				if op == .ne {
					asm_setp_cl(mut g)
					asm_or_al_cl(mut g)
				} else {
					asm_setnp_cl(mut g)
					asm_and_al_cl(mut g)
				}
				asm_movzx_rax_al(mut g)
			} else {
				if op in [.shl, .ashr, .lshr] {
					g.gen_integer_shift(op, instr.operands[0], instr.operands[1], val_id)
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
			}
			// Narrow integer canonicalization
			result_typ_id := g.m.values[val_id].typ
			if result_typ_id > 0 && result_typ_id < g.m.type_store.types.len {
				result_typ := g.m.type_store.types[result_typ_id]
				if result_typ.kind == .int_t && result_typ.width > 0 && result_typ.width < 64 {
					g.canonicalize_narrow_int(result_typ)
				}
			}
			g.store_reg_to_val(0, val_id)
		}
		.store {
			src_id := instr.operands[0]
			ptr_id := instr.operands[1]
			src_typ := g.m.values[src_id].typ
			src_type_info := g.m.type_store.types[src_typ]
			src_size := g.type_size(src_typ)

			// Determine destination element type and size from pointer type
			mut dst_elem_size := 0
			mut dst_is_struct := false
			mut dst_type := ssa.TypeID(0)
			ptr_val := g.m.values[ptr_id]
			if ptr_val.typ > 0 && ptr_val.typ < g.m.type_store.types.len {
				ptr_typ := g.m.type_store.types[ptr_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
					&& ptr_typ.elem_type < g.m.type_store.types.len {
					dst_type = ptr_typ.elem_type
					elem_typ := g.m.type_store.types[ptr_typ.elem_type]
					dst_elem_size = g.type_size(ptr_typ.elem_type)
					dst_is_struct = elem_typ.kind in [.struct_t, .array_t]
				}
			}
			if g.is_zero_const(src_id) && g.is_aggregate_type(dst_type) {
				g.load_val_to_reg(int(r11), ptr_id)
				asm_xor_reg_reg(mut g, rax)
				for off := 0; off < dst_elem_size; off += 8 {
					asm_mov_mem_base_disp_rax(mut g, r11, off)
				}
				return
			}

			if (src_type_info.kind in [.struct_t, .array_t] && src_size > 16)
				|| (dst_is_struct && dst_elem_size > 16) {
				// Large struct copy
				copy_size := if dst_elem_size > 16 { dst_elem_size } else { src_size }
				g.load_struct_src_address_to_reg(int(r10), src_id, src_typ)
				g.load_val_to_reg(int(r11), ptr_id)
				g.emit_copy_memory(r11, r10, copy_size)
			} else if dst_is_struct && dst_elem_size > 0 && dst_elem_size <= 16 {
				// Small struct copy (2 words or less)
				num_chunks := (dst_elem_size + 7) / 8
				if num_chunks > 1 {
					g.load_address_of_val_to_reg(int(r10), src_id)
					g.load_val_to_reg(int(r11), ptr_id)
					g.emit_copy_memory(r11, r10, dst_elem_size)
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
			result_val := g.m.values[val_id]
			result_type := g.m.type_store.types[result_val.typ]
			mut load_size := g.type_size(result_val.typ)
			mut is_signed := false
			ptr_val := g.m.values[ptr_id]
			if ptr_val.typ > 0 && ptr_val.typ < g.m.type_store.types.len {
				ptr_typ := g.m.type_store.types[ptr_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0
					&& ptr_typ.elem_type < g.m.type_store.types.len {
					elem_typ := g.m.type_store.types[ptr_typ.elem_type]
					elem_size := g.type_size(ptr_typ.elem_type)
					if result_type.kind == .ptr_t {
						load_size = 8
					} else if result_type.kind == .int_t {
						if result_type.width > 0 {
							load_size = (result_type.width + 7) / 8
						}
						is_signed = !result_type.is_unsigned
					} else if elem_typ.kind in [.struct_t, .array_t] && elem_size > 8 {
						// Large struct load: copy bytes to stack slot
						g.load_val_to_reg(int(r10), ptr_id)
						if result_off := g.stack_map[val_id] {
							asm_lea_rbp_disp(mut g, r11, result_off)
							g.emit_copy_memory(r11, r10, elem_size)
						} else {
							// Single qword fallback
							asm_mov_rax_mem_base_disp(mut g, r10, 0)
							g.store_reg_to_val(0, val_id)
						}
						return
					}
				}
			}
			if load_size <= 0 {
				load_size = 8
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
			// Zero-initialize large fixed array allocas.
			// The SSA builder skips element-by-element zero-init for arrays > 16 elements,
			// so the codegen must bulk-zero them here (matching the ARM64 backend).
			alloca_val := g.m.values[val_id]
			if alloca_val.typ > 0 && alloca_val.typ < g.m.type_store.types.len {
				alloca_ptr_type := g.m.type_store.types[alloca_val.typ]
				if alloca_ptr_type.kind == .ptr_t && alloca_ptr_type.elem_type > 0
					&& alloca_ptr_type.elem_type < g.m.type_store.types.len {
					elem_typ := g.m.type_store.types[alloca_ptr_type.elem_type]
					if elem_typ.kind == .array_t && elem_typ.len > 16 {
						arr_size := g.type_size(alloca_ptr_type.elem_type)
						if arr_size > 0 {
							// rax already has the alloca address from lea above.
							// Use rep stosb: rdi=dest, rcx=count, al=0
							asm_mov_reg_reg(mut g, rdi, rax)
							asm_xor_reg_reg(mut g, rax) // al = 0
							asm_mov_reg_imm32(mut g, rcx, u32(arr_size))
							asm_rep_stosb(mut g)
							// Reload alloca address into rax (rep stosb clobbers rdi)
							if off >= -128 && off <= 127 {
								asm_lea_rax_rbp_disp8(mut g, i8(off))
							} else {
								asm_lea_rax_rbp_disp32(mut g, off)
							}
						}
					}
				}
			}
			g.store_reg_to_val(0, val_id)
		}
		.heap_alloc {
			// Heap-allocate memory: calloc(1, sizeof(T))
			mut alloc_size := 8
			ha_val := g.m.values[val_id]
			if ha_val.typ > 0 && ha_val.typ < g.m.type_store.types.len {
				ptr_typ := g.m.type_store.types[ha_val.typ]
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
			// V3's SSA builder already computes byte offsets for struct fields
			// and scaled array indexes, matching the ARM64 backend.
			g.load_val_to_reg(0, instr.operands[0])
			g.load_val_to_reg(1, instr.operands[1])
			asm_add_rax_rcx(mut g)
			g.store_reg_to_val(0, val_id)
		}
		.call, .call_sret, .call_indirect, .go_call, .spawn_call {
			g.gen_call(val_id, instr)
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
						g.emit_copy_memory(rdi, r10, ret_size)
					}
				}
				// SysV returns sret pointer in RAX.
				asm_mov_reg_reg(mut g, rax, rdi)
			} else if instr.operands.len > 0 {
				g.load_val_to_reg(0, instr.operands[0])
			} else {
				asm_xor_eax_eax(mut g)
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
			target_idx := int(instr.operands[0])
			g.emit_phi_edge_copies(instr.block, target_idx)
			g.emit_jmp(target_idx)
		}
		.br {
			cond_id := instr.operands[0]
			true_blk := int(instr.operands[1])
			false_blk := int(instr.operands[2])

			// Test condition register directly if register-allocated
			if reg := g.reg_map[cond_id] {
				asm_test_reg_reg(mut g, Reg(reg))
			} else {
				g.load_val_to_reg(0, cond_id)
				asm_test_rax_rax(mut g)
			}

			// Route each edge through a small stub so phi copies run only for the
			// selected successor.
			asm_jne_rel32(mut g)
			patch_offset := g.elf.text_data.len
			g.emit_u32(0)
			g.emit_phi_edge_copies(instr.block, false_blk)
			g.emit_jmp(false_blk)
			true_stub := g.elf.text_data.len - g.curr_offset
			rel := true_stub - (patch_offset + 4 - g.curr_offset)
			g.write_u32(patch_offset, u32(rel))
			g.emit_phi_edge_copies(instr.block, true_blk)
			g.emit_jmp(true_blk)
		}
		.switch_ {
			g.load_val_to_reg(0, instr.operands[0]) // RAX
			for i := 2; i < instr.operands.len; i += 2 {
				g.load_val_to_reg(1, instr.operands[i])
				asm_cmp_rax_rcx(mut g)
				asm_je_rel32(mut g)
				target_idx := int(instr.operands[i + 1])
				if off := g.block_offsets[target_idx] {
					rel := off - (g.elf.text_data.len - g.curr_offset + 4)
					g.emit_u32(u32(rel))
				} else {
					g.record_pending_label(target_idx)
					g.emit_u32(0)
				}
			}
			def_idx := int(instr.operands[1])
			g.emit_jmp(def_idx)
		}
		.assign {
			dest_id := instr.operands[0]
			src_id := instr.operands[1]
			src_typ := g.m.values[src_id].typ
			src_size := g.type_size(src_typ)
			src_type_info := g.m.type_store.types[src_typ]
			// Multi-word copy for structs/strings (e.g. phi-lowered copies).
			// Both source and dest must have addressable stack slots.
			src_has_slot := src_id in g.stack_map || src_id in g.alloca_offsets
				|| g.m.values[src_id].kind == .string_literal
			dest_has_slot := dest_id in g.stack_map || dest_id in g.alloca_offsets
			if src_size > 8 && src_type_info.kind in [.struct_t, .array_t] && src_has_slot
				&& dest_has_slot {
				g.load_address_of_val_to_reg(int(r10), src_id)
				g.load_address_of_val_to_reg(int(r11), dest_id)
				g.emit_copy_memory(r11, r10, src_size)
			} else {
				g.load_val_to_reg(0, src_id)
				g.store_reg_to_val(0, dest_id)
			}
		}
		.trunc, .zext {
			if instr.operands.len > 0 {
				// Check if float-to-float conversion
				src_val := g.m.values[instr.operands[0]]
				src_is_float := src_val.typ > 0 && src_val.typ < g.m.type_store.types.len
					&& g.m.type_store.types[src_val.typ].kind == .float_t
				dst_is_float := g.m.values[val_id].typ > 0
					&& g.m.values[val_id].typ < g.m.type_store.types.len
					&& g.m.type_store.types[g.m.values[val_id].typ].kind == .float_t

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
						if src_typ_id > 0 && src_typ_id < g.m.type_store.types.len {
							src_typ := g.m.type_store.types[src_typ_id]
							if src_typ.kind == .int_t && src_typ.width > 0 && src_typ.width < 64 {
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
						dst_typ_id := g.m.values[val_id].typ
						if dst_typ_id > 0 && dst_typ_id < g.m.type_store.types.len {
							dst_typ := g.m.type_store.types[dst_typ_id]
							if dst_typ.kind == .int_t && dst_typ.width > 0 && dst_typ.width < 64 {
								if dst_typ.width == 1 {
									asm_and_rax_imm32(mut g, 1)
								} else if dst_typ.width <= 8 {
									if dst_typ.is_unsigned {
										asm_movzx_rax_al(mut g)
									} else {
										asm_movsx_rax_al(mut g)
									}
								} else if dst_typ.width <= 16 {
									if dst_typ.is_unsigned {
										asm_movzx_rax_ax(mut g)
									} else {
										asm_movsx_rax_ax(mut g)
									}
								} else if dst_typ.width <= 32 {
									if dst_typ.is_unsigned {
										g.emit(0x89)
										g.emit(0xC0)
									} else {
										asm_movsxd_rax_eax(mut g)
									}
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
				src_type_id := g.m.values[src_id].typ
				dest_type_id := g.m.values[val_id].typ
				if g.is_string_type(src_type_id) && dest_type_id > 0
					&& dest_type_id < g.m.type_store.types.len
					&& g.m.type_store.types[dest_type_id].kind == .ptr_t {
					g.load_address_of_val_to_reg(int(r10), src_id)
					asm_mov_rax_mem_base_disp(mut g, r10, 0)
					g.store_reg_to_val(0, val_id)
					return
				}
				if g.is_float_type(src_type_id) && g.is_float_type(g.m.values[val_id].typ)
					&& g.is_f32_type(src_type_id) != g.is_f32_type(g.m.values[val_id].typ) {
					g.load_float_bits_to_reg(src_id, 0)
					if g.is_f32_type(src_type_id) {
						asm_movd_xmm0_eax(mut g)
						asm_cvtss2sd_xmm0(mut g)
						asm_movq_rax_xmm0(mut g)
					} else {
						asm_movq_xmm0_rax(mut g)
						asm_cvtsd2ss_xmm0(mut g)
						asm_movd_eax_xmm0(mut g)
					}
					g.store_reg_to_val(0, val_id)
					return
				}
				// Check for aggregate bitcast that needs byte copy
				dest_typ_id := g.m.values[val_id].typ
				mut copied_aggregate := false
				if dest_typ_id > 0 && dest_typ_id < g.m.type_store.types.len {
					dest_typ := g.m.type_store.types[dest_typ_id]
					dest_size := g.type_size(dest_typ_id)
					if dest_typ.kind in [.struct_t, .array_t] && dest_size > 8 {
						if dest_off := g.stack_map[val_id] {
							if src_off := g.stack_map[src_id] {
								asm_lea_rbp_disp(mut g, r10, src_off)
								asm_lea_rbp_disp(mut g, r11, dest_off)
								g.emit_copy_memory(r11, r10, dest_size)
								copied_aggregate = true
							}
						}
					}
				}
				if !copied_aggregate {
					g.load_val_to_reg(0, src_id)
					// For sext, sign-extend narrow source
					if op == .sext {
						src_typ_id := g.m.values[src_id].typ
						if src_typ_id > 0 && src_typ_id < g.m.type_store.types.len {
							src_typ := g.m.type_store.types[src_typ_id]
							if src_typ.kind == .int_t && src_typ.width > 0 && src_typ.width < 64 {
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
			idx_val := g.m.values[instr.operands[1]]
			idx := idx_val.name.int()

			tuple_val := g.m.values[tuple_id]

			// String/C-string literals need materialization before field extraction.
			// load_val_to_reg writes {str, len, is_lit} into the stack slot;
			// without this, extractvalue reads uninitialized memory.
			if tuple_val.kind in [.string_literal, .c_string_literal] {
				g.load_val_to_reg(0, tuple_id)
			}

			mut field_byte_off := idx * 8
			mut field_elem_size := 8

			if tuple_val.typ > 0 && tuple_val.typ < g.m.type_store.types.len {
				tuple_typ := g.m.type_store.types[tuple_val.typ]
				if tuple_typ.kind == .struct_t && idx >= 0 {
					field_byte_off = g.struct_field_offset_bytes(tuple_val.typ, idx)
					if idx < tuple_typ.fields.len {
						field_elem_size = g.type_size(tuple_typ.fields[idx])
						if field_elem_size <= 0 {
							field_elem_size = 8
						}
					}
				} else if tuple_typ.kind == .array_t && idx >= 0 {
					field_elem_size = g.type_size(tuple_typ.elem_type)
					if field_elem_size <= 0 {
						field_elem_size = 8
					}
					field_byte_off = idx * field_elem_size
				}
			}

			// Also check if tuple is a load from a struct pointer
			mut scalar_load_agg_src_ptr_id := 0
			if tuple_val.kind == .instruction {
				tuple_ins := g.m.instrs[tuple_val.index]
				if g.selected_opcode(tuple_ins) == .load && tuple_ins.operands.len > 0 {
					load_ptr_id := tuple_ins.operands[0]
					if load_ptr_id > 0 && load_ptr_id < g.m.values.len {
						load_ptr_val := g.m.values[load_ptr_id]
						if load_ptr_val.typ > 0 && load_ptr_val.typ < g.m.type_store.types.len {
							load_ptr_typ := g.m.type_store.types[load_ptr_val.typ]
							if load_ptr_typ.kind == .ptr_t && load_ptr_typ.elem_type > 0
								&& load_ptr_typ.elem_type < g.m.type_store.types.len {
								load_elem_typ := g.m.type_store.types[load_ptr_typ.elem_type]
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
			if instr.typ > 0 && instr.typ < g.m.type_store.types.len {
				result_typ := g.m.type_store.types[instr.typ]
				result_size = g.type_size(instr.typ)
				result_is_agg = result_typ.kind in [.struct_t, .array_t]
			}

			// Tracing disabled

			if scalar_load_agg_src_ptr_id > 0 {
				// Load from struct pointer + field offset
				g.load_val_to_reg(int(r10), scalar_load_agg_src_ptr_id)
				if result_is_agg && result_size > 8 {
					if result_off := g.stack_map[val_id] {
						asm_lea_rbp_disp(mut g, r11, result_off)
						g.emit_copy_memory(r11, r10, result_size)
					}
				} else {
					g.emit_sized_load_base_disp(r10, field_byte_off, field_elem_size)
					g.store_reg_to_val(0, val_id)
				}
			} else if tuple_off := g.stack_map[tuple_id] {
				// Sumtype wrapper: {_tag, _data} where _data is a payload pointer.
				// extractvalue with idx beyond wrapper fields means "read from payload".
				mut handled_wrapper := false
				if tuple_val.typ > 0 && tuple_val.typ < g.m.type_store.types.len {
					ttyp := g.m.type_store.types[tuple_val.typ]
					if ttyp.kind == .struct_t && ttyp.field_names.len == 2
						&& ttyp.field_names[0] == '_tag' && ttyp.field_names[1] == '_data'
						&& idx >= ttyp.fields.len {
						// Load _data pointer from wrapper on stack
						data_off := g.struct_field_offset_bytes(tuple_val.typ, 1)
						asm_load_reg_rbp_disp(mut g, r10, tuple_off + data_off)
						payload_field_idx := idx - ttyp.fields.len
						payload_off := payload_field_idx * 8
						if result_is_agg && result_size > 8 {
							if result_off := g.stack_map[val_id] {
								num_chunks := (result_size + 7) / 8
								for i := 0; i < num_chunks; i++ {
									asm_mov_rax_mem_base_disp(mut g, r10, payload_off + i * 8)
									asm_store_rbp_disp_reg(mut g, result_off + i * 8,
										rax)
								}
								handled_wrapper = true
							}
						} else {
							// Scalar: load from payload_ptr + offset
							asm_mov_rax_mem_base_disp(mut g, r10, payload_off)
							g.store_reg_to_val(0, val_id)
							handled_wrapper = true
						}
					}
				}
				if !handled_wrapper {
					// Load from stack slot + field offset
					if result_is_agg && result_size > 8 {
						if result_off := g.stack_map[val_id] {
							asm_lea_rbp_disp(mut g, r10, tuple_off + field_byte_off)
							asm_lea_rbp_disp(mut g, r11, result_off)
							g.emit_copy_memory(r11, r10, result_size)
						}
					} else {
						// Use sized load for struct fields to avoid reading adjacent fields
						g.emit_sized_load_base_disp(Reg(5), tuple_off + field_byte_off,
							field_elem_size)
						g.store_reg_to_val(0, val_id)
					}
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
			idx_val := g.m.values[instr.operands[2]]
			idx := idx_val.name.int()

			result_offset := g.stack_map[val_id]
			tuple_typ := g.m.type_store.types[instr.typ]
			tuple_size := g.type_size(instr.typ)
			actual_tuple_size := if tuple_size > 0 { tuple_size } else { 8 }
			mut elem_off := idx * 8
			if tuple_typ.kind == .struct_t && idx >= 0 && idx < tuple_typ.fields.len {
				elem_off = g.struct_field_offset_bytes(instr.typ, idx)
			} else if tuple_typ.kind == .array_t && idx >= 0 {
				elem_size := g.type_size(tuple_typ.elem_type)
				if elem_size > 0 {
					elem_off = idx * elem_size
				}
			}

			// Tracing disabled

			// Copy existing tuple data
			tuple_val := g.m.values[tuple_id]
			if !(tuple_val.kind == .constant && tuple_val.name == 'undef') {
				if tuple_offset := g.stack_map[tuple_id] {
					asm_lea_rbp_disp(mut g, r10, tuple_offset)
					asm_lea_rbp_disp(mut g, r11, result_offset)
					g.emit_copy_memory(r11, r10, actual_tuple_size)
				} else {
					// Zero-init
					g.emit_zero_stack_range(result_offset, actual_tuple_size)
				}
			} else {
				// Zero-init for undef
				g.emit_zero_stack_range(result_offset, actual_tuple_size)
			}

			// Store the new element
			mut elem_typ_id := ssa.TypeID(0)
			if tuple_typ.kind == .struct_t && idx >= 0 && idx < tuple_typ.fields.len {
				elem_typ_id = tuple_typ.fields[idx]
			} else if elem_id > 0 && elem_id < g.m.values.len {
				elem_typ_id = g.m.values[elem_id].typ
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
				// Materialize string/c_string literals before reading from stack slot
				elem_val := g.m.values[elem_id]
				if elem_val.kind in [.string_literal, .c_string_literal] {
					g.load_val_to_reg(0, elem_id)
				}
				g.load_address_of_val_to_reg(int(r10), elem_id)
				asm_lea_rbp_disp(mut g, r11, result_offset + elem_off)
				g.emit_copy_memory(r11, r10, elem_size)
			}
		}
		.struct_init {
			// Create struct from field values
			result_offset := g.stack_map[val_id]
			struct_typ := g.m.type_store.types[instr.typ]
			struct_size := g.type_size(instr.typ)
			zero_size := if struct_size > 0 { struct_size } else { 8 }

			// Zero-initialize the entire struct first
			g.emit_zero_stack_range(result_offset, zero_size)

			// Store each field value at its proper offset
			for fi, field_id in instr.operands {
				mut field_off := fi * 8
				if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
					field_off = g.struct_field_offset_bytes(instr.typ, fi)
				}

				mut field_typ_id := ssa.TypeID(0)
				if struct_typ.kind == .struct_t && fi >= 0 && fi < struct_typ.fields.len {
					field_typ_id = struct_typ.fields[fi]
				} else if struct_typ.kind == .array_t {
					field_typ_id = struct_typ.elem_type
				} else if field_id > 0 && field_id < g.m.values.len {
					field_typ_id = g.m.values[field_id].typ
				}
				mut field_size := if field_typ_id > 0 { g.type_size(field_typ_id) } else { 8 }
				if field_size <= 0 {
					field_size = 8
				}
				if struct_typ.kind == .array_t {
					field_off = fi * field_size
				}

				// Skip zero constants (already zeroed)
				field_val := g.m.values[field_id]
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
					// Materialize string/c_string literals before reading from stack slot
					if field_val.kind in [.string_literal, .c_string_literal] {
						g.load_val_to_reg(0, field_id)
					}
					g.load_address_of_val_to_reg(int(r10), field_id)
					asm_lea_rbp_disp(mut g, r11, result_offset + field_off)
					g.emit_copy_memory(r11, r10, field_size)
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
			is_lit_off, is_lit_size := g.struct_field_offset_and_size(instr.typ, 2, 16,
				8)

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
		.select {
			if instr.operands.len >= 3 {
				g.load_val_to_reg(0, instr.operands[0])
				g.load_val_to_reg(1, instr.operands[2])
				g.load_val_to_reg(2, instr.operands[1])
				asm_test_rax_rax(mut g)
				asm_cmovne_rcx_rdx(mut g)
				asm_mov_reg_reg(mut g, rax, rcx)
				g.store_reg_to_val(0, val_id)
			}
		}
		.phi {
			// Phi values are copied on each incoming CFG edge.
		}
		.unreachable {
			asm_ud2(mut g)
		}
		else {
			panic('x64: unsupported op ${op}')
		}
	}
}

fn (mut g Gen) gen_integer_shift(op ssa.OpCode, lhs_id int, rhs_id int, result_id int) {
	width := g.m.type_store.types[g.m.values[lhs_id].typ].width
	width_const := if width > 0 && width < 64 { width } else { 64 }
	width_value := g.m.get_or_add_const(g.m.values[rhs_id].typ, '${width_const}')
	g.load_val_to_reg(0, rhs_id)
	g.load_val_to_reg(1, width_value)
	asm_cmp_rax_rcx(mut g)
	g.emit(0x0F)
	g.emit(0x83) // jae oversized_shift
	jae_disp := g.elf.text_data.len
	g.emit_u32(0)
	g.load_val_to_reg(0, lhs_id)
	g.load_val_to_reg(1, rhs_id)
	match op {
		.shl { asm_shl_rax_cl(mut g) }
		.ashr { asm_sar_rax_cl(mut g) }
		.lshr { asm_shr_rax_cl(mut g) }
		else {}
	}
	asm_jmp_rel32(mut g)
	jmp_disp := g.elf.text_data.len
	g.emit_u32(0)
	oversized := g.elf.text_data.len
	if op == .ashr {
		g.load_val_to_reg(0, lhs_id)
		asm_cqo(mut g)
		asm_mov_rax_rdx(mut g)
	} else {
		asm_xor_eax_eax(mut g)
	}
	done := g.elf.text_data.len
	g.write_u32(jae_disp, u32(oversized - (jae_disp + 4)))
	g.write_u32(jmp_disp, u32(done - (jmp_disp + 4)))
	g.store_reg_to_val(0, result_id)
}

fn (mut g Gen) gen_call(val_id int, instr ssa.Instruction) {
	if instr.operands.len == 0 {
		return
	}
	if g.call_is_c_extern(instr) {
		g.gen_c_extern_call(val_id, instr)
		return
	}
	is_indirect := instr.op == .call_indirect
	ret_indirect := g.is_indirect_return(instr.typ)
	abi_regs := if ret_indirect {
		[6, 2, 1, 8, 9]
	} else {
		[7, 6, 2, 1, 8, 9]
	}
	num_args := instr.operands.len - 1
	max_reg_args := abi_regs.len

	mut stack_args := 0
	if num_args > max_reg_args {
		stack_args = num_args - max_reg_args
		if stack_args % 2 == 1 {
			asm_push(mut g, rax)
		}
		for i := num_args; i > max_reg_args; i-- {
			g.load_call_arg_to_reg(0, instr.operands[i], i - 1, instr)
			asm_push(mut g, rax)
		}
	}

	if ret_indirect {
		g.load_address_of_val_to_reg(int(rdi), val_id)
	}
	for i in 1 .. instr.operands.len {
		if i - 1 < max_reg_args {
			g.load_call_arg_to_reg(abi_regs[i - 1], instr.operands[i], i - 1, instr)
		}
	}

	fn_val := g.m.values[instr.operands[0]]
	asm_xor_eax_eax(mut g)
	if is_indirect {
		g.load_val_to_reg(int(r10), instr.operands[0])
		asm_call_r10(mut g)
	} else if fn_val.name != '' && fn_val.kind in [.unknown, .func_ref] {
		asm_call_rel32(mut g)
		sym_idx := g.elf.add_undefined(fn_val.name)
		g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, r_x86_64_plt32, -4)
		g.emit_u32(0)
	} else {
		g.load_val_to_reg(int(r10), instr.operands[0])
		asm_call_r10(mut g)
	}

	if stack_args > 0 {
		cleanup := (stack_args + (stack_args % 2)) * 8
		if cleanup <= 127 {
			asm_add_rsp_imm8(mut g, u8(cleanup))
		} else {
			asm_add_rsp_imm32(mut g, u32(cleanup))
		}
	}

	if instr.typ != 0 {
		ret_kind := g.m.type_store.types[instr.typ].kind
		if ret_kind != .void_t && !ret_indirect {
			g.store_reg_to_val(0, val_id)
		}
	}
}

fn (g &Gen) call_is_c_extern(instr ssa.Instruction) bool {
	if instr.operands.len == 0 {
		return false
	}
	callee := g.m.values[instr.operands[0]]
	return callee.kind == .func_ref && callee.index >= 0 && callee.index < g.m.funcs.len
		&& g.m.funcs[callee.index].is_c_extern
}

// gen_c_extern_call applies the SysV scalar argument convention used by C
// functions. V-internal helpers keep their compact stack-based convention, but
// mixing both up sends floating arguments to integer registers (e.g. pow).
fn (mut g Gen) gen_c_extern_call(val_id int, instr ssa.Instruction) {
	is_indirect := instr.op == .call_indirect
	ret_indirect := g.is_indirect_return(instr.typ)
	gp_regs := [7, 6, 2, 1, 8, 9]
	mut gp_index := if ret_indirect { 1 } else { 0 }

	mut args := []CCallArg{}
	mut stack_args := 0
	mut float_args := 0
	for arg_index in 1 .. instr.operands.len {
		value_id := instr.operands[arg_index]
		param_index := arg_index - 1
		mut arg_typ := g.call_param_type(instr, param_index)
		if arg_typ == 0 {
			arg_typ = g.m.values[value_id].typ
		}
		if g.is_float_type(arg_typ) {
			if float_args < 8 {
				args << CCallArg{
					value_id: value_id
					typ:      arg_typ
					reg:      float_args
				}
				float_args++
			} else {
				args << CCallArg{
					value_id: value_id
					typ:      arg_typ
					on_stack: true
				}
				stack_args++
			}
		} else if gp_index < gp_regs.len {
			args << CCallArg{
				value_id: value_id
				typ:      arg_typ
				reg:      gp_regs[gp_index]
			}
			gp_index++
		} else {
			args << CCallArg{
				value_id: value_id
				typ:      arg_typ
				on_stack: true
			}
			stack_args++
		}
	}

	if ret_indirect {
		g.load_address_of_val_to_reg(int(rdi), val_id)
	}
	padding := stack_args % 2
	if padding == 1 {
		asm_push(mut g, rax)
	}
	for i := args.len - 1; i >= 0; i-- {
		arg := args[i]
		if arg.on_stack {
			g.load_val_to_reg(0, arg.value_id)
			asm_push(mut g, rax)
		}
	}
	for arg in args {
		if arg.on_stack {
			continue
		}
		if g.is_float_type(arg.typ) {
			if g.is_f32_type(arg.typ) {
				g.load_f32_operand_bits(arg.value_id, 0)
				asm_movd_xmm_eax(mut g, u8(arg.reg))
			} else {
				g.load_float_bits_to_reg(arg.value_id, 0)
				asm_movq_xmm_rax(mut g, u8(arg.reg))
			}
		} else {
			g.load_call_arg_to_reg(arg.reg, arg.value_id, 0, instr)
		}
	}

	asm_xor_eax_eax(mut g)
	if float_args > 0 {
		asm_mov_reg_imm32(mut g, rax, u32(float_args))
	}
	fn_val := g.m.values[instr.operands[0]]
	if is_indirect {
		g.load_val_to_reg(int(r10), instr.operands[0])
		asm_call_r10(mut g)
	} else {
		asm_call_rel32(mut g)
		sym_idx := g.elf.add_undefined(fn_val.name)
		g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, r_x86_64_plt32, -4)
		g.emit_u32(0)
	}

	if stack_args + padding > 0 {
		cleanup := (stack_args + padding) * 8
		if cleanup <= 127 {
			asm_add_rsp_imm8(mut g, u8(cleanup))
		} else {
			asm_add_rsp_imm32(mut g, u32(cleanup))
		}
	}
	if instr.typ != 0 && !ret_indirect {
		ret_kind := g.m.type_store.types[instr.typ].kind
		if ret_kind == .float_t {
			if g.is_f32_type(instr.typ) {
				asm_movd_eax_xmm0(mut g)
			} else {
				asm_movq_rax_xmm0(mut g)
			}
		}
		if ret_kind == .int_t {
			// SysV leaves upper bits unspecified for narrow integer returns.
			g.canonicalize_narrow_int(g.m.type_store.types[instr.typ])
		}
		if ret_kind != .void_t {
			g.store_reg_to_val(0, val_id)
		}
	}
}

fn (g Gen) selected_opcode(instr ssa.Instruction) ssa.OpCode {
	return instr.op
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

// emit_phi_edge_copies materializes the phi values selected by one CFG edge.
fn (mut g Gen) emit_phi_edge_copies(from_blk int, to_blk int) {
	if to_blk <= 0 || to_blk >= g.m.blocks.len {
		return
	}
	for value_id in g.m.blocks[to_blk].instrs {
		value := g.m.values[value_id]
		if value.kind != .instruction {
			continue
		}
		instr := g.m.instrs[value.index]
		if instr.op != .phi {
			break
		}
		for i := 0; i + 1 < instr.operands.len; i += 2 {
			if int(instr.operands[i + 1]) == from_blk {
				g.emit_phi_copy_value(instr.operands[i], value_id)
				break
			}
		}
	}
}

fn (mut g Gen) emit_phi_copy_value(src_id int, dst_id int) {
	if src_id <= 0 || src_id >= g.m.values.len {
		return
	}
	dst := g.m.values[dst_id]
	size := g.type_size(dst.typ)
	if size > 8 && g.is_aggregate_type(dst.typ) {
		g.load_address_of_val_to_reg(int(r10), src_id)
		g.load_address_of_val_to_reg(int(r11), dst_id)
		g.emit_copy_memory(r11, r10, size)
		return
	}
	g.load_val_to_reg(0, src_id)
	g.store_reg_to_val(0, dst_id)
}

fn (mut g Gen) load_call_arg_to_reg(reg int, val_id int, arg_idx int, instr ssa.Instruction) {
	is_indirect := g.call_arg_is_indirect(instr, arg_idx)
	if is_indirect {
		g.load_address_of_val_to_reg(reg, val_id)
		return
	}
	param_type := g.call_param_type(instr, arg_idx)
	value := g.m.values[val_id]
	if param_type > 0 && param_type < g.m.type_store.types.len {
		param := g.m.type_store.types[param_type]
		if param.kind == .ptr_t && param.elem_type == value.typ
			&& g.is_aggregate_type(value.typ) {
			g.load_address_of_val_to_reg(reg, val_id)
			return
		}
	}
	g.load_val_to_reg(reg, val_id)
}

fn (g &Gen) call_arg_is_indirect(instr ssa.Instruction, arg_idx int) bool {
	return g.is_indirect_param(g.call_param_type(instr, arg_idx))
}

// call_param_type resolves the callee's declared ABI type. Using the actual SSA
// value type here misclassifies a bitcast such as &string -> &u8 as a
// by-value string aggregate.
fn (g &Gen) call_param_type(instr ssa.Instruction, arg_idx int) ssa.TypeID {
	if instr.operands.len == 0 {
		return 0
	}
	callee := g.m.values[instr.operands[0]]
	if callee.kind == .func_ref && callee.index >= 0 && callee.index < g.m.funcs.len {
		params := g.m.funcs[callee.index].params
		if arg_idx >= 0 && arg_idx < params.len {
			return g.m.values[params[arg_idx]].typ
		}
	} else if callee.typ > 0 && callee.typ < g.m.type_store.types.len {
		callee_type := g.m.type_store.types[callee.typ]
		if callee_type.kind == .func_t && arg_idx >= 0 && arg_idx < callee_type.params.len {
			return callee_type.params[arg_idx]
		}
	}
	operand_idx := arg_idx + 1
	if operand_idx >= 0 && operand_idx < instr.operands.len {
		return g.m.values[instr.operands[operand_idx]].typ
	}
	return 0
}

fn (mut g Gen) load_struct_src_address_to_reg(reg int, val_id int, expected_struct_typ int) {
	val := g.m.values[val_id]
	if val.typ > 0 && val.typ < g.m.type_store.types.len {
		val_typ := g.m.type_store.types[val.typ]
		if val_typ.kind == .ptr_t && val_typ.elem_type == expected_struct_typ {
			g.load_val_to_reg(reg, val_id)
			return
		}
	}
	g.load_address_of_val_to_reg(reg, val_id)
}

fn (mut g Gen) copy_indirect_param_from_reg(param_id int, src_reg int) {
	param_typ := g.m.values[param_id].typ
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

fn (mut g Gen) load_float_bits_to_reg(val_id int, reg int) {
	val := g.m.values[val_id]
	if val.kind == .constant {
		if g.is_f32_type(val.typ) {
			f_val := f32(val.name.f64())
			bits := *unsafe { &u32(&f_val) }
			asm_mov_reg_imm32(mut g, Reg(reg), bits)
		} else {
			f_val := val.name.f64()
			bits := *unsafe { &u64(&f_val) }
			asm_mov_reg_imm64(mut g, Reg(reg), bits)
		}
		return
	}
	g.load_val_to_reg(reg, val_id)
}

// load_f32_operand_bits materializes an operand at f32 width. SSA may leave a
// numeric constant typed as f64 or an integer while the consuming operation is
// f32, so reading those raw f64 bits as `movd` would produce zero/garbage.
fn (mut g Gen) load_f32_operand_bits(val_id int, reg int) {
	val := g.m.values[val_id]
	if val.kind == .constant {
		mut value := f32(0)
		if g.is_float_type(val.typ) {
			value = f32(val.name.f64())
		} else {
			value = f32(g.integer_constant_value(val))
		}
		bits := *unsafe { &u32(&value) }
		asm_mov_reg_imm32(mut g, Reg(reg), bits)
		return
	}
	g.load_val_to_reg(reg, val_id)
}

fn (mut g Gen) load_val_to_reg(reg int, val_id int) {
	val := g.m.values[val_id]
	if val.kind == .constant {
		if g.is_float_type(val.typ) {
			g.load_float_bits_to_reg(val_id, reg)
			return
		}
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
			int_val := g.integer_constant_value(val)
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
		if val.typ > 0 && val.typ < g.m.type_store.types.len
			&& g.m.type_store.types[val.typ].kind == .ptr_t {
			str_offset := g.elf.rodata.len
			g.elf.rodata << val.name.bytes()
			g.elf.rodata << 0
			sym_name := 'L_strptr_${g.curr_offset}_${str_offset}'
			sym_idx := g.elf.add_symbol(sym_name, u64(str_offset), false, 3)
			asm_lea_reg_rip(mut g, Reg(reg))
			g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
			g.emit_u32(0)
			return
		}
		// V string literal: create { str, len, is_lit } struct on stack
		str_content := val.name
		str_len := str_content.len

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

// integer_constant_value interprets a constant using its SSA width and
// signedness. A positive spelling such as `2147483648` is a valid i32 bit
// pattern (-2147483648); loading it as an i64 keeps the two values distinct.
fn (g &Gen) integer_constant_value(val ssa.Value) i64 {
	typ := if val.typ > 0 && val.typ < g.m.type_store.types.len {
		g.m.type_store.types[val.typ]
	} else {
		ssa.Type{}
	}
	mut literal := val.name.replace('_', '')
	mut negative := false
	if literal.starts_with('-') || literal.starts_with('+') {
		negative = literal[0] == `-`
		literal = literal[1..]
	}
	mut base := 10
	if literal.starts_with('0x') || literal.starts_with('0X') {
		base = 16
		literal = literal[2..]
	} else if literal.starts_with('0o') || literal.starts_with('0O') {
		base = 8
		literal = literal[2..]
	} else if literal.starts_with('0b') || literal.starts_with('0B') {
		base = 2
		literal = literal[2..]
	}
	parsed := literal.parse_uint(base, 64) or { return 0 }
	mut bits := if negative { u64(0) - parsed } else { parsed }
	if typ.kind != .int_t || typ.width <= 0 || typ.width >= 64 {
		return i64(bits)
	}
	mask := (u64(1) << typ.width) - 1
	bits &= mask
	if !typ.is_unsigned && typ.width > 1 && bits & (u64(1) << (typ.width - 1)) != 0 {
		bits |= ~mask
	}
	return i64(bits)
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
	return g.m.type_size(typ_id)
}

fn (mut g Gen) struct_field_offset_bytes(struct_typ_id ssa.TypeID, field_idx int) int {
	return g.m.struct_field_offset(struct_typ_id, field_idx)
}

fn (mut g Gen) struct_field_offset_and_size(struct_typ_id ssa.TypeID, field_idx int, default_off int, default_size int) (int, int) {
	mut off := default_off
	mut size := default_size
	if struct_typ_id > 0 && struct_typ_id < g.m.type_store.types.len {
		typ := g.m.type_store.types[struct_typ_id]
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

fn (mut g Gen) gen_float_binop(op ssa.OpCode, lhs_id int, rhs_id int, val_id int) {
	if g.is_f32_type(g.m.values[val_id].typ) {
		g.load_f32_operand_bits(lhs_id, 0)
		asm_movd_xmm0_eax(mut g)
		g.load_f32_operand_bits(rhs_id, 1)
		asm_movd_xmm1_ecx(mut g)
		match op {
			.fadd {
				asm_addss_xmm0_xmm1(mut g)
			}
			.fsub {
				asm_subss_xmm0_xmm1(mut g)
			}
			.fmul {
				asm_mulss_xmm0_xmm1(mut g)
			}
			.fdiv {
				asm_divss_xmm0_xmm1(mut g)
			}
			.frem {
				asm_movaps_xmm2_xmm0(mut g)
				asm_divss_xmm0_xmm1(mut g)
				asm_cvttss2si_rax_xmm0(mut g)
				asm_cvtsi2ss_xmm0_rax(mut g)
				asm_mulss_xmm0_xmm1(mut g)
				asm_subss_xmm2_xmm0(mut g)
				asm_movaps_xmm0_xmm2(mut g)
			}
			else {}
		}
		asm_movd_eax_xmm0(mut g)
		g.store_reg_to_val(0, val_id)
		return
	}
	g.load_float_bits_to_reg(lhs_id, 0)
	asm_movq_xmm0_rax(mut g)
	g.load_float_bits_to_reg(rhs_id, 1)
	asm_movq_xmm1_rcx(mut g)
	match op {
		.fadd {
			asm_addsd_xmm0_xmm1(mut g)
		}
		.fsub {
			asm_subsd_xmm0_xmm1(mut g)
		}
		.fmul {
			asm_mulsd_xmm0_xmm1(mut g)
		}
		.fdiv {
			asm_divsd_xmm0_xmm1(mut g)
		}
		.frem {
			asm_movsd_xmm2_xmm0(mut g)
			asm_divsd_xmm2_xmm1(mut g)
			asm_roundsd_xmm2_trunc(mut g)
			asm_mulsd_xmm2_xmm1(mut g)
			asm_subsd_xmm0_xmm2(mut g)
		}
		else {}
	}
	asm_movq_rax_xmm0(mut g)
	g.store_reg_to_val(0, val_id)
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
		1 {
			asm_load_byte_base_disp(mut g, base, disp)
		}
		2 {
			asm_load_word_base_disp(mut g, base, disp)
		}
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
		else {
			asm_mov_rax_mem_base_disp(mut g, base, disp)
		}
	}
}

// emit_copy_memory copies exactly `size` bytes without reading or writing past
// either aggregate. Struct sizes are not always multiples of eight.
fn (mut g Gen) emit_copy_memory(dst Reg, src Reg, size int) {
	mut offset := 0
	for offset + 8 <= size {
		g.emit_sized_load_base_disp(src, offset, 8)
		asm_mov_mem_base_disp_rax(mut g, dst, offset)
		offset += 8
	}
	remaining := size - offset
	if remaining >= 4 {
		g.emit_sized_load_base_disp(src, offset, 4)
		asm_store_dword_base_disp(mut g, dst, offset)
		offset += 4
	}
	if size - offset >= 2 {
		g.emit_sized_load_base_disp(src, offset, 2)
		asm_store_word_base_disp(mut g, dst, offset)
		offset += 2
	}
	if offset < size {
		g.emit_sized_load_base_disp(src, offset, 1)
		asm_store_byte_base_disp(mut g, dst, offset)
	}
}

// emit_zero_stack_range clears exactly `size` bytes of a frame object.
fn (mut g Gen) emit_zero_stack_range(offset int, size int) {
	asm_xor_reg_reg(mut g, rax)
	mut cursor := 0
	for cursor + 8 <= size {
		asm_store_rbp_disp_reg(mut g, offset + cursor, rax)
		cursor += 8
	}
	remaining := size - cursor
	if remaining >= 4 {
		asm_store_rbp_disp32_reg32(mut g, offset + cursor, rax)
		cursor += 4
	}
	if size - cursor >= 2 {
		asm_store_rbp_disp16_reg16(mut g, offset + cursor, rax)
		cursor += 2
	}
	if cursor < size {
		asm_store_rbp_disp8_reg8(mut g, offset + cursor, rax)
	}
}

fn (mut g Gen) load_address_of_val_to_reg(reg int, val_id int) {
	val := g.m.values[val_id]
	// string_literal values need full materialization before their address
	// can be taken — the struct {str, len, is_lit} must be built on stack.
	if val.kind == .string_literal {
		g.load_val_to_reg(reg, val_id)
		return
	}
	if val.kind == .instruction {
		instr := g.m.instrs[val.index]
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

// write_file writes the generated ELF64 relocatable object to path.
pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}

// write_and_link writes the generated object and links it with the platform C
// runtime, matching the ARM64 generator's test-facing output API.
pub fn (mut g Gen) write_and_link(output string) {
	object := output + '.o'
	g.write_file(object)
	result := cmdexec.run('cc',
		['-no-pie', object, '-o', output, '-lm', '-ldl', '-lpthread'])
	os.rm(object) or {}
	if result.exit_code != 0 {
		panic('x64 link failed:\n${result.output}')
	}
}
