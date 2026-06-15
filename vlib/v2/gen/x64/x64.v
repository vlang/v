// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import v2.mir
import v2.ssa
import encoding.binary
import math.bits

const x64_windows_stack_probe_page_size = 4096

pub struct Gen {
	mod &mir.Module
mut:
	elf        &ElfObject
	macho      &MachOObject
	coff       &CoffObject
	obj_format ObjectFormat
	abi        X64Abi

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
	cur_func_abi_ret_class    mir.AbiValueClass
	sret_save_offset          int
}

struct Interval {
mut:
	val_id   int
	start    int
	end      int
	has_call bool
}

struct ReferencedWindowsCoffGlobals {
	complete bool
	indexes  map[int]bool
}

struct X64MainArgGlobals {
mut:
	argc []string
	argv []string
}

pub fn Gen.new(mod &mir.Module) &Gen {
	return &Gen{
		mod:        mod
		elf:        ElfObject.new()
		macho:      MachOObject.new()
		coff:       CoffObject.new()
		obj_format: .elf
		abi:        .sysv
	}
}

pub fn Gen.new_with_format(mod &mir.Module, obj_format ObjectFormat) &Gen {
	return Gen.new_with_format_and_abi(mod, obj_format, .sysv)
}

pub fn Gen.new_with_format_and_abi(mod &mir.Module, obj_format ObjectFormat, abi X64Abi) &Gen {
	return &Gen{
		mod:        mod
		elf:        ElfObject.new()
		macho:      MachOObject.new()
		coff:       CoffObject.new()
		obj_format: obj_format
		abi:        abi
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
	referenced_windows_globals := g.referenced_windows_coff_globals()
	for global_index, gvar in g.mod.globals {
		if gvar.linkage == .external {
			continue
		}
		if g.omit_unreferenced_windows_coff_global(global_index, referenced_windows_globals) {
			continue
		}
		for g.data_len() % 8 != 0 {
			g.add_data_byte(0)
		}
		addr := u64(g.data_len())
		g.add_symbol(gvar.name, addr, false, .data)
		if gvar.initial_data.len > 0 {
			g.add_data(gvar.initial_data)
		} else if gvar.is_constant || g.scalar_global_initial_value_supported(gvar.typ) {
			size := g.type_size(gvar.typ)
			mut bytes := []u8{len: if size > 0 { size } else { 8 }}
			if bytes.len >= 8 {
				binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
			} else {
				for i := 0; i < bytes.len; i++ {
					bytes[i] = u8(u64(gvar.initial_value) >> (i * 8))
				}
			}
			g.add_data(bytes)
		} else {
			size := g.type_size(gvar.typ)
			data_size := if size > 0 { size } else { 8 }
			for _ in 0 .. data_size {
				g.add_data_byte(0)
			}
		}
	}
}

fn (g Gen) referenced_windows_coff_globals() ReferencedWindowsCoffGlobals {
	mut referenced := map[int]bool{}
	if g.obj_format != .coff || g.abi != .windows {
		return ReferencedWindowsCoffGlobals{}
	}
	for func in g.mod.funcs {
		if func.is_c_extern || func.blocks.len == 0 {
			continue
		}
		for block_id in func.blocks {
			if block_id < 0 || block_id >= g.mod.blocks.len {
				return ReferencedWindowsCoffGlobals{}
			}
			block := g.mod.blocks[block_id]
			for val_id in block.instrs {
				if val_id < 0 || val_id >= g.mod.values.len {
					return ReferencedWindowsCoffGlobals{}
				}
				val := g.mod.values[val_id]
				if val.kind != .instruction || val.index < 0 || val.index >= g.mod.instrs.len {
					return ReferencedWindowsCoffGlobals{}
				}
				instr := g.mod.instrs[val.index]
				for operand_id in instr.operands {
					if operand_id < 0 || operand_id >= g.mod.values.len {
						return ReferencedWindowsCoffGlobals{}
					}
					operand := g.mod.values[operand_id]
					if operand.kind != .global {
						continue
					}
					if operand.index < 0 || operand.index >= g.mod.globals.len {
						return ReferencedWindowsCoffGlobals{}
					}
					if g.mod.globals[operand.index].linkage != .external {
						referenced[operand.index] = true
					}
				}
			}
		}
	}
	return ReferencedWindowsCoffGlobals{
		complete: true
		indexes:  referenced
	}
}

fn (g Gen) omit_unreferenced_windows_coff_global(index int, referenced ReferencedWindowsCoffGlobals) bool {
	if g.obj_format != .coff || g.abi != .windows {
		return false
	}
	if !referenced.complete {
		return false
	}
	return !referenced.indexes[index]
}

fn (g Gen) scalar_global_initial_value_supported(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return false
	}
	typ := g.mod.type_store.types[typ_id]
	return typ.kind in [.int_t, .ptr_t]
}

fn x64_object_symbol_bare_name(name string) string {
	if name.starts_with('_') {
		return name[1..]
	}
	return name
}

fn x64_main_argc_global_name(name string) bool {
	bare_name := x64_object_symbol_bare_name(name)
	return bare_name == 'g_main_argc' || bare_name == 'builtin__g_main_argc'
}

fn x64_main_argv_global_name(name string) bool {
	bare_name := x64_object_symbol_bare_name(name)
	return bare_name == 'g_main_argv' || bare_name == 'builtin__g_main_argv'
}

fn x64_add_unique_global_name(mut names []string, name string) {
	if name !in names {
		names << name
	}
}

fn x64_enqueue_reachable_func(mut queue []string, reachable map[string]bool, name string) {
	if name != '' && !reachable[name] && name !in queue {
		queue << name
	}
}

fn (g Gen) func_by_name(name string) ?mir.Function {
	for func in g.mod.funcs {
		if func.name == name {
			return func
		}
	}
	return none
}

fn (g Gen) reachable_funcs_from_main() map[string]bool {
	mut reachable := map[string]bool{}
	mut queue := ['main']
	for queue.len > 0 {
		name := queue[0]
		queue.delete(0)
		if reachable[name] {
			continue
		}
		func := g.func_by_name(name) or { continue }
		reachable[name] = true
		for blk_id in func.blocks {
			if blk_id < 0 || blk_id >= g.mod.blocks.len {
				continue
			}
			blk := g.mod.blocks[blk_id]
			for instr_id in blk.instrs {
				if instr_id < 0 || instr_id >= g.mod.values.len {
					continue
				}
				val := g.mod.values[instr_id]
				if val.kind != .instruction || val.index < 0 || val.index >= g.mod.instrs.len {
					continue
				}
				instr := g.mod.instrs[val.index]
				for operand_id in instr.operands {
					if operand_id < 0 || operand_id >= g.mod.values.len {
						continue
					}
					operand := g.mod.values[operand_id]
					if operand.kind == .func_ref {
						x64_enqueue_reachable_func(mut queue, reachable, operand.name)
					}
				}
				if instr.op !in [.call, .call_sret, .go_call, .spawn_call]
					|| instr.operands.len == 0 {
					continue
				}
				callee_id := instr.operands[0]
				if callee_id >= 0 && callee_id < g.mod.values.len {
					callee := g.mod.values[callee_id]
					if callee.kind in [.func_ref, .unknown] {
						x64_enqueue_reachable_func(mut queue, reachable, callee.name)
					}
				}
			}
		}
	}
	return reachable
}

fn (g Gen) referenced_main_arg_globals(reachable map[string]bool) X64MainArgGlobals {
	mut refs := X64MainArgGlobals{}
	for func in g.mod.funcs {
		if !reachable[func.name] {
			continue
		}
		for blk_id in func.blocks {
			if blk_id < 0 || blk_id >= g.mod.blocks.len {
				continue
			}
			blk := g.mod.blocks[blk_id]
			for instr_id in blk.instrs {
				if instr_id < 0 || instr_id >= g.mod.values.len {
					continue
				}
				val := g.mod.values[instr_id]
				if val.kind != .instruction || val.index < 0 || val.index >= g.mod.instrs.len {
					continue
				}
				instr := g.mod.instrs[val.index]
				for operand in instr.operands {
					if operand < 0 || operand >= g.mod.values.len {
						continue
					}
					operand_val := g.mod.values[operand]
					if operand_val.kind != .global {
						continue
					}
					if x64_main_argc_global_name(operand_val.name) {
						x64_add_unique_global_name(mut refs.argc, operand_val.name)
					} else if x64_main_argv_global_name(operand_val.name) {
						x64_add_unique_global_name(mut refs.argv, operand_val.name)
					}
				}
			}
		}
	}
	return refs
}

fn (g Gen) has_defined_global(name string) bool {
	for gvar in g.mod.globals {
		if gvar.name == name && gvar.linkage != .external {
			return true
		}
	}
	return false
}

fn (mut g Gen) store_reg_to_global_symbol(reg Reg, name string, size int) {
	sym_idx := g.add_undefined(name)
	asm_lea_reg_rip(mut g, r10)
	g.add_rip_reloc(sym_idx)
	g.emit_u32(0)
	asm_store_mem_base_disp_reg_size(mut g, r10, 0, reg, size)
}

fn (mut g Gen) maybe_store_sysv_hosted_main_args(func mir.Function) {
	if func.name != 'main' || g.abi != .sysv || g.obj_format !in [.elf, .macho] {
		return
	}
	reachable := g.reachable_funcs_from_main()
	refs := g.referenced_main_arg_globals(reachable)
	for name in refs.argc {
		if g.has_defined_global(name) {
			g.store_reg_to_global_symbol(rdi, name, 4)
		}
	}
	for name in refs.argv {
		if g.has_defined_global(name) {
			g.store_reg_to_global_symbol(rsi, name, 8)
		}
	}
}

fn (mut g Gen) gen_func(func mir.Function) {
	g.curr_offset = g.text_len()
	g.stack_map = map[int]int{}
	g.alloca_offsets = map[int]int{}
	g.block_offsets = map[int]int{}
	g.pending_labels = map[int][]int{}
	g.reg_map = map[int]int{}
	g.used_regs = []int{}
	g.cur_func_ret_type = func.typ
	g.cur_func_abi_ret_indirect = func.abi_ret_indirect
	g.cur_func_abi_ret_class = func.abi_ret_class
	g.sret_save_offset = 0

	g.allocate_registers(func)

	// Start after callee-saved pushes so locals do not overlap their rbp slots.
	mut slot_offset := g.used_regs.len * 8

	// Hidden sret pointer slot (SysV: incoming in RDI, Windows: incoming in RCX)
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

			mut val_has_stack_storage := false
			if g.value_needs_stack_storage(val_id) {
				result_size := g.stack_storage_size(val_id)
				off, next_offset := reserve_stack_bytes(slot_offset, result_size, 16)
				g.stack_map[val_id] = off
				slot_offset = next_offset
				val_has_stack_storage = true
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

			if val_has_stack_storage {
				continue
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

	g.add_symbol(func.name, u64(g.curr_offset), true, .text)

	// Prologue
	asm_endbr64(mut g)
	asm_push_rbp(mut g)
	asm_mov_rbp_rsp(mut g)
	g.maybe_store_sysv_hosted_main_args(func)

	// Push callee-saved regs
	for r in g.used_regs {
		asm_push(mut g, Reg(r))
	}

	g.emit_stack_allocation()

	abi_regs := g.abi.int_arg_regs()
	arg_reg_base := if func.abi_ret_indirect { 1 } else { 0 }
	mut reg_arg_idx := arg_reg_base
	mut sse_arg_idx := 0
	float_arg_regs := g.abi.float_arg_regs()
	if func.abi_ret_indirect && g.sret_save_offset != 0 {
		asm_store_rbp_disp_reg(mut g, g.sret_save_offset, g.abi.sret_reg())
	}
	mut stack_param_offset := 16
	for i, pid in func.params {
		is_indirect_param := i < func.abi_param_class.len && func.abi_param_class[i] == .indirect
		param_size := g.type_size(g.mod.values[pid].typ)
		if g.abi == .windows {
			g.move_windows_param(pid, i + arg_reg_base, is_indirect_param, param_size)
			continue
		}
		if g.value_is_float_type(pid) {
			g.ensure_float_abi_scalar(pid, 'parameter')
			if sse_arg_idx >= float_arg_regs.len {
				g.unsupported_float_abi('stack parameter', pid)
			}
			asm_store_xmm_rbp_disp(mut g, float_arg_regs[sse_arg_idx], g.stack_map[pid], param_size)
			sse_arg_idx++
			continue
		}
		if !is_indirect_param && g.value_is_aggregate(pid) && i < func.abi_param_layouts.len
			&& func.abi_param_layouts[i].locs.len > 0 {
			layout := func.abi_param_layouts[i]
			g.store_sysv_direct_aggregate_param(pid, layout)
			int_limit, sse_limit := sysv_layout_register_limits(layout)
			if int_limit > reg_arg_idx {
				reg_arg_idx = int_limit
			}
			if sse_limit > sse_arg_idx {
				sse_arg_idx = sse_limit
			}
			stack_limit := sysv_layout_stack_slot_limit(layout)
			if stack_limit > 0 {
				stack_param_offset = 16 + stack_limit * 8
			}
			continue
		}
		param_chunks := if !is_indirect_param && g.value_is_aggregate(pid) && param_size > 8
			&& param_size <= 16 {
			(param_size + 7) / 8
		} else {
			1
		}
		if reg_arg_idx + param_chunks <= abi_regs.len {
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
		g.block_offsets[blk_id] = g.text_len() - g.curr_offset

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

fn (mut g Gen) emit_stack_allocation() {
	if g.stack_size <= 0 {
		return
	}
	if g.abi == .windows && g.stack_size >= x64_windows_stack_probe_page_size {
		g.emit_windows_stack_probe_allocation(g.stack_size)
		return
	}
	g.emit_stack_sub(g.stack_size)
}

fn (mut g Gen) emit_stack_sub(size int) {
	if size <= 0 {
		return
	}
	if size <= 127 {
		asm_sub_rsp_imm8(mut g, u8(size))
	} else {
		asm_sub_rsp_imm32(mut g, u32(size))
	}
}

fn (mut g Gen) emit_windows_stack_probe_allocation(size int) {
	mut remaining := size
	for remaining > x64_windows_stack_probe_page_size {
		g.emit_stack_sub(x64_windows_stack_probe_page_size)
		asm_test_byte_ptr_rsp_zero(mut g)
		remaining -= x64_windows_stack_probe_page_size
	}
	g.emit_stack_sub(remaining)
	asm_test_byte_ptr_rsp_zero(mut g)
}

fn (mut g Gen) move_windows_param(pid int, position int, is_indirect_param bool, param_size int) {
	if g.value_is_float_type(pid) {
		g.ensure_float_abi_scalar(pid, 'parameter')
		if position < 4 {
			asm_store_xmm_rbp_disp(mut g, g.abi.float_arg_reg_for_position(position),
				g.stack_map[pid], param_size)
		} else {
			asm_load_xmm_mem_base_disp_size(mut g, 0, rbp, g.abi.stack_arg_offset(position),
				param_size)
			asm_store_xmm_rbp_disp(mut g, 0, g.stack_map[pid], param_size)
		}
		return
	}
	param_is_indirect := g.windows_value_passed_indirect(pid, is_indirect_param, param_size)
	g.ensure_windows_scalar_or_indirect_arg(pid, param_is_indirect, param_size)
	if position < 4 {
		src := g.abi.int_arg_reg_for_position(position)
		if param_is_indirect {
			g.copy_indirect_param_from_reg(pid, src)
		} else if g.value_needs_raw_abi_reg_bytes(pid, param_size) {
			g.store_reg_to_rbp_exact(Reg(src), g.stack_map[pid], param_size)
		} else if reg := g.reg_map[pid] {
			asm_mov_reg_reg(mut g, Reg(reg), Reg(src))
		} else {
			asm_store_rbp_disp_reg(mut g, g.stack_map[pid], Reg(src))
		}
		return
	}
	stack_param_offset := g.abi.stack_arg_offset(position)
	if param_is_indirect {
		asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
		g.copy_indirect_param_from_reg(pid, int(rax))
	} else if g.value_needs_raw_abi_reg_bytes(pid, param_size) {
		asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
		g.store_reg_to_rbp_exact(rax, g.stack_map[pid], param_size)
	} else if reg := g.reg_map[pid] {
		asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
		asm_mov_reg_reg(mut g, Reg(reg), rax)
	} else {
		asm_load_reg_rbp_disp(mut g, rax, stack_param_offset)
		asm_store_rbp_disp_reg(mut g, g.stack_map[pid], rax)
	}
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
			dst_id := instr.operands[1]
			src_typ := g.mod.values[src_id].typ
			src_type_info := g.mod.type_store.types[src_typ]
			src_size := g.type_size(src_typ)
			if src_type_info.kind in [.struct_t, .array_t] || src_size > 8 {
				g.load_struct_src_address_to_reg(int(r10), src_id, src_typ)
				g.load_val_to_reg(int(r11), dst_id)
				g.copy_memory(int(r11), 0, int(r10), 0, src_size)
			} else {
				store_size := g.scalar_store_size_for_pointer_destination(dst_id, src_size)
				if g.value_is_float_type(src_id) && src_size in [4, 8] && store_size == src_size {
					g.load_val_to_reg(1, dst_id) // Ptr -> RCX
					g.load_float_val_to_xmm(0, src_id, src_size)
					asm_store_xmm_mem_base_disp_size(mut g, 0, rcx, 0, src_size)
					return
				}
				g.load_val_to_reg(0, src_id) // Val -> RAX
				g.load_val_to_reg(1, dst_id) // Ptr -> RCX
				asm_store_mem_base_disp_reg_size(mut g, rcx, 0, rax, store_size)
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
			g.zero_large_fixed_array_alloca(val_id, off)
			asm_lea_reg_rbp_disp(mut g, rax, off)
			g.store_reg_to_val(0, val_id)
		}
		.heap_alloc {
			alloc_size := g.heap_alloc_size(val_id)
			cleanup := g.emit_windows_call_frame(0)
			if g.abi == .windows {
				asm_mov_reg_imm32(mut g, rcx, 1)
				asm_mov_reg_imm64(mut g, rdx, u64(alloc_size))
			} else {
				asm_mov_reg_imm32(mut g, rdi, 1)
				asm_mov_reg_imm64(mut g, rsi, u64(alloc_size))
				asm_xor_eax_eax(mut g)
			}
			asm_call_rel32(mut g)
			sym_idx := g.add_undefined('calloc')
			g.add_call_reloc(sym_idx)
			g.emit_u32(0)
			g.cleanup_windows_call_frame(cleanup)
			g.store_reg_to_val(0, val_id)
		}
		.get_element_ptr {
			g.load_val_to_reg(0, instr.operands[0]) // Base -> RAX
			offset := g.gep_const_offset(instr.operands[0], instr.operands[1], instr.typ)
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
			abi_regs := g.abi.int_arg_regs()
			num_args := instr.operands.len - 1
			stack_args := g.call_stack_arg_mask(instr, abi_regs.len, 0)
			stack_slots := g.call_stack_slots(instr, stack_args)
			cleanup := g.prepare_call_stack_args(instr, stack_args, stack_slots, 0)
			if g.abi == .sysv && stack_slots > 0 {
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
			sse_arg_idx := g.load_call_register_args(instr, abi_regs, stack_args, 0)
			fn_val := g.mod.values[instr.operands[0]]
			is_direct_symbol_call := fn_val.name != '' && fn_val.kind in [.unknown, .func_ref]
			if !is_direct_symbol_call {
				g.load_val_to_reg(int(r10), instr.operands[0])
			}

			// AL carries the number of SSE argument registers for variadic calls.
			g.emit_sse_arg_count(sse_arg_idx)

			if is_direct_symbol_call {
				asm_call_rel32(mut g)
				sym_idx := g.add_undefined(fn_val.name)
				// Use R_X86_64_PLT32 (4) for function calls to support shared libraries (libc)
				g.add_call_reloc(sym_idx)
				g.emit_u32(0)
			} else {
				asm_call_r10(mut g)
			}

			// Clean up stack arguments
			if g.abi == .windows {
				g.cleanup_windows_call_frame(cleanup)
			} else if stack_slots > 0 {
				sysv_cleanup := (stack_slots + (stack_slots % 2)) * 8
				if sysv_cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(sysv_cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(sysv_cleanup))
				}
			}

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_call_result(val_id, instr.abi_ret_class)
			}
		}
		.call_sret {
			abi_regs := if g.abi == .sysv { g.abi.int_arg_regs() } else { g.abi.sret_arg_regs() }
			num_args := instr.operands.len - 1
			arg_position_base := 1
			stack_args := g.call_stack_arg_mask(instr, abi_regs.len, arg_position_base)
			stack_slots := g.call_stack_slots(instr, stack_args)

			if g.abi != .windows {
				g.load_address_of_val_to_reg(int(g.abi.sret_reg()), val_id)
			}

			cleanup := g.prepare_call_stack_args(instr, stack_args, stack_slots, arg_position_base)
			if g.abi == .sysv && stack_slots > 0 {
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
			sse_arg_idx := g.load_call_register_args(instr, abi_regs, stack_args, arg_position_base)
			if g.abi == .windows {
				g.load_address_of_val_to_reg(int(g.abi.sret_reg()), val_id)
			}

			fn_val := g.mod.values[instr.operands[0]]
			is_direct_symbol_call := fn_val.name != '' && fn_val.kind in [.unknown, .func_ref]
			if !is_direct_symbol_call {
				g.load_val_to_reg(int(r10), instr.operands[0])
			}

			// AL carries the number of SSE argument registers for variadic calls.
			g.emit_sse_arg_count(sse_arg_idx)

			if is_direct_symbol_call {
				asm_call_rel32(mut g)
				sym_idx := g.add_undefined(fn_val.name)
				g.add_call_reloc(sym_idx)
				g.emit_u32(0)
			} else {
				asm_call_r10(mut g)
			}

			// Clean up stack arguments
			if g.abi == .windows {
				g.cleanup_windows_call_frame(cleanup)
			} else if stack_slots > 0 {
				sysv_cleanup := (stack_slots + (stack_slots % 2)) * 8
				if sysv_cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(sysv_cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(sysv_cleanup))
				}
			}
		}
		.call_indirect {
			// Indirect call through function pointer
			// operands[0] is the function pointer, rest are arguments
			abi_regs := g.abi.int_arg_regs()
			num_args := instr.operands.len - 1
			stack_args := g.call_stack_arg_mask(instr, abi_regs.len, 0)
			stack_slots := g.call_stack_slots(instr, stack_args)
			cleanup := g.prepare_call_stack_args(instr, stack_args, stack_slots, 0)
			if g.abi == .sysv && stack_slots > 0 {
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
			sse_arg_idx := g.load_call_register_args(instr, abi_regs, stack_args, 0)

			// Load function pointer to r10 (caller-saved, not used for args)
			g.load_val_to_reg(10, instr.operands[0])

			// AL carries the number of SSE argument registers for variadic calls.
			g.emit_sse_arg_count(sse_arg_idx)

			// call *r10
			asm_call_r10(mut g)

			// Clean up stack arguments
			if g.abi == .windows {
				g.cleanup_windows_call_frame(cleanup)
			} else if stack_slots > 0 {
				sysv_cleanup := (stack_slots + (stack_slots % 2)) * 8
				if sysv_cleanup <= 127 {
					asm_add_rsp_imm8(mut g, u8(sysv_cleanup))
				} else {
					asm_add_rsp_imm32(mut g, u32(sysv_cleanup))
				}
			}

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_call_result(val_id, instr.abi_ret_class)
			}
		}
		.ret {
			if g.cur_func_abi_ret_indirect {
				if g.sret_save_offset != 0 {
					asm_load_reg_rbp_disp(mut g, g.abi.sret_reg(), g.sret_save_offset)
				}
				if instr.operands.len > 0 {
					ret_val_id := instr.operands[0]
					ret_size := g.type_size(g.cur_func_ret_type)
					if ret_size > 0 {
						g.load_struct_src_address_to_reg(int(r10), ret_val_id, g.cur_func_ret_type)
						g.copy_memory(int(g.abi.sret_reg()), 0, int(r10), 0, ret_size)
					}
				}
				asm_mov_reg_reg(mut g, rax, g.abi.sret_reg())
			} else if instr.operands.len > 0 {
				ret_val_id := instr.operands[0]
				g.ensure_windows_direct_return_supported(ret_val_id, g.cur_func_abi_ret_class,
					'direct return')
				if g.value_is_float_type(ret_val_id) {
					g.ensure_float_abi_scalar(ret_val_id, 'return')
					g.load_float_val_to_xmm(0, ret_val_id,
						g.type_size(g.mod.values[ret_val_id].typ))
				} else if g.load_sysv_direct_aggregate_return(ret_val_id, g.cur_func_abi_ret_class) {
				} else if g.load_sysv_integer_pair_return(ret_val_id, g.cur_func_abi_ret_class) {
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
			g.emit_rel32_to_block(false_blk)
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
				g.emit_rel32_to_block(target_idx)
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
						g.normalize_integer_rax_for_type(dst_typ, op, val_id)
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
				field_typ := g.struct_field_type(instr.typ, fi, g.mod.values[field_id].typ)
				g.store_field_value(val_id, instr.typ, fi, field_id, g.type_size(field_typ))
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
			x64_unsupported('op ${op} in value ${val_id}')
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
	normal_patch := g.text_len()
	g.emit_u32(0)
	asm_mov_reg_reg(mut g, rcx, rax)
	asm_and_rcx_imm8(mut g, 1)
	asm_shr_rax_1(mut g)
	asm_or_rax_rcx(mut g)
	g.emit_signed_int_to_float(result_size, .uitofp, val_id)
	asm_add_float_xmm0_xmm0(mut g, result_size)
	asm_jmp_rel32(mut g)
	end_patch := g.text_len()
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
	big_patch := g.text_len()
	g.emit_u32(0)
	g.emit_float_to_signed_int(src_size, .fptoui, val_id)
	asm_jmp_rel32(mut g)
	end_patch := g.text_len()
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
	str_offset := g.rodata_len()
	g.add_rodata(bytes)
	sym_name := 'L_fp_${g.curr_offset}_${str_offset}'
	sym_idx := g.add_symbol(sym_name, u64(str_offset), false, .rodata)
	asm_lea_reg_rip(mut g, r10)
	g.add_rip_reloc(sym_idx)
	g.emit_u32(0)
	asm_load_xmm_mem_base_disp_size(mut g, 1, r10, 0, size)
}

fn (mut g Gen) patch_rel32(patch_pos int) {
	target := g.text_len()
	rel := target - (patch_pos + 4)
	g.write_u32(patch_pos, u32(rel))
}

fn (g Gen) unsupported_numeric_conversion(op ssa.OpCode, src_size int, dst_size int, val_id int) {
	x64_unsupported('numeric conversion ${op} from ${src_size * 8}-bit to ${dst_size * 8}-bit in value ${val_id}')
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

fn valid_x64_scalar_memory_size(size int) bool {
	return size in [1, 2, 4, 8]
}

fn x64_scalar_memory_size_or_default(size int) int {
	return if valid_x64_scalar_memory_size(size) { size } else { 8 }
}

fn (g Gen) scalar_store_size_for_pointer_destination(ptr_id int, fallback_size int) int {
	if ptr_id <= 0 || ptr_id >= g.mod.values.len {
		return x64_scalar_memory_size_or_default(fallback_size)
	}
	ptr_typ_id := g.mod.values[ptr_id].typ
	if ptr_typ_id <= 0 || ptr_typ_id >= g.mod.type_store.types.len {
		return x64_scalar_memory_size_or_default(fallback_size)
	}
	ptr_typ := g.mod.type_store.types[ptr_typ_id]
	if ptr_typ.kind != .ptr_t || ptr_typ.elem_type <= 0 {
		return x64_scalar_memory_size_or_default(fallback_size)
	}
	elem_size := g.type_size(ptr_typ.elem_type)
	if valid_x64_scalar_memory_size(elem_size) {
		return elem_size
	}
	return x64_scalar_memory_size_or_default(fallback_size)
}

fn (g Gen) store_access_size(src_id int, dst_id int) int {
	src_typ := g.mod.values[src_id].typ
	src_type_info := g.mod.type_store.types[src_typ]
	src_size := g.type_size(src_typ)
	if src_type_info.kind in [.struct_t, .array_t] || src_size > 8 {
		return src_size
	}
	return g.scalar_store_size_for_pointer_destination(dst_id, src_size)
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
			store_size := g.store_access_size(use_instr.operands[0], use_instr.operands[1])
			if store_size > size {
				size = store_size
			}
		}
		if use_instr.op != .get_element_ptr || use_instr.operands.len < 2
			|| use_instr.operands[0] != ptr_id {
			continue
		}
		offset := g.gep_const_offset(ptr_id, use_instr.operands[1], use_instr.typ)
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
			store_size := g.store_access_size(use_instr.operands[0], use_instr.operands[1])
			if store_size > size {
				size = store_size
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
	if typ.is_union {
		return 0
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

fn (g Gen) gep_const_offset(base_id int, idx_id int, result_typ_id ssa.TypeID) int {
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
		if result_typ_id > 0 && result_typ_id < g.mod.type_store.types.len {
			result_typ := g.mod.type_store.types[result_typ_id]
			if result_typ.kind == .ptr_t && result_typ.elem_type == base_typ.elem_type {
				return idx * g.type_size(base_typ.elem_type)
			}
		}
		return g.struct_field_offset_bytes(base_typ.elem_type, idx)
	}
	if elem_typ.kind == .array_t {
		return idx * g.type_size(elem_typ.elem_type)
	}
	return idx * g.type_size(base_typ.elem_type)
}

fn (g Gen) gep_elem_size(base_id int) int {
	if base_id > 0 && base_id < g.mod.values.len {
		base_typ_id := g.mod.values[base_id].typ
		if base_typ_id > 0 && base_typ_id < g.mod.type_store.types.len {
			base_typ := g.mod.type_store.types[base_typ_id]
			if base_typ.kind == .ptr_t {
				elem_typ := g.mod.type_store.types[base_typ.elem_type]
				if elem_typ.kind == .array_t {
					size := g.type_size(elem_typ.elem_type)
					return if size > 0 { size } else { 8 }
				}
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

fn (mut g Gen) zero_large_fixed_array_alloca(val_id int, off int) {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return
	}
	alloca_val := g.mod.values[val_id]
	if alloca_val.typ <= 0 || alloca_val.typ >= g.mod.type_store.types.len {
		return
	}
	alloca_ptr_type := g.mod.type_store.types[alloca_val.typ]
	if alloca_ptr_type.kind != .ptr_t || alloca_ptr_type.elem_type <= 0
		|| alloca_ptr_type.elem_type >= g.mod.type_store.types.len {
		return
	}
	elem_typ := g.mod.type_store.types[alloca_ptr_type.elem_type]
	if elem_typ.kind != .array_t || elem_typ.len <= 16 {
		return
	}
	arr_size := g.type_size(alloca_ptr_type.elem_type)
	if arr_size <= 0 {
		return
	}
	asm_xor_reg_reg(mut g, rax)
	g.store_repeated_zero(int(rbp), off, arr_size)
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
		x64_unsupported('raw memory size ${size}')
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
		x64_unsupported('raw register spill size ${size}')
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
	if g.value_is_float_type(src_id) && size in [4, 8] {
		g.load_float_val_to_xmm(0, src_id, size)
		asm_store_xmm_rbp_disp(mut g, 0, dst_off, size)
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
	_ = g
	return instr.op
}

fn (mut g Gen) emit_jmp(target_idx int) {
	asm_jmp_rel32(mut g)
	g.emit_rel32_to_block(target_idx)
}

fn (mut g Gen) emit_rel32_to_block(target_idx int) {
	if target_idx in g.block_offsets {
		off := g.block_offsets[target_idx]
		rel := off - (g.text_len() - g.curr_offset + 4)
		g.emit_u32(u32(rel))
		return
	}
	g.record_pending_label(target_idx)
	g.emit_u32(0)
}

fn (mut g Gen) load_call_arg_to_reg(reg int, val_id int, arg_idx int, instr mir.Instruction) {
	is_indirect := g.call_arg_is_indirect(val_id, arg_idx, instr)
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

fn (mut g Gen) load_sysv_direct_aggregate_arg_to_regs(val_id int, layout mir.AbiValueLayout, abi_regs []int) {
	g.ensure_sysv_direct_aggregate_supported(val_id, layout.value_class, 'argument')
	size := g.type_size(g.mod.values[val_id].typ)
	g.load_struct_src_address_to_reg(int(r10), val_id, g.mod.values[val_id].typ)
	sse_regs := g.abi.float_arg_regs()
	mut loc_idx := 0
	for loc_idx < layout.locs.len {
		loc := layout.locs[loc_idx]
		if loc.kind in [.none, .stack] {
			loc_idx++
			continue
		}
		chunk_size := sysv_abi_chunk_size(size, loc.offset)
		if chunk_size <= 0 {
			loc_idx++
			continue
		}
		match loc.kind {
			.int_reg {
				reg := sysv_checked_int_reg(abi_regs, loc.index, 'argument')
				g.load_raw_mem_to_reg(reg, r10, loc.offset, chunk_size)
				loc_idx++
			}
			.sse_reg {
				if sysv_layout_has_sseup_pair(layout, loc_idx, size) {
					xmm := sysv_checked_sse_reg(sse_regs, loc.index, 'argument')
					asm_load_xmm_mem_base_disp_128(mut g, xmm, r10, loc.offset)
					loc_idx += 2
					continue
				}
				if loc.class == .sseup {
					x64_unsupported('backend feature: SysV direct aggregate argument with unpaired SSEUP ABI location is not implemented yet')
				}
				sysv_checked_sse_chunk_size(chunk_size, 'argument')
				xmm := sysv_checked_sse_reg(sse_regs, loc.index, 'argument')
				asm_load_xmm_mem_base_disp_size(mut g, xmm, r10, loc.offset, chunk_size)
				loc_idx++
			}
			else {
				x64_unsupported('backend feature: SysV direct aggregate argument with unsupported ABI location is not implemented yet')
			}
		}
	}
}

fn (mut g Gen) prepare_call_stack_args(instr mir.Instruction, stack_args []bool, stack_slots int, arg_position_base int) int {
	if g.abi != .windows {
		return 0
	}
	cleanup := g.emit_windows_call_frame(stack_slots)
	for arg_idx, is_stack in stack_args {
		if is_stack {
			g.store_windows_call_stack_arg(instr.operands[arg_idx + 1], arg_idx, arg_idx +
				arg_position_base, instr)
		}
	}
	return cleanup
}

fn (mut g Gen) emit_windows_call_frame(stack_slots int) int {
	if g.abi != .windows {
		return 0
	}
	cleanup := g.abi.call_frame_size(stack_slots)
	if cleanup <= 127 {
		asm_sub_rsp_imm8(mut g, u8(cleanup))
	} else {
		asm_sub_rsp_imm32(mut g, u32(cleanup))
	}
	return cleanup
}

fn (mut g Gen) cleanup_windows_call_frame(cleanup int) {
	if cleanup == 0 {
		return
	}
	if cleanup <= 127 {
		asm_add_rsp_imm8(mut g, u8(cleanup))
	} else {
		asm_add_rsp_imm32(mut g, u32(cleanup))
	}
}

fn (mut g Gen) store_windows_call_stack_arg(val_id int, arg_idx int, position int, instr mir.Instruction) {
	is_indirect := g.call_arg_is_indirect(val_id, arg_idx, instr)
	size := g.type_size(g.mod.values[val_id].typ)
	g.ensure_windows_scalar_or_indirect_arg(val_id, is_indirect, size)
	disp := g.abi.call_stack_arg_offset(position)
	if g.value_is_float_type(val_id) {
		g.ensure_float_abi_scalar(val_id, 'stack argument')
		g.load_float_val_to_xmm(0, val_id, size)
		asm_store_xmm_mem_base_disp_size(mut g, 0, rsp, disp, size)
		return
	}
	g.load_call_arg_to_reg(0, val_id, arg_idx, instr)
	asm_store_mem_base_disp_reg_size(mut g, rsp, disp, rax, 8)
}

fn (mut g Gen) ensure_windows_scalar_or_indirect_arg(val_id int, is_indirect bool, size int) {
	if is_indirect {
		return
	}
	if g.value_is_aggregate(val_id) && size !in [1, 2, 4, 8] {
		g.unsupported_windows_abi_arg(
			'direct aggregate argument larger than 8 bytes reached codegen; ' +
			'expected ABI lowering before codegen to pass it indirectly', val_id)
	}
	if !g.value_is_aggregate(val_id) && size !in [1, 2, 4, 8] {
		g.unsupported_windows_abi_arg(
			'scalar argument with unsupported storage width ${size} bytes reached codegen; ' +
			'expected ABI lowering before codegen', val_id)
	}
}

fn (g Gen) unsupported_windows_abi_arg(reason string, val_id int) {
	x64_unsupported('backend feature: Windows argument lowering for value ${val_id}: ${reason}; check ABI lowering')
}

fn (g Gen) ensure_sysv_direct_aggregate_supported(val_id int, value_class mir.AbiValueClass, context string) {
	if g.abi != .sysv || value_class.mode != .direct || !g.value_is_aggregate(val_id)
		|| value_class.classes.len == 0 {
		return
	}
	for i, class in value_class.classes {
		match class {
			.no_class, .integer, .sse {}
			.sseup {
				if i == 0 || value_class.classes[i - 1] !in [.sse, .sseup] {
					x64_unsupported('backend feature: SysV direct aggregate ${context} with unpaired SSEUP eightbyte class is not implemented yet')
				}
			}
			else {
				x64_unsupported('backend feature: SysV direct aggregate ${context} with MEMORY eightbyte classes is not implemented yet')
			}
		}
	}
}

fn sysv_abi_chunk_size(total_size int, offset int) int {
	if total_size <= offset {
		return 0
	}
	remaining := total_size - offset
	if remaining < 8 {
		return remaining
	}
	return 8
}

fn sysv_layout_register_limits(layout mir.AbiValueLayout) (int, int) {
	mut int_limit := 0
	mut sse_limit := 0
	for loc in layout.locs {
		if loc.kind == .int_reg && loc.index + 1 > int_limit {
			int_limit = loc.index + 1
		}
		if loc.kind == .sse_reg && loc.class == .sse && loc.index + 1 > sse_limit {
			sse_limit = loc.index + 1
		}
	}
	return int_limit, sse_limit
}

fn sysv_layout_stack_slot_limit(layout mir.AbiValueLayout) int {
	mut limit := 0
	for loc in layout.locs {
		if loc.kind == .stack && loc.index + 1 > limit {
			limit = loc.index + 1
		}
	}
	return limit
}

fn sysv_layout_uses_stack(layout mir.AbiValueLayout) bool {
	for loc in layout.locs {
		if loc.kind == .stack {
			return true
		}
	}
	return false
}

fn sysv_checked_int_reg(regs []int, index int, context string) Reg {
	if index < 0 || index >= regs.len {
		x64_unsupported('backend feature: SysV direct aggregate ${context} needs INTEGER register ${index} outside available ABI registers')
	}
	return Reg(regs[index])
}

fn sysv_checked_sse_reg(regs []int, index int, context string) int {
	if index < 0 || index >= regs.len {
		x64_unsupported('backend feature: SysV direct aggregate ${context} needs SSE register ${index} outside available ABI registers')
	}
	return regs[index]
}

fn sysv_checked_sse_chunk_size(size int, context string) {
	if size !in [4, 8] {
		x64_unsupported('backend feature: SysV direct aggregate ${context} with ${size}-byte SSE eightbyte chunk is not implemented yet')
	}
}

fn sysv_layout_has_sseup_pair(layout mir.AbiValueLayout, loc_idx int, total_size int) bool {
	if loc_idx + 1 >= layout.locs.len {
		return false
	}
	loc := layout.locs[loc_idx]
	next := layout.locs[loc_idx + 1]
	return loc.kind == .sse_reg && loc.class == .sse && next.kind == .sse_reg
		&& next.class == .sseup && next.index == loc.index && next.offset == loc.offset + 8
		&& total_size >= loc.offset + 16
}

fn sysv_class_has_sseup_pair(classes []mir.AbiEightbyteClass, class_idx int, total_size int) bool {
	return class_idx + 1 < classes.len && classes[class_idx] == .sse
		&& classes[class_idx + 1] == .sseup && total_size >= class_idx * 8 + 16
}

fn sysv_int_return_reg(index int, context string) Reg {
	return match index {
		0 {
			rax
		}
		1 {
			rdx
		}
		else {
			x64_unsupported('backend feature: SysV direct aggregate ${context} needs INTEGER return register ${index} outside available ABI registers')
			rax
		}
	}
}

fn sysv_sse_return_reg(index int, context string) int {
	if index < 0 || index >= 2 {
		x64_unsupported('backend feature: SysV direct aggregate ${context} needs SSE return register ${index} outside available ABI registers')
	}
	return index
}

fn (mut g Gen) store_sysv_direct_aggregate_param(pid int, layout mir.AbiValueLayout) {
	if g.abi != .sysv {
		return
	}
	g.ensure_sysv_direct_aggregate_supported(pid, layout.value_class, 'parameter')
	param_size := g.type_size(g.mod.values[pid].typ)
	dst_off := g.stack_map[pid]
	int_regs := g.abi.int_arg_regs()
	sse_regs := g.abi.float_arg_regs()
	mut loc_idx := 0
	for loc_idx < layout.locs.len {
		loc := layout.locs[loc_idx]
		if loc.kind == .none {
			loc_idx++
			continue
		}
		chunk_size := sysv_abi_chunk_size(param_size, loc.offset)
		if chunk_size <= 0 {
			loc_idx++
			continue
		}
		match loc.kind {
			.int_reg {
				reg := sysv_checked_int_reg(int_regs, loc.index, 'parameter')
				g.store_reg_to_rbp_exact(reg, dst_off + loc.offset, chunk_size)
				loc_idx++
			}
			.sse_reg {
				if sysv_layout_has_sseup_pair(layout, loc_idx, param_size) {
					xmm := sysv_checked_sse_reg(sse_regs, loc.index, 'parameter')
					asm_store_xmm_mem_base_disp_128(mut g, xmm, rbp, dst_off + loc.offset)
					loc_idx += 2
					continue
				}
				if loc.class == .sseup {
					x64_unsupported('backend feature: SysV direct aggregate parameter with unpaired SSEUP ABI location is not implemented yet')
				}
				sysv_checked_sse_chunk_size(chunk_size, 'parameter')
				xmm := sysv_checked_sse_reg(sse_regs, loc.index, 'parameter')
				asm_store_xmm_rbp_disp(mut g, xmm, dst_off + loc.offset, chunk_size)
				loc_idx++
			}
			.stack {
				g.copy_memory(int(rbp), dst_off + loc.offset, int(rbp), 16 + loc.index * 8,
					chunk_size)
				loc_idx++
			}
			else {
				x64_unsupported('backend feature: SysV direct aggregate parameter with unsupported ABI location is not implemented yet')
			}
		}
	}
}

fn (g Gen) ensure_windows_direct_return_supported(val_id int, ret_class mir.AbiValueClass, context string) {
	if g.abi != .windows {
		return
	}
	if ret_class.mode == .indirect {
		return
	}
	if ret_class.size == 0 && ret_class.classes.len == 0 {
		return
	}
	size := g.type_size(g.mod.values[val_id].typ)
	if g.value_is_aggregate(val_id) && size !in [1, 2, 4, 8] {
		x64_unsupported('backend feature: Windows ${context} lowering for value ${val_id}: ' +
			'aggregate return larger than 8 bytes reached direct return codegen; ' +
			'expected ABI lowering before codegen to use hidden sret pointer; check ABI lowering')
	}
	if !g.value_is_aggregate(val_id) && !g.value_is_float_type(val_id) && size !in [1, 2, 4, 8] {
		x64_unsupported('backend feature: Windows ${context} lowering for value ${val_id}: ' +
			'scalar return with unsupported storage width ${size} bytes reached codegen; ' +
			'expected ABI lowering before codegen; check ABI lowering')
	}
}

fn (g Gen) windows_value_passed_indirect(val_id int, marked_indirect bool, size int) bool {
	if marked_indirect {
		return true
	}
	return g.abi == .windows && g.value_is_aggregate(val_id) && size !in [1, 2, 4, 8]
}

fn (g Gen) call_arg_is_indirect(val_id int, arg_idx int, instr mir.Instruction) bool {
	marked_indirect := arg_idx >= 0 && arg_idx < instr.abi_arg_class.len
		&& instr.abi_arg_class[arg_idx] == .indirect
	return g.windows_value_passed_indirect(val_id, marked_indirect,
		g.type_size(g.mod.values[val_id].typ))
}

fn (mut g Gen) load_call_register_args(instr mir.Instruction, abi_regs []int, stack_args []bool, arg_position_base int) int {
	if g.abi.uses_positional_arg_regs() {
		return g.load_windows_call_register_args(instr, stack_args, arg_position_base)
	}

	mut reg_arg_idx := arg_position_base
	mut sse_arg_idx := 0
	float_arg_regs := g.abi.float_arg_regs()
	for i in 1 .. instr.operands.len {
		arg_idx := i - 1
		arg_id := instr.operands[i]
		if g.value_is_float_type(arg_id) {
			if sse_arg_idx >= float_arg_regs.len {
				g.unsupported_float_abi('stack argument', arg_id)
			}
			g.load_float_call_arg_to_xmm(float_arg_regs[sse_arg_idx], arg_id)
			sse_arg_idx++
			continue
		}
		if arg_idx < instr.abi_arg_classes.len {
			g.ensure_sysv_direct_aggregate_supported(arg_id, instr.abi_arg_classes[arg_idx],
				'argument')
		}
		if stack_args[arg_idx] {
			continue
		}
		if !g.call_arg_is_indirect(arg_id, arg_idx, instr) && g.value_is_aggregate(arg_id)
			&& arg_idx < instr.abi_arg_layouts.len && instr.abi_arg_layouts[arg_idx].locs.len > 0 {
			layout := instr.abi_arg_layouts[arg_idx]
			g.load_sysv_direct_aggregate_arg_to_regs(arg_id, layout, abi_regs)
			int_limit, sse_limit := sysv_layout_register_limits(layout)
			if int_limit > reg_arg_idx {
				reg_arg_idx = int_limit
			}
			if sse_limit > sse_arg_idx {
				sse_arg_idx = sse_limit
			}
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

fn (mut g Gen) load_windows_call_register_args(instr mir.Instruction, stack_args []bool, arg_position_base int) int {
	mut sse_arg_count := 0
	duplicate_vararg_float := g.call_needs_windows_vararg_float_duplication(instr)
	// Load positional Windows arguments from right to left. Some value loaders use
	// RCX as scratch, so loading arg0 first can corrupt it before the call.
	for i := instr.operands.len - 1; i >= 1; i-- {
		arg_idx := i - 1
		if stack_args[arg_idx] {
			continue
		}
		arg_id := instr.operands[i]
		position := arg_idx + arg_position_base
		if g.value_is_float_type(arg_id) {
			xmm := g.abi.float_arg_reg_for_position(position)
			if xmm == x64_no_arg_reg {
				g.unsupported_float_abi('stack argument', arg_id)
			}
			g.load_float_call_arg_to_xmm(xmm, arg_id)
			if duplicate_vararg_float {
				g.duplicate_windows_vararg_float_arg_to_gp(position, xmm)
			}
			sse_arg_count++
			continue
		}
		reg := g.abi.int_arg_reg_for_position(position)
		if reg == x64_no_arg_reg {
			continue
		}
		is_indirect := g.call_arg_is_indirect(arg_id, arg_idx, instr)
		size := g.type_size(g.mod.values[arg_id].typ)
		g.ensure_windows_scalar_or_indirect_arg(arg_id, is_indirect, size)
		g.load_call_arg_to_reg(reg, arg_id, arg_idx, instr)
	}
	return sse_arg_count
}

fn (g Gen) call_needs_windows_vararg_float_duplication(instr mir.Instruction) bool {
	if g.abi != .windows || instr.operands.len == 0 {
		return false
	}
	fn_val := g.mod.values[instr.operands[0]]
	return fn_val.name in ['snprintf', '_scprintf', '_snprintf']
}

fn (mut g Gen) duplicate_windows_vararg_float_arg_to_gp(position int, xmm int) {
	if position >= 4 {
		return
	}
	reg := g.abi.int_arg_reg_for_position(position)
	if reg == x64_no_arg_reg {
		return
	}
	g.emit_movq_reg_xmm(Reg(reg), xmm)
}

fn (mut g Gen) emit_movq_reg_xmm(dst Reg, src_xmm int) {
	dst_hw := g.map_reg(int(dst))
	mut rex := u8(0x48)
	if src_xmm >= 8 {
		rex |= 4
	}
	if dst_hw >= 8 {
		rex |= 1
	}
	g.emit(0x66)
	g.emit(rex)
	g.emit(0x0f)
	g.emit(0x7e)
	src := u8(src_xmm & 7)
	dst_bits := u8(dst_hw & 7)
	g.emit(0xc0 | (src << 3) | dst_bits)
}

fn (mut g Gen) load_float_call_arg_to_xmm(xmm int, val_id int) {
	g.ensure_float_abi_scalar(val_id, 'argument')
	g.load_float_val_to_xmm(xmm, val_id, g.type_size(g.mod.values[val_id].typ))
}

fn (mut g Gen) store_call_result(val_id int, ret_class mir.AbiValueClass) {
	g.ensure_windows_direct_return_supported(val_id, ret_class, 'call result')
	if g.value_is_float_type(val_id) {
		g.ensure_float_abi_scalar(val_id, 'call result')
		asm_store_xmm0_rbp_disp(mut g, g.stack_map[val_id], g.type_size(g.mod.values[val_id].typ))
		return
	}
	if g.store_sysv_direct_aggregate_call_result(val_id, ret_class) {
		return
	}
	g.ensure_sysv_direct_aggregate_supported(val_id, ret_class, 'call result')
	if g.store_sysv_integer_pair_call_result(val_id, ret_class) {
		return
	}
	g.normalize_integer_call_result(val_id)
	g.store_reg_to_val(0, val_id)
}

fn (mut g Gen) normalize_integer_call_result(val_id int) {
	typ_id := g.mod.values[val_id].typ
	g.normalize_integer_rax_for_type(typ_id, .sext, val_id)
}

fn (mut g Gen) normalize_integer_rax_for_type(typ_id int, op ssa.OpCode, val_id int) {
	if typ_id <= 0 || typ_id >= g.mod.type_store.types.len {
		return
	}
	typ := g.mod.type_store.types[typ_id]
	if typ.kind != .int_t {
		return
	}
	size := g.type_size(typ_id)
	if size == 8 {
		return
	}
	if typ.width == 1 {
		asm_mov_reg_imm32(mut g, rcx, 1)
		asm_and_rax_rcx(mut g)
		return
	}
	if typ.is_unsigned {
		g.mask_rax_to_size(size, .zext, val_id)
		return
	}
	match size {
		1 { asm_movsx_rax_al(mut g) }
		2 { asm_movsx_rax_ax(mut g) }
		4 { asm_movsxd_rax_eax(mut g) }
		else { g.unsupported_numeric_conversion(op, size, 8, val_id) }
	}
}

fn (g Gen) is_sysv_integer_pair_return(ret_class mir.AbiValueClass) bool {
	return g.abi == .sysv && ret_class.mode == .direct && ret_class.size > 8 && ret_class.size <= 16
		&& ret_class.classes.len == 2 && ret_class.classes[0] == .integer
		&& ret_class.classes[1] == .integer
}

fn (g Gen) is_sysv_direct_aggregate_return(val_id int, ret_class mir.AbiValueClass) bool {
	return g.abi == .sysv && ret_class.mode == .direct && g.value_is_aggregate(val_id)
		&& ret_class.classes.len > 0
}

fn (mut g Gen) store_sysv_direct_aggregate_call_result(val_id int, ret_class mir.AbiValueClass) bool {
	if !g.is_sysv_direct_aggregate_return(val_id, ret_class)
		|| g.is_sysv_integer_pair_return(ret_class) {
		return false
	}
	g.ensure_sysv_direct_aggregate_supported(val_id, ret_class, 'call result')
	off := g.stack_map[val_id]
	mut int_idx := 0
	mut sse_idx := 0
	mut i := 0
	for i < ret_class.classes.len {
		class := ret_class.classes[i]
		loc_off := i * 8
		chunk_size := sysv_abi_chunk_size(ret_class.size, loc_off)
		if chunk_size <= 0 {
			i++
			continue
		}
		match class {
			.no_class {
				i++
			}
			.integer {
				reg := sysv_int_return_reg(int_idx, 'call result')
				g.store_reg_to_rbp_exact(reg, off + loc_off, chunk_size)
				int_idx++
				i++
			}
			.sse {
				if sysv_class_has_sseup_pair(ret_class.classes, i, ret_class.size) {
					xmm := sysv_sse_return_reg(sse_idx, 'call result')
					asm_store_xmm_mem_base_disp_128(mut g, xmm, rbp, off + loc_off)
					sse_idx++
					i += 2
					continue
				}
				sysv_checked_sse_chunk_size(chunk_size, 'call result')
				xmm := sysv_sse_return_reg(sse_idx, 'call result')
				asm_store_xmm_rbp_disp(mut g, xmm, off + loc_off, chunk_size)
				sse_idx++
				i++
			}
			.sseup {
				x64_unsupported('backend feature: SysV direct aggregate call result with unpaired SSEUP eightbyte class is not implemented yet')
			}
			else {
				g.ensure_sysv_direct_aggregate_supported(val_id, ret_class, 'call result')
				i++
			}
		}
	}
	return true
}

fn (mut g Gen) load_sysv_direct_aggregate_return(ret_val_id int, ret_class mir.AbiValueClass) bool {
	if !g.is_sysv_direct_aggregate_return(ret_val_id, ret_class)
		|| g.is_sysv_integer_pair_return(ret_class) {
		return false
	}
	g.ensure_sysv_direct_aggregate_supported(ret_val_id, ret_class, 'return')
	g.load_struct_src_address_to_reg(int(r10), ret_val_id, g.cur_func_ret_type)
	mut int_idx := 0
	mut sse_idx := 0
	mut i := 0
	for i < ret_class.classes.len {
		class := ret_class.classes[i]
		loc_off := i * 8
		chunk_size := sysv_abi_chunk_size(ret_class.size, loc_off)
		if chunk_size <= 0 {
			i++
			continue
		}
		match class {
			.no_class {
				i++
			}
			.integer {
				reg := sysv_int_return_reg(int_idx, 'return')
				g.load_raw_mem_to_reg(reg, r10, loc_off, chunk_size)
				int_idx++
				i++
			}
			.sse {
				if sysv_class_has_sseup_pair(ret_class.classes, i, ret_class.size) {
					xmm := sysv_sse_return_reg(sse_idx, 'return')
					asm_load_xmm_mem_base_disp_128(mut g, xmm, r10, loc_off)
					sse_idx++
					i += 2
					continue
				}
				sysv_checked_sse_chunk_size(chunk_size, 'return')
				xmm := sysv_sse_return_reg(sse_idx, 'return')
				asm_load_xmm_mem_base_disp_size(mut g, xmm, r10, loc_off, chunk_size)
				sse_idx++
				i++
			}
			.sseup {
				x64_unsupported('backend feature: SysV direct aggregate return with unpaired SSEUP eightbyte class is not implemented yet')
			}
			else {
				g.ensure_sysv_direct_aggregate_supported(ret_val_id, ret_class, 'return')
				i++
			}
		}
	}
	return true
}

fn (mut g Gen) store_sysv_integer_pair_call_result(val_id int, ret_class mir.AbiValueClass) bool {
	if !g.is_sysv_integer_pair_return(ret_class) {
		return false
	}
	off := g.stack_map[val_id]
	g.store_reg_to_rbp_exact(rax, off, 8)
	second_size := ret_class.size - 8
	if second_size > 0 {
		g.store_reg_to_rbp_exact(rdx, off + 8, second_size)
	}
	return true
}

fn (mut g Gen) load_sysv_integer_pair_return(ret_val_id int, ret_class mir.AbiValueClass) bool {
	if !g.is_sysv_integer_pair_return(ret_class) {
		return false
	}
	g.load_struct_src_address_to_reg(int(r10), ret_val_id, g.cur_func_ret_type)
	second_size := ret_class.size - 8
	if second_size == 8 {
		g.load_raw_mem_to_reg(rax, r10, 0, 8)
		g.load_raw_mem_to_reg(rdx, r10, 8, second_size)
	} else if second_size > 0 {
		g.load_raw_mem_to_reg(rdx, r10, 8, second_size)
		g.load_raw_mem_to_reg(rax, r10, 0, 8)
	} else {
		g.load_raw_mem_to_reg(rax, r10, 0, 8)
	}
	return true
}

fn (mut g Gen) emit_sse_arg_count(count int) {
	if g.abi != .sysv {
		return
	}
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
	is_indirect := g.call_arg_is_indirect(val_id, arg_idx, instr)
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
	is_indirect := g.call_arg_is_indirect(val_id, arg_idx, instr)
	if is_indirect {
		return 1
	}
	size := g.type_size(g.mod.values[val_id].typ)
	if g.value_is_aggregate(val_id) || size > 8 {
		return (size + 7) / 8
	}
	return 1
}

fn (g Gen) call_stack_arg_mask(instr mir.Instruction, abi_reg_count int, arg_position_base int) []bool {
	num_args := instr.operands.len - 1
	mut stack_args := []bool{len: num_args}
	if g.abi.uses_positional_arg_regs() {
		for arg_idx := 0; arg_idx < num_args; arg_idx++ {
			position := arg_idx + arg_position_base
			if position >= g.abi.int_arg_regs().len {
				stack_args[arg_idx] = true
				continue
			}
			arg_id := instr.operands[arg_idx + 1]
			if g.value_is_float_type(arg_id) {
				g.ensure_float_abi_scalar(arg_id, 'argument')
				continue
			}
			if g.call_arg_reg_chunks(arg_id, arg_idx, instr) > 1 {
				g.unsupported_windows_abi_arg('aggregate register splitting reached codegen; ' +
					'expected ABI lowering before codegen to pass it indirectly or as one legal slot',
					arg_id)
			}
		}
		return stack_args
	}

	mut reg_arg_idx := arg_position_base
	mut sse_arg_idx := 0
	float_arg_regs := g.abi.float_arg_regs()
	for arg_idx := 0; arg_idx < num_args; arg_idx++ {
		arg_id := instr.operands[arg_idx + 1]
		if g.value_is_float_type(arg_id) {
			g.ensure_float_abi_scalar(arg_id, 'argument')
			if sse_arg_idx >= float_arg_regs.len {
				g.unsupported_float_abi('stack argument', arg_id)
			}
			sse_arg_idx++
			continue
		}
		if !g.call_arg_is_indirect(arg_id, arg_idx, instr) && g.value_is_aggregate(arg_id)
			&& arg_idx < instr.abi_arg_layouts.len && instr.abi_arg_layouts[arg_idx].locs.len > 0 {
			layout := instr.abi_arg_layouts[arg_idx]
			g.ensure_sysv_direct_aggregate_supported(arg_id, layout.value_class, 'argument')
			stack_args[arg_idx] = sysv_layout_uses_stack(layout)
			int_limit, sse_limit := sysv_layout_register_limits(layout)
			if int_limit > reg_arg_idx {
				reg_arg_idx = int_limit
			}
			if sse_limit > sse_arg_idx {
				sse_arg_idx = sse_limit
			}
			continue
		}
		if arg_idx < instr.abi_arg_classes.len {
			g.ensure_sysv_direct_aggregate_supported(arg_id, instr.abi_arg_classes[arg_idx],
				'argument')
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
	is_indirect := g.call_arg_is_indirect(val_id, arg_idx, instr)
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

			str_offset := g.rodata_len()
			g.add_rodata(raw_bytes)
			g.add_rodata_byte(0)
			sym_name := 'L_str_${g.curr_offset}_${str_offset}'
			sym_idx := g.add_symbol(sym_name, u64(str_offset), false, .rodata)

			// lea reg, [rip + disp]
			asm_lea_reg_rip(mut g, Reg(reg))
			g.add_rip_reloc(sym_idx)
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
	} else if val.kind == .func_ref {
		sym_idx := g.add_undefined(val.name)
		asm_lea_reg_rip(mut g, Reg(reg))
		g.add_rip_reloc(sym_idx)
		g.emit_u32(0)
	} else if val.kind == .c_string_literal {
		g.materialize_c_string_literal(reg, val_id)
	} else if val.kind == .global {
		sym_idx := g.add_undefined(val.name)
		if g.obj_format == .macho && val.index >= 0 && val.index < g.mod.globals.len
			&& g.mod.globals[val.index].linkage == .external {
			asm_mov_reg_got_rip(mut g, Reg(reg))
			g.add_macho_got_load_reloc(sym_idx)
		} else {
			asm_lea_reg_rip(mut g, Reg(reg))
			g.add_rip_reloc(sym_idx)
		}
		g.emit_u32(0)
	} else if val.kind == .string_literal {
		g.materialize_string_literal(reg, val_id)
	} else {
		if val.kind == .instruction {
			instr := g.mod.instrs[val.index]
			if instr.op == .alloca {
				if off := g.alloca_offsets[val_id] {
					asm_lea_reg_rbp_disp(mut g, Reg(reg), off)
					return
				}
			}
		}
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

fn (mut g Gen) materialize_c_string_literal(reg int, val_id int) {
	val := g.mod.values[val_id]
	raw_bytes := decode_c_string_literal_bytes(val.name)
	str_offset := g.rodata_len()
	g.add_rodata(raw_bytes)
	g.add_rodata_byte(0)
	sym_name := 'L_cstr_${g.curr_offset}_${str_offset}'
	sym_idx := g.add_symbol(sym_name, u64(str_offset), false, .rodata)
	asm_lea_reg_rip(mut g, Reg(reg))
	g.add_rip_reloc(sym_idx)
	g.emit_u32(0)
}

fn x64_c_escape_hex_digit(c u8) int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a`) + 10
	}
	if c >= `A` && c <= `F` {
		return int(c - `A`) + 10
	}
	return -1
}

fn x64_c_escape_is_octal_digit(c u8) bool {
	return c >= `0` && c <= `7`
}

fn decode_c_string_literal_bytes(raw string) []u8 {
	mut raw_bytes := []u8{cap: raw.len}
	mut i := 0
	for i < raw.len {
		if raw[i] == `\\` && i + 1 < raw.len {
			next := raw[i + 1]
			if x64_c_escape_is_octal_digit(next) {
				mut j := i + 1
				mut value := u32(0)
				mut digits := 0
				for j < raw.len && digits < 3 && x64_c_escape_is_octal_digit(raw[j]) {
					value = (value * 8 + u32(raw[j] - `0`)) & 0xff
					j++
					digits++
				}
				raw_bytes << u8(value)
				i = j
				continue
			}
			match next {
				`a` {
					raw_bytes << 7
				}
				`b` {
					raw_bytes << 8
				}
				`f` {
					raw_bytes << 12
				}
				`n` {
					raw_bytes << 10
				}
				`t` {
					raw_bytes << 9
				}
				`r` {
					raw_bytes << 13
				}
				`v` {
					raw_bytes << 11
				}
				`\\` {
					raw_bytes << 92
				}
				`"` {
					raw_bytes << 34
				}
				`'` {
					raw_bytes << 39
				}
				`?` {
					raw_bytes << 63
				}
				`x` {
					mut j := i + 2
					mut value := u32(0)
					mut saw_digit := false
					for j < raw.len {
						digit := x64_c_escape_hex_digit(raw[j])
						if digit < 0 {
							break
						}
						value = ((value << 4) + u32(digit)) & 0xff
						saw_digit = true
						j++
					}
					if saw_digit {
						raw_bytes << u8(value)
						i = j
						continue
					}
					raw_bytes << next
				}
				else {
					raw_bytes << next
				}
			}

			i += 2
		} else {
			raw_bytes << raw[i]
			i++
		}
	}
	return raw_bytes
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
	str_offset := g.rodata_len()
	g.add_rodata(val.name.bytes())
	g.add_rodata_byte(0)
	sym_name := 'L_str_${g.curr_offset}_${str_offset}'
	sym_idx := g.add_symbol(sym_name, u64(str_offset), false, .rodata)

	slot_off := g.stack_map[val_id]
	asm_lea_reg_rip(mut g, rax)
	g.add_rip_reloc(sym_idx)
	g.emit_u32(0)
	mut str_field_size := g.type_size(g.struct_field_type(val.typ, 0, 0))
	if str_field_size <= 0 {
		str_field_size = 8
	}
	mut len_field_size := g.type_size(g.struct_field_type(val.typ, 1, 0))
	if len_field_size <= 0 {
		len_field_size = 8
	}
	mut is_lit_field_size := g.type_size(g.struct_field_type(val.typ, 2, 0))
	if is_lit_field_size <= 0 {
		is_lit_field_size = 8
	}
	asm_store_rbp_disp_reg_size(mut g, slot_off + g.struct_field_offset_bytes(val.typ, 0), rax,
		str_field_size)
	asm_mov_reg_imm32(mut g, rax, u32(val.index))
	asm_store_rbp_disp_reg_size(mut g, slot_off + g.struct_field_offset_bytes(val.typ, 1), rax,
		len_field_size)
	asm_mov_reg_imm32(mut g, rax, 1)
	asm_store_rbp_disp_reg_size(mut g, slot_off + g.struct_field_offset_bytes(val.typ, 2), rax,
		is_lit_field_size)
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
			if typ.is_union {
				mut max_size := 0
				mut max_align := 1
				for field_typ in typ.fields {
					field_size := g.type_size(field_typ)
					if field_size > max_size {
						max_size = field_size
					}
					field_align := g.type_align(field_typ)
					if field_align > max_align {
						max_align = field_align
					}
				}
				if max_align > 1 && max_size % max_align != 0 {
					max_size = (max_size + max_align - 1) & ~(max_align - 1)
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
		if typ.kind == .struct_t && typ.is_union {
			mut max_align := 1
			for field_typ in typ.fields {
				align := g.type_align(field_typ)
				if align > max_align {
					max_align = align
				}
			}
			return max_align
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
	if val_id > 0 && val_id < g.mod.values.len && g.mod.values[val_id].kind == .string_literal {
		g.materialize_string_literal(reg, val_id)
		return
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

fn (mut g Gen) record_pending_label(blk int) {
	off := g.text_len() - g.curr_offset
	g.pending_labels[blk] << off
}

// Register Allocation Logic

fn (mut g Gen) allocate_registers(func mir.Function) {
	if g.abi == .windows {
		return
	}
	mut intervals := map[int]&Interval{}
	mut instr_idx := 0
	mut total_instrs := 0

	for blk_id in func.blocks {
		total_instrs += g.mod.blocks[blk_id].instrs.len
	}

	// Phi elimination lowers edge copies as `.assign dest, src` and leaves
	// placeholder `.bitcast` values for former phis. Keep these CFG-carried
	// values on the stack so branch order does not decide register lifetime.
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

	// Track which values are alloca results - don't register allocate these
	// as they hold addresses that may be needed across the function
	mut alloca_vals := map[int]bool{}

	for i, pid in func.params {
		param_size := g.type_size(g.mod.values[pid].typ)
		if i < func.abi_param_class.len && func.abi_param_class[i] == .indirect {
			alloca_vals[pid] = true
			continue
		}
		if g.value_is_float_type(pid) {
			alloca_vals[pid] = true
			continue
		}
		if g.value_is_aggregate(pid) || param_size > 8
			|| g.value_needs_raw_abi_reg_bytes(pid, param_size) {
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
				if unsafe { intervals[val_id] == nil } && !(val.kind == .instruction
					&& g.value_needs_stack_storage(val_id)) && val_id !in phi_related_vals {
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
	}

	mut block_of_def := map[int]int{}
	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			block_of_def[val_id] = blk_id
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
							if mut interval := intervals[op] {
								interval.end = total_instrs
							}
						}
					}
				}
			}
		}
	}

	mut sorted := []&Interval{}
	for _, i in intervals {
		sorted << i
		mut j := sorted.len - 1
		for j > 0 && sorted[j - 1].start > sorted[j].start {
			sorted[j - 1], sorted[j] = sorted[j], sorted[j - 1]
			j--
		}
	}

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
	subject := if context == 'stack parameter' {
		'stack-passed float parameter'
	} else if context == 'stack argument' {
		'stack-passed float argument'
	} else {
		'float ${context}'
	}
	x64_unsupported('backend feature: ${subject} (${size * 8}-bit type in value ${val_id}) ' +
		'is not implemented for this ABI yet')
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
	if g.value_is_aggregate(val_id) {
		return if size > 0 { size } else { 8 }
	}
	return if size > 8 { size } else { 8 }
}

fn (g Gen) value_needs_stack_storage(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.kind == .string_literal {
		return true
	}
	if g.value_is_eliminated_phi_placeholder(val_id) {
		return true
	}
	if val.typ <= 0 || val.typ >= g.mod.type_store.types.len {
		return false
	}
	if val.kind == .instruction {
		instr := g.mod.instrs[val.index]
		if instr.op == .bitcast && instr.operands.len > 0
			&& g.bitcast_touches_pointer(instr.operands[0], val.typ) {
			return false
		}
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

fn (g Gen) value_is_eliminated_phi_placeholder(val_id int) bool {
	if val_id <= 0 || val_id >= g.mod.values.len {
		return false
	}
	val := g.mod.values[val_id]
	if val.kind != .instruction || val.uses.len == 0 {
		return false
	}
	if val.index < 0 || val.index >= g.mod.instrs.len {
		return false
	}
	instr := g.mod.instrs[val.index]
	return instr.op == .bitcast && instr.operands.len == 0
}

fn (g Gen) bitcast_touches_pointer(src_id int, dst_typ int) bool {
	if dst_typ > 0 && dst_typ < g.mod.type_store.types.len
		&& g.mod.type_store.types[dst_typ].kind == .ptr_t {
		return true
	}
	if src_id <= 0 || src_id >= g.mod.values.len {
		return false
	}
	src_typ := g.mod.values[src_id].typ
	return src_typ > 0 && src_typ < g.mod.type_store.types.len
		&& g.mod.type_store.types[src_typ].kind == .ptr_t
}
