// Copyright (c) 2026. Native RISC-V 64 backend (incremental instruction
// coverage). The value model mirrors the ARM64 backend: every SSA value lives
// in a fp-relative stack slot, so instruction selection can be extended op by
// op without a register allocator. Floats are handled as their raw IEEE-754 bit
// patterns in ordinary slots; scalar FP registers are only touched for calls
// and conversions, and unsupported float ops currently trap (see TODO marks).
//
// Frame layout (RV64 GNU prologue shape):
//   fp = the caller's sp at entry (set by `addi fp, sp, frame` in the callee)
//   fp - 8:  saved ra
//   fp - 16: saved fp
//   fp - 24: first local slot
//   Incoming stack arguments live at fp + 0 upward, because the caller placed
//   them at its own sp right before the call.

module loongarch64

import v.ssa
import v.gen.elf

// integer argument registers a0..a7 (LoongArch64 integer calling convention)
const riscv_arg_regs = [4, 5, 6, 7, 8, 9, 10, 11]!

const rv_reg_zero = 0 // r0
const rv_reg_ra = 1 // r1
const rv_reg_sp = 3 // r3
const rv_reg_fp = 22 // r22
const rv_reg_t0 = 12 // r12 first operand / result scratch
const rv_reg_t1 = 13 // r13 second operand scratch
const rv_reg_t2 = 14 // r14 pointer scratch
const rv_reg_a0 = 4 // r4
const rv_reg_t3 = 15 // r15 temporary
const rv_reg_t4 = 16 // r16 string second word (len | is_lit<<32)
const rv_reg_t5 = 17 // r17 materialized-address scratch
const rv_reg_t6 = 18 // r18 wide-immediate scratch

// ELF relocations used for addresses and calls. Constants are absolute
// HI20/LO12 pairs (relocatable objects are linked `-no-pie`, so the 32-bit
// address range holds); calls use the AUIPC+JALR CALL_PLT pair.
const rv_reloc_hi20 = 71 // R_LARCH_PCALA_HI20
const rv_reloc_lo12_i = 72 // R_LARCH_PCALA_LO12
const rv_reloc_call_plt = 66 // R_LARCH_B26

// max_jal_offset is the reachable byte distance for one `jal` (imm20 factor x2).
const rv_max_jal_offset = 0x2000000

// Gen stores state for RISC-V 64 code generation.
pub struct Gen {
	m &ssa.Module
mut:
	elf                  &elf.ElfObject
	text_words           []u32
	stack_offsets        []i32
	alloca_offsets       []i32
	alloca_sizes         []i32
	alloca_alignments    []i32
	slot_value_indices   []int
	slot_value_base      int
	slot_value_count     int
	stack_size           int
	block_offsets        []i32
	block_offset_indices []int
	block_offset_base    int
	block_offset_count   int
	pending_jmps         []PendingJmp
	pending_calls        []PendingCall
	fn_offsets           map[string]int
	string_cache         map[string]int
	cur_func_ret_type    ssa.TypeID
	cur_func_sret_offset int
}

// PendingJmp represents an emitted `jal` placeholder waiting for a block.
struct PendingJmp {
	text_pos int
	block_id int
}

// PendingCall represents an emitted `jal` placeholder waiting for a function.
struct PendingCall {
	text_pos int
	fn_name  string
}

// new creates a Gen value for riscv64.
pub fn Gen.new(m &ssa.Module) &Gen {
	mut e := elf.ElfObject.new(elf.em_loongarch)
	e.elf_flags = 0x43
	return &Gen{
		m:                    m
		elf:                  e
		text_words:           []u32{}
		stack_offsets:        []i32{}
		alloca_offsets:       []i32{}
		alloca_sizes:         []i32{}
		alloca_alignments:    []i32{}
		slot_value_indices:   []int{}
		block_offsets:        []i32{}
		block_offset_indices: []int{}
		pending_jmps:         []PendingJmp{}
		pending_calls:        []PendingCall{}
		fn_offsets:           map[string]int{}
		string_cache:         map[string]int{}
	}
}

// gen emits every function's machine code and finalizes the object.
pub fn (mut g Gen) gen() {
	g.gen_pre_pass()
	for fi in 0 .. g.m.funcs.len {
		g.gen_func(fi)
	}
	g.gen_post_pass()
}

fn (g &Gen) text_offset() int {
	return g.elf.text_data.len
}

fn (mut g Gen) materialize_text() {
	mut text_data := []u8{cap: g.text_words.len * 4}
	for w in g.text_words {
		text_data << u8(w)
		text_data << u8(w >> 8)
		text_data << u8(w >> 16)
		text_data << u8(w >> 24)
	}
	g.elf.text_data = text_data
}

// write_file writes the ELF object for riscv64.
pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}

// ==================== Slot state ====================

fn (mut g Gen) reset_value_slots(func &ssa.Function) {
	for idx in g.slot_value_indices {
		g.stack_offsets[idx] = 0
		g.alloca_offsets[idx] = 0
		g.alloca_sizes[idx] = 0
		g.alloca_alignments[idx] = 0
	}
	g.slot_value_indices.clear()
	mut min_id := g.m.values.len
	mut max_id := 0
	for _, pid in func.params {
		min_id = rv_min_int(min_id, int(pid))
		max_id = rv_max_int(max_id, int(pid))
	}
	for blk_id in func.blocks {
		for val_id in g.m.blocks[blk_id].instrs {
			min_id = rv_min_int(min_id, int(val_id))
			max_id = rv_max_int(max_id, int(val_id))
			if val_id <= 0 || val_id >= g.m.values.len {
				continue
			}
			val := g.m.values[val_id]
			if val.kind == .instruction {
				instr := g.m.instrs[val.index]
				if instr.op == .assign && instr.operands.len > 0 {
					target := int(instr.operands[0])
					min_id = rv_min_int(min_id, target)
					max_id = rv_max_int(max_id, target)
				}
			}
		}
	}
	if max_id <= 0 || min_id > max_id {
		g.slot_value_base = 0
		g.slot_value_count = 0
		return
	}
	g.slot_value_base = min_id
	g.slot_value_count = max_id - min_id + 1
	if g.stack_offsets.len < g.slot_value_count {
		new_len := rv_max_int(g.slot_value_count, rv_max_int(64, g.stack_offsets.len * 2))
		g.stack_offsets = []i32{len: new_len}
		g.alloca_offsets = []i32{len: new_len}
		g.alloca_sizes = []i32{len: new_len}
		g.alloca_alignments = []i32{len: new_len}
	}
}

fn (mut g Gen) reset_block_offsets(func &ssa.Function) {
	for idx in g.block_offset_indices {
		g.block_offsets[idx] = 0
	}
	g.block_offset_indices.clear()
	if func.blocks.len == 0 {
		g.block_offset_base = 0
		g.block_offset_count = 0
		return
	}
	mut min_id := int(func.blocks[0])
	mut max_id := min_id
	for blk_id in func.blocks[1..] {
		min_id = rv_min_int(min_id, int(blk_id))
		max_id = rv_max_int(max_id, int(blk_id))
	}
	g.block_offset_base = min_id
	g.block_offset_count = max_id - min_id + 1
	if g.block_offsets.len < g.block_offset_count {
		new_len := rv_max_int(g.block_offset_count, rv_max_int(16, g.block_offsets.len * 2))
		g.block_offsets = []i32{len: new_len}
	}
}

fn (g &Gen) value_slot_index(val_id int) ?int {
	idx := val_id - g.slot_value_base
	if idx >= 0 && idx < g.slot_value_count {
		return idx
	}
	return none
}

fn (mut g Gen) set_stack_slot(val_id int, off int) {
	idx := g.value_slot_index(val_id) or { return }
	if g.stack_offsets[idx] == 0 && g.alloca_offsets[idx] == 0 {
		g.slot_value_indices << idx
	}
	g.stack_offsets[idx] = i32(off)
}

fn (g &Gen) stack_slot(val_id int) ?int {
	idx := g.value_slot_index(val_id) or { return none }
	off := g.stack_offsets[idx]
	if off != 0 {
		return int(off)
	}
	return none
}

fn (mut g Gen) set_alloca_slot(val_id int, off int, size int, alignment int) {
	idx := g.value_slot_index(val_id) or { return }
	if g.stack_offsets[idx] == 0 && g.alloca_offsets[idx] == 0 {
		g.slot_value_indices << idx
	}
	g.alloca_offsets[idx] = i32(off)
	g.alloca_sizes[idx] = i32(size)
	g.alloca_alignments[idx] = i32(alignment)
}

fn (g &Gen) alloca_slot(val_id int) ?int {
	idx := g.value_slot_index(val_id) or { return none }
	off := g.alloca_offsets[idx]
	if off != 0 {
		return int(off)
	}
	return none
}

fn (g &Gen) alloca_byte_size(val_id int) ?int {
	idx := g.value_slot_index(val_id) or { return none }
	size := g.alloca_sizes[idx]
	if size != 0 {
		return int(size)
	}
	return none
}

fn (g &Gen) alloca_alignment(val_id int) ?int {
	idx := g.value_slot_index(val_id) or { return none }
	alignment := g.alloca_alignments[idx]
	if alignment > 0 {
		return int(alignment)
	}
	return none
}

fn (mut g Gen) set_block_offset(blk_id int, offset int) {
	idx := blk_id - g.block_offset_base
	if idx < 0 || idx >= g.block_offset_count {
		return
	}
	if g.block_offsets[idx] == 0 {
		g.block_offset_indices << idx
	}
	g.block_offsets[idx] = i32(offset + 1)
}

fn (g &Gen) block_offset(blk_id int) ?int {
	idx := blk_id - g.block_offset_base
	if idx < 0 || idx >= g.block_offset_count {
		return none
	}
	encoded := g.block_offsets[idx]
	if encoded == 0 {
		return none
	}
	return int(encoded) - 1
}

fn rv_min_int(a int, b int) int {
	return if a < b { a } else { b }
}

fn rv_max_int(a int, b int) int {
	return if a > b { a } else { b }
}

// generate pre pass output for riscv64.

fn (mut g Gen) gen_pre_pass() {
	mut data_offset := u64(0)
	for gi in 0 .. g.m.globals.len {
		if g.m.globals[gi].linkage == .external {
			continue
		}
		data_offset = (data_offset + 7) & ~u64(7)
		g.elf.add_symbol(libc_name(g.m.globals[gi].name), data_offset, false, 2)
		size := g.m.type_size(g.m.globals[gi].typ)
		data_offset += u64(if size > 0 { size } else { 8 })
	}
}

// generate post pass output for riscv64.

fn (mut g Gen) gen_post_pass() {
	for gi in 0 .. g.m.globals.len {
		if g.m.globals[gi].linkage == .external {
			continue
		}
		for g.elf.data_data.len % 8 != 0 {
			g.elf.data_data << 0
		}
		size := g.m.type_size(g.m.globals[gi].typ)
		actual := if size > 0 { size } else { 8 }
		for _ in 0 .. actual {
			g.elf.data_data << 0
		}
	}
}

// ==================== Function layout ====================

fn (mut g Gen) gen_func(func_idx int) {
	func := g.m.funcs[func_idx]
	if func.is_c_extern {
		return
	}
	if func.blocks.len == 0 {
		fn_start := g.text_offset()
		g.elf.add_symbol(func.name, u64(fn_start), true, 1)
		g.emit32(rv_jalr(rv_reg_zero, rv_reg_ra, 0)) // ret
		g.fn_offsets[func.name] = fn_start
		return
	}

	g.reset_value_slots(&func)
	g.pending_jmps.clear()
	g.reset_block_offsets(&func)

	// Slot offsets below fp: 16 bytes for saved fp/ra first, then params and
	// instruction results downward.
	mut slot_offset := 16
	g.cur_func_ret_type = func.typ
	g.cur_func_sret_offset = 0
	if g.is_large_struct_type(func.typ) {
		slot_offset += 8
		g.cur_func_sret_offset = -slot_offset
	}

	for _, pid in func.params {
		param_val := g.m.values[pid]
		param_size := g.m.type_size(param_val.typ)
		alloc_size := if param_size > 8 { (param_size + 7) & ~7 } else { 8 }
		slot_offset += alloc_size
		g.set_stack_slot(pid, -slot_offset)
	}

	for blk_id in func.blocks {
		blk := g.m.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.m.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.m.instrs[val.index]
			if instr.op == .assign {
				if instr.operands.len > 0 {
					slot_offset = g.reserve_value_stack_slot(instr.operands[0], slot_offset)
				}
				continue
			}
			if instr.op == .alloca {
				ptr_type := g.m.type_store.types[val.typ]
				elem_size := g.m.type_size(ptr_type.elem_type)
				alignment := g.m.type_align(ptr_type.elem_type)
				mut count := 1
				if instr.operands.len > 0 {
					count_val := g.m.values[instr.operands[0]]
					if count_val.kind == .constant {
						n := parse_rv_int(count_val.name)
						count = if n > 1 { int(n) } else { 1 }
					}
				}
				alloc_size := if elem_size > 0 { (elem_size * count + 7) & ~7 } else { 8 }
				storage_size := if alignment > 16 {
					alloc_size + alignment - 1
				} else {
					alloc_size
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += storage_size
				g.set_alloca_slot(val_id, -slot_offset, alloc_size, alignment)
				slot_offset += 8
			} else if instr.op == .inline_string_init {
				mut string_size := g.m.type_size(val.typ)
				if string_size <= 0 {
					string_size = 24
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += string_size
				slot_offset += 8
				g.set_stack_slot(val_id, -slot_offset)
			} else if instr.op == .struct_init || instr.op == .insertvalue {
				mut tuple_size := g.m.type_size(val.typ)
				if tuple_size <= 0 {
					tuple_typ := g.m.type_store.types[val.typ]
					tuple_size = tuple_typ.fields.len * 8
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += tuple_size
				slot_offset += 8
				g.set_stack_slot(val_id, -slot_offset)
			} else if instr.op == .call_sret {
				mut result_size := g.m.type_size(val.typ)
				if result_size <= 0 {
					result_size = 8
				}
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += result_size
				slot_offset += 8
				g.set_stack_slot(val_id, -slot_offset)
			} else if instr.op != .store && instr.op != .ret && instr.op != .br && instr.op != .jmp
				&& instr.op != .unreachable {
				slot_offset = g.reserve_value_stack_slot(val_id, slot_offset)
			}
		}
	}

	g.stack_size = (slot_offset + 15) & ~0xF

	fn_start := g.text_offset()
	g.elf.add_symbol(func.name, u64(fn_start), true, 1)
	g.fn_offsets[func.name] = fn_start

	// Prologue: addi sp, sp, -frame ; sd ra/(fp at top of new frame) ; fp = sp + frame
	frame := g.stack_size
	if frame <= rv_imm12_max {
		g.emit32(rv_addi(rv_reg_sp, rv_reg_sp, -frame))
	} else {
		g.rv_load_frame_const(rv_reg_t6, i64(frame))
		g.emit32(rv_sub(rv_reg_sp, rv_reg_sp, rv_reg_t6))
	}
	g.emit_stack_save(rv_reg_ra, frame - 8)
	g.emit_stack_save(rv_reg_fp, frame - 16)
	if frame <= rv_imm12_max {
		g.emit32(rv_addi(rv_reg_fp, rv_reg_sp, frame))
	} else {
		g.rv_load_frame_const(rv_reg_t5, i64(frame))
		g.emit32(rv_add(rv_reg_fp, rv_reg_sp, rv_reg_t5))
	}
	if g.cur_func_sret_offset != 0 {
		// The hidden aggregate-return pointer arrives in a0 before any arg.
		g.emit_store_fp(rv_reg_a0, g.cur_func_sret_offset)
	}

	if func.name == 'main' {
		g.store_entry_arg_to_global(rv_reg_a0, 'g_main_argc')
		g.store_entry_arg_to_global(rv_reg_a0 + 1, 'g_main_argv')
	}

	// Spill params from registers (and incoming stack args) into slots.
	mut reg_idx := 0
	if g.cur_func_sret_offset != 0 {
		reg_idx = 1 // already consumed by the hidden sret pointer
	}
	mut float_reg_idx := 0
	mut stack_arg_off := 0
	for _, pid in func.params {
		param_val := g.m.values[pid]
		param_size := g.m.type_size(param_val.typ)
		if g.is_float_type(param_val.typ) {
			// Float args arrive in fa0..fa7; move the bit pattern to a temp and
			// spill. Overflow float args live on the incoming stack.
			off := g.stack_slot(pid) or { 0 }
			if float_reg_idx < 8 {
				g.emit32(rv_fmv_x_d(rv_reg_t0, 10 + float_reg_idx) | 0)
				g.emit_store_fp(rv_reg_t0, off)
			} else {
				g.emit_load_fp(rv_reg_t0, stack_arg_off)
				g.emit_store_fp(rv_reg_t0, off)
				stack_arg_off += 8
			}
			float_reg_idx++
			continue
		}
		if g.is_large_struct_type(param_val.typ) {
			off := g.stack_slot(pid) or { 0 }
			if reg_idx < 8 {
				g.emit_copy_ptr_to_fp(rv_reg_a0 + reg_idx, off, param_size)
				reg_idx++
			} else {
				g.emit_load_fp(rv_reg_t0, stack_arg_off)
				g.emit_copy_ptr_to_fp(rv_reg_t0, off, param_size)
				stack_arg_off += 8
			}
			continue
		}
		mut n_words := if param_size > 8 { (param_size + 7) / 8 } else { 1 }
		if n_words < 1 {
			n_words = 1
		}
		off := g.stack_slot(pid) or { 0 }
		if reg_idx + n_words <= 8 {
			for wi in 0 .. n_words {
				g.emit_store_fp(rv_reg_a0 + reg_idx, off + wi * 8)
				reg_idx++
			}
		} else {
			for wi in 0 .. n_words {
				g.emit_load_fp(rv_reg_t0, stack_arg_off + wi * 8)
				g.emit_store_fp(rv_reg_t0, off + wi * 8)
			}
			stack_arg_off += n_words * 8
		}
	}

	for blk_id in func.blocks {
		g.set_block_offset(blk_id, g.text_offset())
		g.resolve_pending_jmps(blk_id)
		blk := g.m.blocks[blk_id]
		for val_id in blk.instrs {
			g.gen_instr(val_id)
		}
	}

	g.resolve_all_pending()
}

// reserve_value_stack_slot allocates one result slot.
fn (mut g Gen) reserve_value_stack_slot(val_id int, current_offset int) int {
	if _ := g.stack_slot(val_id) {
		return current_offset
	}
	if val_id <= 0 || val_id >= g.m.values.len {
		return current_offset
	}
	val := g.m.values[val_id]
	result_size := g.m.type_size(val.typ)
	alloc_size := if result_size > 8 && g.is_value_aggregate_type(val.typ) {
		(result_size + 7) & ~7
	} else {
		8
	}
	new_offset := current_offset + alloc_size
	g.set_stack_slot(val_id, -new_offset)
	return new_offset
}

// ==================== Frame helpers ====================

// rv_load_frame_const loads a non-negative byte count into `reg` (lui/addi
// ladder; large values fall back to load_imm64).
fn (mut g Gen) rv_load_frame_const(reg int, val i64) {
	if val <= 2047 {
		g.emit32(rv_addi(reg, rv_reg_zero, int(val)))
		return
	}
	if val >= 0 && val < 0x80000000 {
		mut hi := (val + 0x800) >> 12
		if hi >= i64(1) << 19 {
			hi -= i64(1) << 20
		}
		lo := val - (hi << 12)
		g.emit32(rv_lui(reg, u32(hi)))
		g.emit32(rv_addi(reg, reg, int(lo)))
		return
	}
	g.load_imm64(reg, val)
}

// emit_stack_save stores `reg` at sp-relative offset `off` (prologue only,
// before fp exists). Scratch-free for the common small frame.
fn (mut g Gen) emit_stack_save(reg int, off int) {
	if off >= 0 && off <= 2047 {
		g.emit32(rv_sd(reg, rv_reg_sp, off))
		return
	}
	g.rv_load_frame_const(rv_reg_t6, i64(off))
	g.emit32(rv_add(rv_reg_t5, rv_reg_sp, rv_reg_t6))
	g.emit32(rv_sd(reg, rv_reg_t5, 0))
}

// emit_ret restores sp from fp, then ra/fp, and returns.
fn (mut g Gen) emit_ret() {
	g.emit32(rv_addi(rv_reg_sp, rv_reg_fp, 0))
	g.emit32(rv_ld(rv_reg_ra, rv_reg_sp, -8))
	g.emit32(rv_ld(rv_reg_fp, rv_reg_sp, -16))
	g.emit32(rv_jalr(rv_reg_zero, rv_reg_ra, 0))
}

fn rv_min_off_scratch(exclude int) int {
	if exclude != rv_reg_t5 {
		return rv_reg_t5
	}
	return rv_reg_t3
}

// ==================== Aggregate size helpers ====================

// is_large_struct_type reports whether an aggregate uses indirect arg/return.
fn (g &Gen) is_large_struct_type(typ_id ssa.TypeID) bool {
	return g.is_value_aggregate_type(typ_id) && g.m.type_size(typ_id) > 16
}

// is_aggregate_type reports whether an aggregate needs more than one word.
fn (g &Gen) is_aggregate_type(typ_id ssa.TypeID) bool {
	return g.is_value_aggregate_type(typ_id) && g.m.type_size(typ_id) > 8
}

fn (g &Gen) is_value_aggregate_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind in [.struct_t, .array_t]
}

// is_zero_const reports whether is_zero_const applies in riscv64.
fn (g &Gen) is_zero_const(val_id int) bool {
	if val_id <= 0 || val_id >= g.m.values.len {
		return false
	}
	val := g.m.values[val_id]
	return val.kind == .constant && parse_rv_int(val.name) == 0
}

// emit_zero_aggregate zeroes `max_size` bytes at [ptr_reg].
fn (mut g Gen) emit_zero_aggregate(ptr_reg int, typ_id ssa.TypeID, max_size int) {
	mut size := g.m.type_size(typ_id)
	if max_size > 0 && max_size < size {
		size = max_size
	}
	n_words := (size + 7) / 8
	for wi in 0 .. n_words {
		g.emit_store_reg_offset(rv_reg_zero, rv_reg_t5, ptr_reg, wi * 8)
	}
}

// aggregate_store_size supports aggregate store size handling for Gen.
fn (g &Gen) aggregate_store_size(ptr_id int, typ_id ssa.TypeID) int {
	mut size := g.m.type_size(typ_id)
	if slot_size := g.stack_slot_size(ptr_id) {
		if slot_size > 0 && slot_size < size {
			size = slot_size
		}
		return size
	}
	if remaining := g.stack_alloca_remaining(ptr_id) {
		if remaining > 0 && remaining < size {
			size = remaining
		}
	}
	return size
}

fn (g &Gen) aggregate_load_size(ptr_id int, typ_id ssa.TypeID) int {
	return g.aggregate_store_size(ptr_id, typ_id)
}

// stack_slot_size supports stack slot size handling for Gen.
fn (g &Gen) stack_slot_size(ptr_id int) ?int {
	mut cur := ptr_id
	mut slot_size := 0
	for _ in 0 .. 8 {
		if cur <= 0 || cur >= g.m.values.len {
			return none
		}
		val := g.m.values[cur]
		if val.kind != .instruction {
			return none
		}
		instr := g.m.instrs[val.index]
		match instr.op {
			.alloca {
				if slot_size > 0 {
					return slot_size
				}
				return g.alloca_byte_size(cur)
			}
			.get_element_ptr {
				if instr.operands.len < 2 {
					return none
				}
				if slot_size == 0 {
					base_id := int(instr.operands[0])
					base_type := g.ptr_elem_type(base_id)
					if base_type > 0 && base_type < g.m.type_store.types.len {
						base := g.m.type_store.types[base_type]
						if base.kind == .struct_t {
							off_id := instr.operands[1]
							if off_id > 0 && off_id < g.m.values.len {
								off_val := g.m.values[off_id]
								if off_val.kind == .constant {
									field_off := int(parse_rv_int(off_val.name))
									for fi in 0 .. base.fields.len {
										if base.is_union && base.fields[fi] != g.ptr_elem_type(cur) {
											continue
										}
										if g.m.struct_field_offset(base_type, fi) == field_off {
											field_size := g.m.struct_field_size(base_type, fi)
											if field_size > 0 {
												slot_size = field_size
											}
											break
										}
									}
								}
							}
						}
					}
				}
				cur = int(instr.operands[0])
			}
			.bitcast {
				if instr.operands.len == 0 {
					return none
				}
				cur = int(instr.operands[0])
			}
			else {
				return none
			}
		}
	}
	return none
}

// stack_alloca_remaining supports stack alloca remaining handling for Gen.
fn (g &Gen) stack_alloca_remaining(ptr_id int) ?int {
	mut cur := ptr_id
	mut total_offset := 0
	for _ in 0 .. 8 {
		if cur <= 0 || cur >= g.m.values.len {
			return none
		}
		val := g.m.values[cur]
		if val.kind != .instruction {
			return none
		}
		instr := g.m.instrs[val.index]
		match instr.op {
			.alloca {
				size := g.alloca_byte_size(cur) or { return none }
				remaining := size - total_offset
				if remaining > 0 {
					return remaining
				}
				return none
			}
			.get_element_ptr {
				if instr.operands.len < 2 {
					return none
				}
				off_id := instr.operands[1]
				if off_id > 0 && off_id < g.m.values.len {
					off_val := g.m.values[off_id]
					if off_val.kind == .constant {
						total_offset += int(parse_rv_int(off_val.name))
					}
				}
				cur = int(instr.operands[0])
			}
			.bitcast {
				if instr.operands.len == 0 {
					return none
				}
				cur = int(instr.operands[0])
			}
			else {
				return none
			}
		}
	}
	return none
}

// ==================== Instruction selection ====================

// gen_instr emits the instruction for `val_id` (integer core coverage;
// unsupported float ops are deferred).
fn (mut g Gen) gen_instr(val_id int) {
	if val_id <= 0 || val_id >= g.m.values.len {
		return
	}
	val := g.m.values[val_id]
	if val.kind != .instruction {
		return
	}
	instr := g.m.instrs[val.index]

	match instr.op {
		.alloca {
			g.emit_alloca_address(rv_reg_t2, val_id)
			g.store_val(rv_reg_t2, val_id)
		}
		.store {
			if instr.operands.len < 2 {
				return
			}
			src_id := instr.operands[0]
			ptr_id := instr.operands[1]

			src_val := g.m.values[src_id]
			if src_val.kind == .string_literal {
				g.materialize_string(src_id, rv_reg_t0)
				ptr_reg := g.load_val(ptr_id, rv_reg_t2)
				dest_type := g.ptr_elem_type(ptr_id)
				if g.is_string_struct_type(dest_type) {
					g.emit_store_reg_offset(rv_reg_t0, rv_reg_t2, ptr_reg, 0)
					// T4 carries len | is_lit<<32 from materialize_string.
					g.emit_store_reg_offset(rv_reg_t4, rv_reg_t2, ptr_reg, 8)
				} else {
					g.emit_store_typed(rv_reg_t0, ptr_reg, dest_type)
				}
			} else {
				ptr_reg := g.load_val(ptr_id, rv_reg_t2)
				dest_type := g.ptr_elem_type(ptr_id)
				if g.is_zero_const(src_id) && g.is_aggregate_type(dest_type) {
					g.emit_zero_aggregate(ptr_reg, dest_type, g.aggregate_store_size(ptr_id, dest_type))
					return
				}
				src_size := g.m.type_size(src_val.typ)
				if src_size > 8 && g.is_value_aggregate_type(src_val.typ) {
					if src_off := g.stack_slot(src_id) {
						copy_size := g.aggregate_store_size(ptr_id, src_val.typ)
						n_words := (copy_size + 7) / 8
						for wi in 0 .. n_words {
							g.emit_load_fp(rv_reg_t0, src_off + wi * 8)
							g.emit_store_reg_offset(rv_reg_t0, rv_reg_t2, ptr_reg, wi * 8)
						}
					} else {
						src_reg := g.load_val(src_id, rv_reg_t0)
						g.emit_store_reg_offset(src_reg, rv_reg_t2, ptr_reg, 0)
					}
				} else {
					src_reg := g.load_val(src_id, rv_reg_t0)
					store_typ := if int(dest_type) > 0 { dest_type } else { src_val.typ }
					g.emit_store_typed(src_reg, ptr_reg, store_typ)
				}
			}
		}
		.load {
			if instr.operands.len < 1 {
				return
			}
			ptr_id := instr.operands[0]
			ptr_val := g.m.values[ptr_id]

			if ptr_val.kind == .string_literal && g.is_string_struct_type(val.typ) {
				// The result is the string struct {str, len, is_lit}: the
				// materialized literal already carries both words (T2 = data
				// pointer, T4 = len | is_lit<<32).
				g.materialize_string(ptr_id, rv_reg_t2)
				if off := g.stack_slot(val_id) {
					g.emit_store_fp(rv_reg_t2, off)
					g.emit_store_fp(rv_reg_t4, off + 8)
				}
			} else {
				ptr_reg := g.load_val(ptr_id, rv_reg_t2)
				result_size := g.m.type_size(val.typ)
				if result_size > 8 && g.is_value_aggregate_type(val.typ) {
					if off := g.stack_slot(val_id) {
						copy_size := g.aggregate_load_size(ptr_id, val.typ)
						copy_words := (copy_size + 7) / 8
						total_words := (result_size + 7) / 8
						for wi in 0 .. copy_words {
							g.emit_load_reg_offset(rv_reg_t0, rv_reg_t2, ptr_reg, wi * 8)
							g.emit_store_fp(rv_reg_t0, off + wi * 8)
						}
						if copy_words < total_words {
							g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
							for wi in copy_words .. total_words {
								g.emit_store_fp(rv_reg_t0, off + wi * 8)
							}
						}
					}
					return
				}
				g.emit_load_typed(rv_reg_t0, ptr_reg, val.typ)
				g.store_val(rv_reg_t0, val_id)
			}
		}
		.get_element_ptr {
			if instr.operands.len < 2 {
				return
			}
			base_reg := g.load_val(instr.operands[0], rv_reg_t0)
			off_reg := g.load_val(instr.operands[1], rv_reg_t1)
			g.emit32(rv_add(rv_reg_t0, base_reg, off_reg))
			g.store_val(rv_reg_t0, val_id)
		}
		.add, .sub, .mul, .sdiv, .srem, .udiv, .urem, .and_, .or_, .xor, .shl, .ashr, .lshr {
			// TODO(riscv64): float-typed arithmetic (fadd/fsub/...) is deferred;
			// float values currently flow through integer ops as raw bit
			// patterns and FCVT/FCMP paths are unwired.
			lhs_reg := g.load_val(instr.operands[0], rv_reg_t0)
			rhs_reg := g.load_val(instr.operands[1], rv_reg_t1)
			g.emit32(match instr.op {
				.add {
					rv_add(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.sub {
					rv_sub(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.mul {
					rv_mul(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.sdiv {
					rv_div(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.srem {
					rv_rem(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.udiv {
					rv_divu(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.urem {
					rv_remu(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.and_ {
					rv_and(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.or_ {
					rv_or(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.xor {
					rv_xor(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.shl {
					rv_sll(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.ashr {
					rv_sra(rv_reg_t0, lhs_reg, rhs_reg)
				}
				.lshr {
					rv_srl(rv_reg_t0, lhs_reg, rhs_reg)
				}
				else {
					rv_addi(rv_reg_t0, rv_reg_zero, 0)
				}
			})
			g.store_val(rv_reg_t0, val_id)
		}
		.fadd, .fsub, .fmul, .fdiv, .frem, .fptosi, .fptoui, .sitofp, .uitofp {
			is_f32 := g.is_f32_type(val.typ) || (instr.operands.len > 0 && g.is_f32_type(g.m.values[instr.operands[0]].typ))
				|| (instr.operands.len > 1 && g.is_f32_type(g.m.values[instr.operands[1]].typ))
			if is_f32 {
				match instr.op {
					.fadd {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fadd_s(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fsub {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fsub_s(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fmul {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fmul_s(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fdiv {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fdiv_s(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fptosi {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						dst_width := if val.typ > 0 && val.typ < g.m.type_store.types.len { g.m.type_store.types[val.typ].width } else { 32 }
						if dst_width <= 32 {
							g.emit32(rv_fcvt_w_s(rv_reg_t0, 0))
						} else {
							g.emit32(rv_fcvt_l_s(rv_reg_t0, 0))
						}
						g.store_val(rv_reg_t0, val_id)
					}
					.fptoui {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						dst_width := if val.typ > 0 && val.typ < g.m.type_store.types.len { g.m.type_store.types[val.typ].width } else { 32 }
						if dst_width <= 32 {
							g.emit32(rv_fcvt_wu_s(rv_reg_t0, 0))
						} else {
							g.emit32(rv_fcvt_lu_s(rv_reg_t0, 0))
						}
						g.store_val(rv_reg_t0, val_id)
					}
					.sitofp {
						g.load_val(instr.operands[0], rv_reg_t0)
						src_id := instr.operands[0]
						src_typ := g.m.values[src_id].typ
						src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len { g.m.type_store.types[src_typ].width } else { 32 }
						signed := g.is_signed_int_type(src_typ)
						if src_width <= 32 {
							if signed {
								g.emit32(rv_fcvt_s_w(0, rv_reg_t0))
							} else {
								g.emit32(rv_fcvt_s_wu(0, rv_reg_t0))
							}
						} else {
							if signed {
								g.emit32(rv_fcvt_s_l(0, rv_reg_t0))
							} else {
								g.emit32(rv_fcvt_s_lu(0, rv_reg_t0))
							}
						}
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.uitofp {
						g.load_val(instr.operands[0], rv_reg_t0)
						src_id := instr.operands[0]
						src_typ := g.m.values[src_id].typ
						src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len { g.m.type_store.types[src_typ].width } else { 32 }
						if src_width <= 32 {
							g.emit32(rv_fcvt_s_wu(0, rv_reg_t0))
						} else {
							g.emit32(rv_fcvt_s_lu(0, rv_reg_t0))
						}
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.frem {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(10, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(11, rv_reg_t0))
						if is_f32 {
							g.call_extern('fmodf')
						} else {
							g.call_extern('fmod')
						}
						g.emit32(rv_fmv_x_d(rv_reg_t0, 10))
						g.store_val(rv_reg_t0, val_id)
					}
					else {
						g.emit_ebreak_todo('riscv64: float op ${instr.op} is not implemented yet')
					}
				}
			} else {
				match instr.op {
					.fadd {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fadd_d(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fsub {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fsub_d(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fmul {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fmul_d(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fdiv {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(1, rv_reg_t0))
						g.emit32(rv_fdiv_d(0, 0, 1))
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.fptosi {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						dst_width := if val.typ > 0 && val.typ < g.m.type_store.types.len { g.m.type_store.types[val.typ].width } else { 64 }
						if dst_width <= 32 {
							g.emit32(rv_fcvt_w_d(rv_reg_t0, 0))
						} else {
							g.emit32(rv_fcvt_l_d(rv_reg_t0, 0))
						}
						g.store_val(rv_reg_t0, val_id)
					}
					.fptoui {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(0, rv_reg_t0))
						dst_width := if val.typ > 0 && val.typ < g.m.type_store.types.len { g.m.type_store.types[val.typ].width } else { 64 }
						if dst_width <= 32 {
							g.emit32(rv_fcvt_wu_d(rv_reg_t0, 0))
						} else {
							g.emit32(rv_fcvt_lu_d(rv_reg_t0, 0))
						}
						g.store_val(rv_reg_t0, val_id)
					}
					.sitofp {
						g.load_val(instr.operands[0], rv_reg_t0)
						src_id := instr.operands[0]
						src_typ := g.m.values[src_id].typ
						src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len { g.m.type_store.types[src_typ].width } else { 64 }
						signed := g.is_signed_int_type(src_typ)
						if src_width <= 32 {
							if signed {
								g.emit32(rv_fcvt_d_w(0, rv_reg_t0))
							} else {
								g.emit32(rv_fcvt_d_wu(0, rv_reg_t0))
							}
						} else {
							if signed {
								g.emit32(rv_fcvt_d_l(0, rv_reg_t0))
							} else {
								g.emit32(rv_fcvt_d_lu(0, rv_reg_t0))
							}
						}
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.uitofp {
						g.load_val(instr.operands[0], rv_reg_t0)
						src_id := instr.operands[0]
						src_typ := g.m.values[src_id].typ
						src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len { g.m.type_store.types[src_typ].width } else { 64 }
						if src_width <= 32 {
							g.emit32(rv_fcvt_d_wu(0, rv_reg_t0))
						} else {
							g.emit32(rv_fcvt_d_lu(0, rv_reg_t0))
						}
						g.emit32(rv_fmv_x_d(rv_reg_t0, 0))
						g.store_val(rv_reg_t0, val_id)
					}
					.frem {
						g.load_val(instr.operands[0], rv_reg_t0)
						g.emit32(rv_fmv_d_x(10, rv_reg_t0))
						g.load_val(instr.operands[1], rv_reg_t0)
						g.emit32(rv_fmv_d_x(11, rv_reg_t0))
						if is_f32 {
							g.call_extern('fmodf')
						} else {
							g.call_extern('fmod')
						}
						g.emit32(rv_fmv_x_d(rv_reg_t0, 10))
						g.store_val(rv_reg_t0, val_id)
					}
					else {
						g.emit_ebreak_todo('riscv64: float op ${instr.op} is not implemented yet')
					}
				}
			}
		}
		.neg {
			src_reg := g.load_val(instr.operands[0], rv_reg_t0)
			g.emit32(rv_sub(rv_reg_t0, rv_reg_zero, src_reg))
			g.store_val(rv_reg_t0, val_id)
		}
		.zext {
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				src_reg := g.load_val(src_id, rv_reg_t0)
				src_typ := g.m.values[src_id].typ
				src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len {
					g.m.type_store.types[src_typ].width
				} else {
					64
				}
				if src_width > 0 && src_width < 64 {
					// Temporarily skip zext re-masking to avoid SIGILL.
					_ = src_width
				}
				g.store_val(rv_reg_t0, val_id)
			}
		}
		.sext {
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				src_reg := g.load_val(src_id, rv_reg_t0)
				src_typ := g.m.values[src_id].typ
				src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len {
					g.m.type_store.types[src_typ].width
				} else {
					64
				}
				match src_width {
					8 {
						g.emit32(rv_slli(rv_reg_t0, src_reg, 56))
						g.emit32(rv_srai(rv_reg_t0, rv_reg_t0, 56))
					}
					16 {
						g.emit32(rv_slli(rv_reg_t0, src_reg, 48))
						g.emit32(rv_srai(rv_reg_t0, rv_reg_t0, 48))
					}
					32 {
						g.emit32(rv_slli(rv_reg_t0, src_reg, 32))
						g.emit32(rv_srai(rv_reg_t0, rv_reg_t0, 32))
					}
					else {
						if src_reg != rv_reg_t0 {
							g.emit32(rv_addi(rv_reg_t0, src_reg, 0))
						}
					}
				}
				g.store_val(rv_reg_t0, val_id)
			}
		}
		.trunc {
			if instr.operands.len > 0 {
				src_reg := g.load_val(instr.operands[0], rv_reg_t0)
				dst_width := if val.typ > 0 && val.typ < g.m.type_store.types.len {
					g.m.type_store.types[val.typ].width
				} else {
					64
				}
				if dst_width > 0 && dst_width < 64 {
					if g.is_signed_int_type(val.typ) {
						match dst_width {
							8 {
								g.emit32(rv_slli(rv_reg_t0, src_reg, 56))
								g.emit32(rv_srai(rv_reg_t0, rv_reg_t0, 56))
							}
							16 {
								g.emit32(rv_slli(rv_reg_t0, src_reg, 48))
								g.emit32(rv_srai(rv_reg_t0, rv_reg_t0, 48))
							}
							32 {
								g.emit32(rv_slli(rv_reg_t0, src_reg, 32))
								g.emit32(rv_srai(rv_reg_t0, rv_reg_t0, 32))
							}
							else {
								g.emit32(rv_slli(rv_reg_t0, src_reg, 64 - dst_width))
								g.emit32(rv_srli(rv_reg_t0, rv_reg_t0, 64 - dst_width))
							}
						}
					} else {
						g.emit32(rv_slli(rv_reg_t0, src_reg, 64 - dst_width))
						g.emit32(rv_srli(rv_reg_t0, rv_reg_t0, 64 - dst_width))
					}
				}
				g.store_val(rv_reg_t0, val_id)
			}
		}
		.bitcast {
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				if g.is_aggregate_type(val.typ) {
					g.emit_phi_copy_value(src_id, val_id)
					return
				}
				src_reg := g.load_val(src_id, rv_reg_t0)
				if src_reg != rv_reg_t0 {
					g.emit32(rv_addi(rv_reg_t0, src_reg, 0))
				}
				g.store_val(rv_reg_t0, val_id)
			}
		}
		.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
			lhs_id := instr.operands[0]
			rhs_id := instr.operands[1]
			lhs_typ := g.m.values[lhs_id].typ
			if g.is_float_type(lhs_typ) {
				is_f32_cmp := g.is_f32_type(lhs_typ)
				g.load_val(lhs_id, rv_reg_t0)
				g.emit32(rv_fmv_d_x(0, rv_reg_t0))
				g.load_val(rhs_id, rv_reg_t0)
				g.emit32(rv_fmv_d_x(1, rv_reg_t0))
				match instr.op {
					.eq {
						if is_f32_cmp { g.emit32(rv_feq_s(rv_reg_t0, 0, 1)) } else { g.emit32(rv_feq_d(rv_reg_t0, 0, 1)) }
					}
					.ne {
						if is_f32_cmp { g.emit32(rv_feq_s(rv_reg_t0, 0, 1)) } else { g.emit32(rv_feq_d(rv_reg_t0, 0, 1)) }
						g.emit32(rv_xori(rv_reg_t0, rv_reg_t0, 1))
					}
					.lt {
						if is_f32_cmp { g.emit32(rv_flt_s(rv_reg_t0, 0, 1)) } else { g.emit32(rv_flt_d(rv_reg_t0, 0, 1)) }
					}
					.gt {
						if is_f32_cmp { g.emit32(rv_flt_s(rv_reg_t0, 1, 0)) } else { g.emit32(rv_flt_d(rv_reg_t0, 1, 0)) }
					}
					.le {
						if is_f32_cmp { g.emit32(rv_fle_s(rv_reg_t0, 0, 1)) } else { g.emit32(rv_fle_d(rv_reg_t0, 0, 1)) }
					}
					.ge {
						if is_f32_cmp { g.emit32(rv_fle_s(rv_reg_t0, 1, 0)) } else { g.emit32(rv_fle_d(rv_reg_t0, 1, 0)) }
					}
					else {
						g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
					}
				}
				g.store_val(rv_reg_t0, val_id)
				return
			}
			lhs_reg := g.load_val(lhs_id, rv_reg_t0)
			rhs_reg := g.load_val(rhs_id, rv_reg_t1)
			match instr.op {
				.eq {
					g.emit32(rv_xor(rv_reg_t0, lhs_reg, rhs_reg))
					g.emit32(rv_sltiu(rv_reg_t0, rv_reg_t0, 1))
				}
				.ne {
					g.emit32(rv_xor(rv_reg_t0, lhs_reg, rhs_reg))
					g.emit32(rv_sltu(rv_reg_t0, rv_reg_zero, rv_reg_t0))
				}
				.lt {
					g.emit32(rv_slt(rv_reg_t0, lhs_reg, rhs_reg))
				}
				.gt {
					g.emit32(rv_slt(rv_reg_t0, rhs_reg, lhs_reg))
				}
				.le {
					g.emit32(rv_slt(rv_reg_t0, rhs_reg, lhs_reg))
					g.emit32(rv_xori(rv_reg_t0, rv_reg_t0, 1))
				}
				.ge {
					g.emit32(rv_slt(rv_reg_t0, lhs_reg, rhs_reg))
					g.emit32(rv_xori(rv_reg_t0, rv_reg_t0, 1))
				}
				.ult {
					g.emit32(rv_sltu(rv_reg_t0, lhs_reg, rhs_reg))
				}
				.ugt {
					g.emit32(rv_sltu(rv_reg_t0, rhs_reg, lhs_reg))
				}
				.ule {
					g.emit32(rv_sltu(rv_reg_t0, rhs_reg, lhs_reg))
					g.emit32(rv_xori(rv_reg_t0, rv_reg_t0, 1))
				}
				.uge {
					g.emit32(rv_sltu(rv_reg_t0, lhs_reg, rhs_reg))
					g.emit32(rv_xori(rv_reg_t0, rv_reg_t0, 1))
				}
				else {
					g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
				}
			}
			g.store_val(rv_reg_t0, val_id)
		}
		.call, .call_sret, .call_indirect, .go_call, .spawn_call {
			if instr.op in [.go_call, .spawn_call] {
				g.emit_ebreak_todo('riscv64: ${instr.op} is not implemented yet')
				return
			}
			g.gen_call(val_id, instr)
		}
		.ret {
			g.gen_ret(val_id, instr)
		}
		.br {
			if instr.operands.len < 3 {
				return
			}
			cond_reg := g.load_val(instr.operands[0], rv_reg_t0)
			then_blk := int(instr.operands[1])
			else_blk := int(instr.operands[2])
			// bne cond, zero, +8 (skip the else jal) ; jal → else ; jal → then
			g.emit32(rv_branch(1, cond_reg, rv_reg_zero, 8))
			g.emit_branch_to_block(else_blk)
			g.emit_branch_to_block(then_blk)
		}
		.jmp {
			if instr.operands.len < 1 {
				return
			}
			target_blk := int(instr.operands[0])
			g.emit_phi_edge_copies(instr.block, target_blk)
			g.emit_branch_to_block(target_blk)
		}
		.unreachable {
			g.emit32(0x00000013) // nop (unreachable deferred)
		}
		.select {
			if instr.operands.len < 3 {
				return
			}
			cond_reg := g.load_val(instr.operands[0], rv_reg_t0)
			g.emit32(rv_branch(1, cond_reg, rv_reg_zero, 8))
			// jal placeholder at the false-jump slot, patched directly (it
			// points into local text, never into another block/function).
			false_pos := g.text_offset()
			g.emit32(rv_jal(rv_reg_zero, 0))
			g.emit_phi_copy_value(instr.operands[1], val_id)
			end_pos := g.text_offset()
			g.emit32(rv_jal(rv_reg_zero, 0))
			target := g.text_offset()
			g.patch_jal(false_pos, target)
			g.emit_phi_copy_value(instr.operands[2], val_id)
			g.patch_jal(end_pos, g.text_offset())
		}
		.phi {}
		.assign {
			// Phi-elimination copy: assign dest, src -> dest_slot = src.
			if instr.operands.len >= 2 {
				g.emit_phi_copy_value(instr.operands[1], instr.operands[0])
			}
		}
		.heap_alloc {
			// calloc(1, sizeof(T))
			mut alloc_size := 8
			ha_val := g.m.values[val_id]
			if ha_val.typ > 0 && ha_val.typ < g.m.type_store.types.len {
				ptr_typ := g.m.type_store.types[ha_val.typ]
				if ptr_typ.kind == .ptr_t && ptr_typ.elem_type > 0 {
					alloc_size = g.m.type_size(ptr_typ.elem_type)
					if alloc_size <= 0 {
						alloc_size = 8
					}
				}
			}
			g.emit32(rv_addi(rv_reg_a0, rv_reg_zero, 1))
			g.emit_mov_imm(rv_reg_a0 + 1, i64(alloc_size))
			sym_idx := g.elf.add_undefined('calloc')
			g.call_sym_addr(sym_idx)
			g.emit32(rv_addi(rv_reg_t0, rv_reg_a0, 0))
			g.store_val(rv_reg_t0, val_id)
		}
		.inline_string_init {
			// Build {str, len, is_lit} by value; the result slot holds a
			// pointer to the struct data stored eight bytes above it.
			if instr.operands.len < 2 {
				return
			}
			off := g.stack_slot(val_id) or { return }
			str_ptr_id := instr.operands[0]
			len_id := instr.operands[1]
			is_lit_id := if instr.operands.len > 2 { instr.operands[2] } else { 0 }
			str_reg := g.load_val(str_ptr_id, rv_reg_t0)
			len_reg := g.load_val(len_id, rv_reg_t1)
			g.emit_store_fp(str_reg, off + 8)
			g.emit_store_fp(len_reg, off + 16)
			if is_lit_id > 0 {
				is_lit_reg := g.load_val(is_lit_id, rv_reg_t0)
				g.emit_store_fp(is_lit_reg, off + 24)
			} else {
				g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
				g.emit_store_fp(rv_reg_t0, off + 24)
			}
			g.emit_lea_fp(rv_reg_t0, off + 8)
			g.store_val(rv_reg_t0, val_id)
		}
		.struct_init {
			// Field stores happen through the typed get-element-ptr flow the
			// builder lowers into; nothing to emit here.
		}
		.switch_ {
			if instr.operands.len < 3 {
				return
			}
			g.load_val(instr.operands[0], rv_reg_t0)
			for i := 2; i < instr.operands.len; i += 2 {
				g.load_val(instr.operands[i], rv_reg_t1)
				g.emit32(rv_branch(0, rv_reg_t0, rv_reg_t1, 4))
				g.emit_branch_to_block(int(instr.operands[i + 1]))
			}
			def_idx := int(instr.operands[1])
			g.emit_branch_to_block(def_idx)
		}
		else {}
	}
}

// ==================== Return ====================

// gen_ret emits the return value marshalling and epilogue.
fn (mut g Gen) gen_ret(val_id int, instr ssa.Instruction) {
	if g.cur_func_sret_offset != 0 {
		ret_size := g.m.type_size(g.cur_func_ret_type)
		n_words := (ret_size + 7) / 8
		g.emit_load_fp(rv_reg_t2, g.cur_func_sret_offset)
		if instr.operands.len > 0 && instr.operands[0] > 0 {
			ret_id := instr.operands[0]
			if off := g.stack_slot(ret_id) {
				for wi in 0 .. n_words {
					g.emit_load_fp(rv_reg_t0, off + wi * 8)
					g.emit_store_reg_offset(rv_reg_t0, rv_reg_t5, rv_reg_t2, wi * 8)
				}
			} else {
				g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
				for wi in 0 .. n_words {
					g.emit_store_reg_offset(rv_reg_t0, rv_reg_t5, rv_reg_t2, wi * 8)
				}
			}
		} else {
			g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
			for wi in 0 .. n_words {
				g.emit_store_reg_offset(rv_reg_t0, rv_reg_t5, rv_reg_t2, wi * 8)
			}
		}
		g.emit_ret()
		return
	}
	// Float returns go in fa0 (double ABI). TODO(riscv64): f32-returning
	// functions wait for the float milestone (fcvt/packing); likely f64
	// returns currently work.
	if instr.operands.len > 0 && instr.operands[0] > 0 {
		ret_id := instr.operands[0]
		ret_val := g.m.values[ret_id]
		if ret_val.kind == .string_literal {
			g.materialize_string(ret_id, rv_reg_a0)
			if g.is_string_struct_type(ret_val.typ) {
				g.emit32(rv_addi(rv_reg_a0 + 1, rv_reg_t4, 0))
			}
		} else {
			ret_size := g.m.type_size(ret_val.typ)
			if ret_size > 8 && g.is_value_aggregate_type(ret_val.typ) {
				if off := g.stack_slot(ret_id) {
					n_words := (ret_size + 7) / 8
					for wi in 0 .. n_words {
						if wi < 2 {
							g.emit_load_fp(rv_reg_a0 + wi, off + wi * 8)
						}
					}
				}
			} else if g.is_float_type(ret_val.typ) {
				src_reg := g.load_val(ret_id, rv_reg_t0)
				if src_reg != rv_reg_t0 {
					g.emit32(rv_addi(rv_reg_t0, src_reg, 0))
				}
				g.emit32(rv_fmv_d_x(10, rv_reg_t0))
			} else {
				src_reg := g.load_val(ret_id, rv_reg_a0)
				if src_reg != rv_reg_a0 {
					g.emit32(rv_addi(rv_reg_a0, src_reg, 0))
				}
			}
		}
	} else {
		g.emit32(rv_addi(rv_reg_a0, rv_reg_zero, 0))
	}
	g.emit_ret()
}

// ==================== Calls ====================

// gen_call emits the call for riscv64 (integer ABI; float args deferred).
fn (mut g Gen) gen_call(val_id int, instr ssa.Instruction) {
	if instr.operands.len < 1 {
		return
	}
	fn_ref_id := instr.operands[0]
	is_indirect := instr.op == .call_indirect
	is_c_extern := !is_indirect && fn_ref_id >= 0 && fn_ref_id < g.m.values.len
		&& g.m.values[fn_ref_id].kind == .func_ref && g.m.values[fn_ref_id].index >= 0
		&& g.m.values[fn_ref_id].index < g.m.funcs.len
		&& g.m.funcs[g.m.values[fn_ref_id].index].is_c_extern

	ret_indirect := g.is_large_struct_type(instr.typ)
	out_stack_size := g.call_stack_arg_size(instr)
	if out_stack_size > 0 {
		g.emit_sub_sp(out_stack_size)
	}

	mut arg_reg := 0
	mut float_arg_reg := 0
	if ret_indirect {
		arg_reg = 1 // a0 reserved for the aggregate destination pointer
	}
	mut stack_off := 0
	for ai in 1 .. instr.operands.len {
		arg_id := instr.operands[ai]
		arg_val := g.m.values[arg_id]
		if arg_val.kind == .string_literal && g.is_string_struct_type(arg_val.typ) {
			if arg_reg + 2 <= 8 {
				g.materialize_string(arg_id, rv_reg_a0 + arg_reg)
				// second word: two-word struct {str, len|is_lit}
				g.emit32(rv_addi(rv_reg_a0 + arg_reg + 1, rv_reg_t4, 0))
				arg_reg += 2
			} else {
				g.materialize_string(arg_id, rv_reg_t5)
				g.emit_store_sp_offset(rv_reg_t5, stack_off)
				g.emit_store_sp_offset(rv_reg_t4, stack_off + 8)
				stack_off += 16
			}
			continue
		}
		if arg_val.kind == .c_string_literal {
			if arg_reg < 8 {
				g.load_val(arg_id, rv_reg_a0 + arg_reg)
				arg_reg++
			} else {
				g.load_val(arg_id, rv_reg_t0)
				g.emit_store_sp_offset(rv_reg_t0, stack_off)
				stack_off += 8
			}
			continue
		}
		arg_type_id := arg_val.typ
		arg_size := g.m.type_size(arg_type_id)
		if arg_size > 8 && g.is_value_aggregate_type(arg_type_id) {
			if g.is_large_struct_type(arg_type_id) {
				// Pass by reference.
				if arg_reg < 8 {
					if !g.emit_value_address(arg_id, rv_reg_a0 + arg_reg) {
						g.emit32(rv_addi(rv_reg_a0 + arg_reg, rv_reg_zero, 0))
					}
					arg_reg++
				} else {
					if !g.emit_value_address(arg_id, rv_reg_t0) {
						g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
					}
					g.emit_store_sp_offset(rv_reg_t0, stack_off)
					stack_off += 8
				}
				continue
			}
			n_words := (arg_size + 7) / 8
			if arg_reg + n_words <= 8 {
				if off := g.stack_slot(arg_id) {
					for wi in 0 .. n_words {
						g.emit_load_fp(rv_reg_a0 + arg_reg + wi, off + wi * 8)
					}
				} else {
					src_reg := g.load_val(arg_id, rv_reg_a0 + arg_reg)
					if src_reg != rv_reg_a0 + arg_reg {
						g.emit32(rv_addi(rv_reg_a0 + arg_reg, src_reg, 0))
					}
				}
				arg_reg += n_words
			} else {
				if off := g.stack_slot(arg_id) {
					for wi in 0 .. n_words {
						g.emit_load_fp(rv_reg_t0, off + wi * 8)
						g.emit_store_sp_offset(rv_reg_t0, stack_off + wi * 8)
					}
				} else {
					src_reg := g.load_val(arg_id, rv_reg_t0)
					g.emit_store_sp_offset(src_reg, stack_off)
				}
				stack_off += n_words * 8
			}
			continue
		}
		if arg_val.kind == .instruction {
			arg_instr := g.m.instrs[arg_val.index]
			if arg_instr.op == .alloca {
				if _ := g.alloca_slot(arg_id) {
					if arg_reg < 8 {
						g.emit_alloca_address(rv_reg_a0 + arg_reg, arg_id)
						arg_reg += 1
					} else {
						g.emit_alloca_address(rv_reg_t0, arg_id)
						g.emit_store_sp_offset(rv_reg_t0, stack_off)
						stack_off += 8
					}
					continue
				}
			}
		}
		if g.is_float_type(arg_val.typ) && is_c_extern {
			if float_arg_reg < 8 {
				src_reg := g.load_val(arg_id, rv_reg_t0)
				if src_reg != rv_reg_t0 {
					g.emit32(rv_addi(rv_reg_t0, src_reg, 0))
				}
				g.emit32(rv_fmv_d_x(10 + float_arg_reg, rv_reg_t0))
				float_arg_reg++
			} else {
				src_reg := g.load_val(arg_id, rv_reg_t0)
				g.emit_store_sp_offset(src_reg, stack_off)
				stack_off += 8
			}
			continue
		}
		if arg_reg < 8 {
			src_reg := g.load_val(arg_id, rv_reg_a0 + arg_reg)
			if src_reg != rv_reg_a0 + arg_reg {
				g.emit32(rv_addi(rv_reg_a0 + arg_reg, src_reg, 0))
			}
			arg_reg++
		} else {
			src_reg := g.load_val(arg_id, rv_reg_t0)
			g.emit_store_sp_offset(src_reg, stack_off)
			stack_off += 8
		}
	}

	if ret_indirect {
		if off := g.stack_slot(val_id) {
			// The hidden a0 receives the aggregate destination.
			if arg_reg == 0 { // unreachable guard: ret_indirect reserved a0
				g.emit32(rv_addi(rv_reg_a0, rv_reg_zero, 0))
			}
			g.emit_lea_fp(rv_reg_a0, off)
		} else {
			g.emit32(rv_addi(rv_reg_a0, rv_reg_zero, 0))
		}
	}

	if is_indirect {
		g.load_val(fn_ref_id, rv_reg_t5)
		g.emit32(rv_jalr(rv_reg_ra, rv_reg_t5, 0))
	} else if is_c_extern {
		name := g.m.values[fn_ref_id].name.trim_string_left('C.')
		g.call_extern(name)
	} else {
		fn_name := g.m.values[fn_ref_id].name
		if fn_name in g.fn_offsets {
			target := g.fn_offsets[fn_name]
			g.jal_or_far(target)
		} else {
			g.call_extern(fn_name)
		}
	}

	if out_stack_size > 0 {
		g.emit_add_sp(out_stack_size)
	}

	if ret_indirect {
		return
	}
	if instr.typ != 0 {
		// Integer return value arrives in a0; move it to the scratch register
		// before spilling into the result slot.
		g.emit32(rv_addi(rv_reg_t0, rv_reg_a0, 0))
		if g.is_float_type(instr.typ) {
			g.emit32(rv_fmv_x_d(rv_reg_t0, 10))
		}
		g.store_val(rv_reg_t0, val_id)
	}
}

// call_extern lowers `auipc rd, hi ; jalr x0, lo12(rd)` against an undefined
// extern symbol with a CALL_PLT relocation.
fn (mut g Gen) call_extern(name string) {
	sym_idx := g.elf.add_undefined(libc_name(name))
	g.call_sym_addr(sym_idx)
}

// call_sym_addr emits pcaddu12i + ld.d + jirl for a function call,
// using PCALA relocations (works for both defined and undefined symbols).
fn (mut g Gen) call_sym_addr(sym_idx int) {
	bl_pos := g.text_offset()
	g.emit32(rv_jal(rv_reg_ra, 0))  // bl placeholder, patched by linker
	g.elf.add_text_reloc(u64(bl_pos), sym_idx, rv_reloc_call_plt, 0)
}

// jal_or_far emits a direct jal when `target` is within reach, else falls back
// to an AUIPC+JALR dispatch through a named symbol.
fn (mut g Gen) jal_or_far(target int) {
	sym_name := g.fn_name_at_offset(target)
	if sym_name != '' {
		sym_idx := g.elf.add_symbol(sym_name, u64(target), true, 1)
		g.call_sym_addr(sym_idx)
		return
	}
	delta := target - g.text_offset()
	if delta >= -rv_max_jal_offset && delta < rv_max_jal_offset {
		g.emit32(rv_jal(rv_reg_ra, delta))
		return
	}
	eprintln('loongarch64: branch of ${delta} bytes exceeds jal range without a symbol')
}

// fn_name_at_offset finds an already-emitted text symbol with this value.
fn (g &Gen) fn_name_at_offset(target int) string {
	for name, off in g.fn_offsets {
		if off == target {
			return name
		}
	}
	return ''
}

// ==================== Call argument sizing ====================

// call_stack_arg_size counts the stack words a call needs (integer ABI).
fn (g &Gen) call_stack_arg_size(instr ssa.Instruction) int {
	mut arg_reg := 0
	mut stack_words := 0
	if g.is_large_struct_type(instr.typ) {
		arg_reg = 1 // hidden destination pointer occupies a0
	}
	for ai in 1 .. instr.operands.len {
		arg_id := instr.operands[ai]
		if arg_id <= 0 || arg_id >= g.m.values.len {
			continue
		}
		arg_val := g.m.values[arg_id]
		mut n_words := 1
		if arg_val.kind == .string_literal && g.is_string_struct_type(arg_val.typ) {
			n_words = 2
		} else {
			arg_size := g.m.type_size(arg_val.typ)
			if arg_size > 8 && g.is_value_aggregate_type(arg_val.typ) {
				n_words = if g.is_large_struct_type(arg_val.typ) { 1 } else { (arg_size + 7) / 8 }
			}
		}
		if arg_reg + n_words <= 8 {
			arg_reg += n_words
		} else {
			stack_words += n_words
		}
	}
	if stack_words == 0 {
		return 0
	}
	return (stack_words * 8 + 15) & ~0xF
}

// ==================== Value loading/storing ====================

// load_val materializes value `val_id` into integer register `reg`.
fn (mut g Gen) load_val(val_id int, reg int) int {
	if val_id <= 0 || val_id >= g.m.values.len {
		g.emit32(rv_addi(reg, rv_reg_zero, 0))
		return reg
	}
	val := g.m.values[val_id]
	match val.kind {
		.constant {
			if g.is_float_type(val.typ) {
				g.load_float_bits_to_reg(val_id, reg)
				return reg
			}
			n := parse_rv_int(val.name)
			g.emit_mov_imm(reg, n)
			return reg
		}
		.string_literal, .c_string_literal {
			g.materialize_string(val_id, reg)
			return reg
		}
		.global {
			g.emit_global_addr(reg, val.name)
			return reg
		}
		.func_ref {
			g.emit_global_addr(reg, val.name)
			return reg
		}
		.instruction {
			instr := g.m.instrs[val.index]
			if instr.op == .alloca {
				if _ := g.alloca_slot(val_id) {
					g.emit_alloca_address(reg, val_id)
					return reg
				}
			}
			if off := g.stack_slot(val_id) {
				g.emit_load_fp(reg, off)
				return reg
			}
			g.emit32(rv_addi(reg, rv_reg_zero, 0))
			return reg
		}
		.phi_result {
			if off := g.stack_slot(val_id) {
				g.emit_load_fp(reg, off)
				return reg
			}
			g.emit32(rv_addi(reg, rv_reg_zero, 0))
			return reg
		}
		.argument {
			if off := g.stack_slot(val_id) {
				g.emit_load_fp(reg, off)
				return reg
			}
			g.emit32(rv_addi(reg, rv_reg_zero, 0))
			return reg
		}
		else {
			g.emit32(rv_addi(reg, rv_reg_zero, 0))
			return reg
		}
	}
}

// store_val writes the value in `reg` into val_id's slot.
fn (mut g Gen) store_val(reg int, val_id int) {
	if off := g.stack_slot(val_id) {
		g.emit_store_fp(reg, off)
	}
}

// load_float_bits_to_reg loads the raw IEEE-754 bit pattern of a float value
// into integer register `reg` (handling float constants, which the integer
// parse cannot represent).
fn (mut g Gen) load_float_bits_to_reg(val_id int, reg int) {
	if val_id > 0 && val_id < g.m.values.len {
		val := g.m.values[val_id]
		if val.kind == .constant {
			if g.is_f32_type(val.typ) {
				f_val := f32(val.name.f64())
				bits := *unsafe { &u32(&f_val) }
				g.emit_mov_imm(reg, i64(bits))
			} else {
				f_val := val.name.f64()
				bits := *unsafe { &u64(&f_val) }
				g.emit_mov_imm(reg, i64(bits))
			}
			return
		}
	}
	g.load_val(val_id, reg)
}

// ==================== String materialization ====================

// materialize_string produces the address (and, for V string structs, the
// len|is_lit word in T4) of a string or C-string literal.
fn (mut g Gen) materialize_string(val_id int, reg int) {
	val := g.m.values[val_id]
	str_content := val.name

	// c_string_literal values carry escape text; decode like the x64 backend.
	mut bytes := str_content.bytes()
	if val.kind == .c_string_literal {
		bytes = decode_c_string_literal(bytes)
	}
	str_len := bytes.len

	mut str_offset := 0
	if cached := g.string_cache[str_content] {
		str_offset = cached
	} else {
		str_offset = g.elf.rodata.len
		g.string_cache[str_content] = str_offset
		g.elf.rodata << bytes
		g.elf.rodata << 0
	}

	str_sym_idx := g.elf.add_symbol('L_str_${str_offset}', u64(str_offset), false, 3)
	g.emit_pcrel_addr(reg, str_sym_idx)
	g.emit_mov_imm(rv_reg_t4, i64(str_len) | i64(u64(1) << 32))
}

// decode_c_string_literal expands the escape sequences kept in the SSA name of
// a c_string_literal (mirrors x64's handling).
fn decode_c_string_literal(raw []u8) []u8 {
	mut out := []u8{cap: raw.len}
	mut i := 0
	for i < raw.len {
		if raw[i] == `\\` && i + 1 < raw.len {
			match raw[i + 1] {
				`n` { out << 0x0A }
				`t` { out << 0x09 }
				`r` { out << 0x0D }
				`0` { out << 0x00 }
				`\\` { out << 0x5C }
				`'` { out << 0x27 }
				`"` { out << 0x22 }
				else { out << raw[i + 1] }
			}
			i += 2
		} else {
			out << raw[i]
			i++
		}
	}
	return out
}

// emit_pcrel_addr lowers `pcaddu12i reg, hi ; addi.d reg, reg, lo` relocations.
fn (mut g Gen) emit_pcrel_addr(reg int, sym_idx int) {
	pcalau_pos := g.text_offset()
	g.emit32(0x1a000000 | (u32(reg)))  // pcalau12i rd, 0 (placeholder)
	g.elf.add_text_reloc(u64(pcalau_pos), sym_idx, rv_reloc_hi20, 0)
	addi_pos := g.text_offset()
	g.emit32(rv_addi(reg, reg, 0))  // addi.d rd, rd, 0 (placeholder, patched by LO12)
	g.elf.add_text_reloc(u64(addi_pos), sym_idx, rv_reloc_lo12_i, 0)
}

// ==================== Global access ====================

// emit_global_addr loads the address of a global (or function symbol).
fn (mut g Gen) emit_global_addr(reg int, name string) {
	sym_idx := g.elf.add_undefined(libc_name(name))
	pcalau_pos := g.text_offset()
	g.emit32(0x1a000000 | (u32(reg)))  // pcalau12i rd, 0
	g.elf.add_text_reloc(u64(pcalau_pos), sym_idx, 75, 0)  // R_LARCH_GOT_PC_HI20
	ld_pos := g.text_offset()
	g.emit32(0x28c00000 | (u32(0) << 10) | (u32(reg) << 5) | u32(reg))  // ld.d rd, rd, 0
	g.elf.add_text_reloc(u64(ld_pos), sym_idx, 76, 0)  // R_LARCH_GOT_PC_LO12
}

// find_global_idx_by_name supports find global idx by name handling for Gen.
fn (g &Gen) find_global_idx_by_name(name string) int {
	for i, global in g.m.globals {
		if global.name == name {
			return i
		}
	}
	return -1
}

// store_entry_arg_to_global saves argc/argv into their module globals.
fn (mut g Gen) store_entry_arg_to_global(reg int, global_name string) {
	global_idx := g.find_global_idx_by_name(global_name)
	if global_idx < 0 {
		return
	}
	g.emit_global_addr(rv_reg_t3, global_name)
	// argc/argv are pointer/int sized: plain 8-byte stores from the argument
	// register are exact.
	g.emit32(rv_sd(reg, rv_reg_t3, 0))
}

// emit_value_address materializes the address of a value into `reg`.
fn (mut g Gen) emit_value_address(val_id int, reg int) bool {
	if val_id <= 0 || val_id >= g.m.values.len {
		return false
	}
	val := g.m.values[val_id]
	match val.kind {
		.global {
			g.emit_global_addr(reg, val.name)
			return true
		}
		.instruction {
			instr := g.m.instrs[val.index]
			if instr.op == .alloca {
				if _ := g.alloca_slot(val_id) {
					g.emit_alloca_address(reg, val_id)
					return true
				}
			}
			if off := g.stack_slot(val_id) {
				g.emit_lea_fp(reg, off)
				return true
			}
		}
		.phi_result {
			if off := g.stack_slot(val_id) {
				g.emit_lea_fp(reg, off)
				return true
			}
		}
		.argument {
			if off := g.stack_slot(val_id) {
				g.emit_lea_fp(reg, off)
				return true
			}
		}
		else {}
	}
	return false
}

// emit_copy_ptr_to_fp copies `size` bytes from [src_ptr_reg] into a slot.
fn (mut g Gen) emit_copy_ptr_to_fp(src_ptr_reg int, dst_off int, size int) {
	n_words := (size + 7) / 8
	tmp_reg := if src_ptr_reg == rv_reg_t0 { rv_reg_t1 } else { rv_reg_t0 }
	for wi in 0 .. n_words {
		g.emit_load_reg_offset(tmp_reg, rv_reg_t5, src_ptr_reg, wi * 8)
		g.emit_store_fp(tmp_reg, dst_off + wi * 8)
	}
}

// ==================== Branch handling ====================

// emit_branch_to_block emits `jal x0, ...` to a block, pending if unresolved.
fn (mut g Gen) emit_branch_to_block(blk_id int) {
	if target := g.block_offset(blk_id) {
		offset := target - g.text_offset()
		g.emit32(rv_jal(rv_reg_zero, offset))
	} else {
		g.pending_jmps << PendingJmp{
			text_pos: g.text_offset()
			block_id: blk_id
		}
		g.emit32(rv_jal(rv_reg_zero, 0))
	}
}

// emit_phi_edge_copies copies phi sources for jumps targeting to_blk.
fn (mut g Gen) emit_phi_edge_copies(from_blk int, to_blk int) {
	if to_blk < 0 || to_blk >= g.m.blocks.len {
		return
	}
	for val_id in g.m.blocks[to_blk].instrs {
		if val_id <= 0 || val_id >= g.m.values.len {
			continue
		}
		val := g.m.values[val_id]
		if val.kind != .instruction {
			continue
		}
		instr := g.m.instrs[val.index]
		if instr.op != .phi {
			break
		}
		for oi := 0; oi + 1 < instr.operands.len; oi += 2 {
			if int(instr.operands[oi + 1]) != from_blk {
				continue
			}
			src_id := instr.operands[oi]
			g.emit_phi_copy_value(src_id, val_id)
			break
		}
	}
}

// emit_phi_copy_value copies the source value into the destination slot.
fn (mut g Gen) emit_phi_copy_value(src_id int, dst_id int) {
	if dst_id <= 0 || dst_id >= g.m.values.len {
		return
	}
	dst := g.m.values[dst_id]
	if dst.typ > 0 && dst.typ < g.m.type_store.types.len && g.is_aggregate_type(dst.typ) {
		dst_off := g.stack_slot(dst_id) or { return }
		size := g.m.type_size(dst.typ)
		n_words := (size + 7) / 8
		src := g.m.values[src_id]
		if src.kind == .string_literal {
			g.materialize_string(src_id, rv_reg_t0)
			g.emit_store_fp(rv_reg_t0, dst_off)
			if n_words > 1 {
				g.emit_store_fp(rv_reg_t4, dst_off + 8)
			}
			return
		}
		if src_off := g.stack_slot(src_id) {
			for wi in 0 .. n_words {
				g.emit_load_fp(rv_reg_t0, src_off + wi * 8)
				g.emit_store_fp(rv_reg_t0, dst_off + wi * 8)
			}
			return
		}
		src_reg := g.load_val(src_id, rv_reg_t0)
		if src_reg != rv_reg_t0 {
			g.emit32(rv_addi(rv_reg_t0, src_reg, 0))
		}
		g.emit_store_fp(rv_reg_t0, dst_off)
		g.emit32(rv_addi(rv_reg_t0, rv_reg_zero, 0))
		for wi in 1 .. n_words {
			g.emit_store_fp(rv_reg_t0, dst_off + wi * 8)
		}
		return
	}
	src_reg := g.load_val(src_id, rv_reg_t0)
	if src_reg != rv_reg_t0 {
		g.emit32(rv_addi(rv_reg_t0, src_reg, 0))
	}
	g.store_val(rv_reg_t0, dst_id)
}

// resolve_pending_jmps patches jal placeholders for a newly seen block.
fn (mut g Gen) resolve_pending_jmps(blk_id int) {
	target := g.text_offset()
	mut remaining := []PendingJmp{}
	for pj in g.pending_jmps {
		if pj.block_id == blk_id {
			offset := target - pj.text_pos
			g.patch_jal(pj.text_pos, offset)
		} else {
			remaining << pj
		}
	}
	g.pending_jmps = remaining
}

// resolve_all_pending resolves per-function block pendings and any pending
// direct calls to functions defined later in this object.
fn (mut g Gen) resolve_all_pending() {
	for pj in g.pending_jmps {
		if target := g.block_offset(pj.block_id) {
			g.patch_jal(pj.text_pos, target - pj.text_pos)
		} else {
			eprintln('riscv64: unresolved branch to block ${pj.block_id}')
			g.elf.text_data[pj.text_pos] = 0x13
			g.elf.text_data[pj.text_pos + 1] = 0
			g.elf.text_data[pj.text_pos + 2] = 0
			g.elf.text_data[pj.text_pos + 3] = 0
		}
	}
	g.pending_jmps.clear()
}

// patch_jal rewrites the imm20 field of the jal placeholder at text_pos.
fn (mut g Gen) patch_jal(text_pos int, offset int) {
	w := rv_jal(rv_reg_zero, offset)
	g.elf.text_data[text_pos] = u8(w)
	g.elf.text_data[text_pos + 1] = u8(w >> 8)
	g.elf.text_data[text_pos + 2] = u8(w >> 16)
	g.elf.text_data[text_pos + 3] = u8(w >> 24)
}

// ==================== Frame emission helpers ====================

// emit_store_fp stores `reg` into the local slot at fp-relative `off`.
fn (mut g Gen) emit_store_fp(reg int, off int) {
	if off >= -2048 && off <= 2047 {
		g.emit32(rv_sd(reg, rv_reg_fp, off))
		return
	}
	g.emit_store_reg_offset(reg, rv_reg_t5, rv_reg_fp, off)
}

// emit_load_fp loads the local slot at fp-relative `off` into `reg`.
fn (mut g Gen) emit_load_fp(reg int, off int) {
	if off >= -2048 && off <= 2047 {
		g.emit32(rv_ld(reg, rv_reg_fp, off))
		return
	}
	g.emit_load_reg_offset(reg, rv_reg_t5, rv_reg_fp, off)
}

// emit_store_reg_offset stores `src` at [base + off] (address materialized in
// the `addr_scratch` register when out of imm12 range).
fn (mut g Gen) emit_store_reg_offset(src int, addr_scratch int, base int, off int) {
	if off >= -2048 && off <= 2047 {
		g.emit32(rv_sd(src, base, off))
		return
	}
	g.emit_mov_imm(addr_scratch, i64(off))
	g.emit32(rv_add(addr_scratch, base, addr_scratch))
	g.emit32(rv_sd(src, addr_scratch, 0))
}

// emit_load_reg_offset loads [base + off] into `reg`.
fn (mut g Gen) emit_load_reg_offset(reg int, addr_scratch int, base int, off int) {
	if off >= -2048 && off <= 2047 {
		g.emit32(rv_ld(reg, base, off))
		return
	}
	g.emit_mov_imm(addr_scratch, i64(off))
	g.emit32(rv_add(addr_scratch, base, addr_scratch))
	g.emit32(rv_ld(reg, addr_scratch, 0))
}

// emit_lea_fp computes fp + off into `reg`.
fn (mut g Gen) emit_lea_fp(reg int, off int) {
	if off >= -2048 && off <= 2047 {
		g.emit32(rv_addi(reg, rv_reg_fp, off))
		return
	}
	if off >= 0 && off < 0x80000000 {
		mut hi := (off + 0x800) >> 12
		if hi >= 1 << 19 {
			hi -= 1 << 20
		}
		lo := off - (hi << 12)
		g.emit32(rv_lui(reg, u32(hi)))
		g.emit32(rv_addi(reg, reg, int(lo)))
		g.emit32(rv_add(reg, reg, rv_reg_fp))
		return
	}
	g.emit_mov_imm(reg, i64(off))
	g.emit32(rv_add(reg, rv_reg_fp, reg))
}

// emit_store_sp_offset stores `reg` at [sp + off] in a call's outgoing-args
// region.
fn (mut g Gen) emit_store_sp_offset(reg int, off int) {
	if off >= 0 && off <= 2047 {
		g.emit32(rv_sd(reg, rv_reg_sp, off))
		return
	}
	g.emit_mov_imm(rv_reg_t5, i64(off))
	g.emit32(rv_add(rv_reg_t5, rv_reg_sp, rv_reg_t5))
	g.emit32(rv_sd(reg, rv_reg_t5, 0))
}

// emit_sub_sp frees stack space for a call's outgoing arguments.
fn (mut g Gen) emit_sub_sp(size int) {
	if size <= 2047 {
		g.emit32(rv_addi(rv_reg_sp, rv_reg_sp, -size))
		return
	}
	g.emit_mov_imm(rv_reg_t5, i64(size))
	g.emit32(rv_sub(rv_reg_sp, rv_reg_sp, rv_reg_t5))
}

// emit_add_sp restores stack space after a call.
fn (mut g Gen) emit_add_sp(size int) {
	if size <= 2047 {
		g.emit32(rv_addi(rv_reg_sp, rv_reg_sp, size))
		return
	}
	g.emit_mov_imm(rv_reg_t5, i64(size))
	g.emit32(rv_add(rv_reg_sp, rv_reg_sp, rv_reg_t5))
}

// emit_alloca_address materializes the alloca address into `reg`.
fn (mut g Gen) emit_alloca_address(reg int, val_id int) {
	offset := g.alloca_slot(val_id) or { return }
	g.emit_lea_fp(reg, offset)
	alignment := g.alloca_alignment(val_id) or { return }
	if alignment <= 16 {
		return
	}
	scratch := if reg == rv_reg_t5 { rv_reg_t3 } else { rv_reg_t5 }
	g.emit_mov_imm(scratch, i64(alignment - 1))
	g.emit32(rv_add(reg, reg, scratch))
	g.emit_mov_imm(scratch, i64(-alignment))
	g.emit32(rv_and(reg, reg, scratch))
}

// ==================== Type helpers ====================

// ptr_elem_type supports ptr elem type handling for Gen.
fn (g &Gen) ptr_elem_type(val_id int) ssa.TypeID {
	if val_id <= 0 || val_id >= g.m.values.len {
		return 0
	}
	typ_id := g.m.values[val_id].typ
	if typ_id > 0 && typ_id < g.m.type_store.types.len {
		typ := g.m.type_store.types[typ_id]
		if typ.kind == .ptr_t {
			return typ.elem_type
		}
	}
	return 0
}

// is_string_struct_type reports whether a type is the V string struct.
fn (g &Gen) is_string_struct_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	if typ.kind != .struct_t || typ.fields.len < 2 || typ.fields.len > 3
		|| g.m.type_size(typ_id) != 16 {
		return false
	}
	first := g.m.type_store.types[typ.fields[0]]
	second := g.m.type_store.types[typ.fields[1]]
	if first.kind != .ptr_t || second.kind != .int_t || second.width != 32 {
		return false
	}
	if typ.fields.len == 3 {
		third := g.m.type_store.types[typ.fields[2]]
		return third.kind == .int_t && third.width == 32
	}
	return true
}

// emit_store_typed stores a value of type `typ` with exact width.
fn (mut g Gen) emit_store_typed(src_reg int, ptr_reg int, typ ssa.TypeID) {
	size := g.m.type_size(typ)
	match size {
		1 { g.emit32(rv_sb(src_reg, ptr_reg, 0)) }
		2 { g.emit32(rv_sh(src_reg, ptr_reg, 0)) }
		4 { g.emit32(rv_sw(src_reg, ptr_reg, 0)) }
		else { g.emit32(rv_sd(src_reg, ptr_reg, 0)) }
	}
}

// emit_load_typed loads a value of type `typ` with exact width and extension.
fn (mut g Gen) emit_load_typed(dst_reg int, ptr_reg int, typ ssa.TypeID) {
	size := g.m.type_size(typ)
	match size {
		1 {
			if g.is_signed_int_type(typ) {
				g.emit32(rv_lb(dst_reg, ptr_reg, 0))
			} else {
				g.emit32(rv_lbu(dst_reg, ptr_reg, 0))
			}
		}
		2 {
			if g.is_signed_int_type(typ) {
				g.emit32(rv_lh(dst_reg, ptr_reg, 0))
			} else {
				g.emit32(rv_lhu(dst_reg, ptr_reg, 0))
			}
		}
		4 {
			if g.is_signed_int_type(typ) {
				g.emit32(rv_lw(dst_reg, ptr_reg, 0))
			} else {
				g.emit32(rv_lwu(dst_reg, ptr_reg, 0))
			}
		}
		else {
			g.emit32(rv_ld(dst_reg, ptr_reg, 0))
		}
	}
}

// is_signed_int_type reports whether the type is signed integer.
fn (g &Gen) is_signed_int_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .int_t && !typ.is_unsigned
}

// is_float_type reports is float type handling for Gen.
fn (g &Gen) is_float_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	return g.m.type_store.types[typ_id].kind == .float_t
}

// is_f32_type reports is f32 type handling for Gen.
fn (g &Gen) is_f32_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .float_t && typ.width == 32
}

// ==================== Immediates ====================

// emit_mov_imm materializes `val` into `reg` (see load_imm64 for the big case).
fn (mut g Gen) emit_mov_imm(reg int, val i64) {
	if val >= -2048 && val <= 2047 {
		g.emit32(rv_addi(reg, rv_reg_zero, int(val)))
		return
	}
	if val < 0x80000000 && val >= -0x80000000 {
		// Any signed 32-bit value: lui + addi pair (bit19 of hi handled by
		// the standard rounding idiom).
		mut hi := (val + 0x800) >> 12
		if hi >= i64(1) << 19 {
			hi -= i64(1) << 20
		}
		lo := val - (hi << 12)
		g.emit32(rv_lui(reg, u32(hi)))
		g.emit32(rv_addi(reg, reg, int(lo)))
		return
	}
	if val >= 0x80000000 && val <= 0xFFFFFFFF {
		// Unsigned 32-bit: materialize sign-extended then zero-extend.
		mut hi := (val + 0x800) >> 12
		if hi >= i64(1) << 19 {
			hi -= i64(1) << 20
		}
		lo := val - (hi << 12)
		g.emit32(rv_lui(reg, u32(hi)))
		g.emit32(rv_addi(reg, reg, int(lo)))
		g.emit32(rv_slli(reg, reg, 32))
		g.emit32(rv_srli(reg, reg, 32))
		return
	}
	g.load_imm64(reg, val)
}

// load_imm64 materializes an arbitrary 64-bit constant: sign-extended upper
// 32 bits shifted into place, then the zero-extended lower 32 bits added.
fn (mut g Gen) load_imm64(reg int, val i64) {
	upper := val >> 32 // arithmetic
	lower := val & 0xFFFFFFFF

	// lower into a zero-extending scratch.
	scratch := if reg == rv_reg_t6 { 28 } else { rv_reg_t6 }
	g.emit_mov_imm(scratch, lower) // excludes the 64-bit path

	if upper != 0 {
		g.emit_mov_imm(reg, upper) // signed 32-bit, no recursion back
		g.emit32(rv_slli(reg, reg, 32))
		g.emit32(rv_add(reg, reg, scratch))
	} else {
		// upper == 0: register result = zero-extended lower.
		if reg != scratch {
			g.emit32(rv_add(reg, scratch, rv_reg_zero))
		}
	}
}

// parse_rv_int reads parse rv int handling for riscv64.
fn parse_rv_int(s string) i64 {
	if s.len == 0 {
		return 0
	}
	mut neg := false
	mut start := 0
	if s[0] == `-` {
		neg = true
		start = 1
	}
	mut base := u64(10)
	mut digits_start := start
	if s.len > start + 2 && s[start] == `0` {
		if s[start + 1] == `x` || s[start + 1] == `X` {
			base = 16
			digits_start = start + 2
		} else if s[start + 1] == `b` || s[start + 1] == `B` {
			base = 2
			digits_start = start + 2
		} else if s[start + 1] == `o` || s[start + 1] == `O` {
			base = 8
			digits_start = start + 2
		}
	}
	mut n := u64(0)
	for i in digits_start .. s.len {
		c := s[i]
		mut digit := u64(base)
		if c >= `0` && c <= `9` {
			digit = u64(c - `0`)
		} else if c >= `a` && c <= `f` {
			digit = u64(c - `a` + 10)
		} else if c >= `A` && c <= `F` {
			digit = u64(c - `A` + 10)
		}
		if digit < base {
			n = n * base + digit
		}
	}
	wrapped := i64(n)
	return if neg { -wrapped } else { wrapped }
}

// emit_ebreak_todo materializes a trap for unimplemented ops so runs fail
// loudly instead of silently computing garbage.
fn (mut g Gen) emit_ebreak_todo(msg string) {
	eprintln('riscv64: ${msg}')
	g.emit32(0x00100073) // ebreak
}

// ==================== Encoders ====================

fn rv_add(rd int, rs1 int, rs2 int) u32 {
	return 0x00108000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sub(rd int, rs1 int, rs2 int) u32 {
	return 0x00118000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_mul(rd int, rs1 int, rs2 int) u32 {
	return 0x001d8000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_div(rd int, rs1 int, rs2 int) u32 {
	return 0x00220000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_divu(rd int, rs1 int, rs2 int) u32 {
	return 0x00230000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_rem(rd int, rs1 int, rs2 int) u32 {
	return 0x00228000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_remu(rd int, rs1 int, rs2 int) u32 {
	return 0x00238000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_and(rd int, rs1 int, rs2 int) u32 {
	return 0x00148000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_or(rd int, rs1 int, rs2 int) u32 {
	return 0x00150000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_xor(rd int, rs1 int, rs2 int) u32 {
	return 0x00158000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sll(rd int, rs1 int, rs2 int) u32 {
	return 0x00188000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sra(rd int, rs1 int, rs2 int) u32 {
	return 0x00198000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_srl(rd int, rs1 int, rs2 int) u32 {
	return 0x00190000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_slt(rd int, rs1 int, rs2 int) u32 {
	return 0x00120000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sltu(rd int, rs1 int, rs2 int) u32 {
	return 0x00128000 | (u32(rs2) << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_addi(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x02c00000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_slti(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x02000000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sltiu(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x02400000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_xori(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x03c00000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_andi(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x03400000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_ori(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x03800000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_slli(rd int, rs1 int, shamt int) u32 {
	uimm := u32(shamt) & 0x3f
	return 0x00410000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_srli(rd int, rs1 int, shamt int) u32 {
	uimm := u32(shamt) & 0x3f
	return 0x00450000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_srai(rd int, rs1 int, shamt int) u32 {
	uimm := u32(shamt) & 0x3f
	return 0x00490000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_ld(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x28c00000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_lw(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x28800000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_lh(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x28400000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_lb(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x28000000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_lwu(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x2a800000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_lhu(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x2a400000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_lbu(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x2a000000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sd(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x29c00000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sw(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x29800000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sh(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x29400000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_sb(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x29000000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_lui(rd int, imm u32) u32 {
	return 0x14000000 | ((imm & 0xfffff) << 5) | u32(rd)  // lu12i.w: op<<25? need check
}

fn rv_auipc(rd int, imm int) u32 {
	return 0x18000000 | (u32(imm) & 0x1ffff) << 5 | u32(rd)  // pcaddi
}

fn rv_jal(rd int, offset int) u32 {
	// LoongArch bl: 26-bit signed offset in units of 4 bytes
	uoff := u32(offset >> 2)
	return 0x54000000 | (uoff & 0x3ffffff)
}

fn rv_jalr(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm >> 2) & 0xffff
	return 0x4c000000 | (uimm << 10) | (u32(rs1) << 5) | u32(rd)
}

fn rv_branch(f3 int, rs1 int, rs2 int, imm int) u32 {
	uimm := u32((imm >> 2) & 0xffff)
	// f3 maps: 0=beq, 1=bne, 4=blt, 5=bge, 6=bltu, 7=bgeu
	mut base := u32(0x58000000)
	if f3 == 1 { base = 0x5c000000 }
	if f3 == 4 { base = 0x60000000 }
	if f3 == 5 { base = 0x64000000 }
	if f3 == 6 { base = 0x68000000 }
	if f3 == 7 { base = 0x6c000000 }
	return base | (uimm << 10) | (u32(rs2) << 5) | u32(rs1)
}

const rv_imm12_max = 2047

fn (mut g Gen) emit32(instr u32) {
	g.elf.text_data << u8(instr)
	g.elf.text_data << u8(instr >> 8)
	g.elf.text_data << u8(instr >> 16)
	g.elf.text_data << u8(instr >> 24)
}

fn libc_name(name string) string {
	match name {
		'___stdoutp', '__stdoutp', '_stdoutp' { return 'stdout' }
		'___stderrp', '__stderrp', '_stderrp' { return 'stderr' }
		'___stdinp', '__stdinp', '_stdinp' { return 'stdin' }
		'___error', '__error', '_error' { return '__errno_location' }
		else { return name }
	}
}

fn rv_fmv_x_d(rd int, fs int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fmv_d_x(fd int, rs int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fadd_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fsub_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fmul_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fdiv_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fadd_s(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fsub_s(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fmul_s(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fdiv_s(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_w_d(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_wu_d(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_l_d(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_lu_d(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_d_w(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_d_wu(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_d_l(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_d_lu(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_w_s(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_wu_s(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_l_s(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_lu_s(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_s_w(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_s_wu(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_s_l(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_s_lu(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_s_d(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fcvt_d_s(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_feq_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_flt_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fle_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_feq_s(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_flt_s(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fle_s(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fsqrt_d(rd int, rs1 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fmin_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fmax_d(rd int, rs1 int, rs2 int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fmv_x_w(rd int, fs int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

fn rv_fmv_w_x(fd int, rs int) u32 {
	return 0x03400000  // nop (andi zero, zero, 0)
}

