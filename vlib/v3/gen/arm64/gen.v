module arm64

import v3.ssa

// Gen stores state for ARM64 code generation.
pub struct Gen {
mut:
	m                    &ssa.Module  = unsafe { nil }
	macho                &MachOObject = unsafe { nil }
	stack_offsets        []int
	alloca_offsets       []int
	alloca_sizes         []int
	stack_size           int
	block_offsets        []int
	pending_jmps         []PendingJmp
	fn_offsets           map[string]int
	string_cache         map[string]int
	cur_func_ret_type    ssa.TypeID
	cur_func_sret_offset int
}

// PendingJmp represents pending jmp data used by arm64.
struct PendingJmp {
	text_pos int
	block_id int
}

// new creates a Gen value for arm64.
pub fn Gen.new(m &ssa.Module) &Gen {
	return &Gen{
		m:              m
		macho:          MachOObject.new()
		stack_offsets:  []int{}
		alloca_offsets: []int{}
		alloca_sizes:   []int{}
		block_offsets:  []int{}
		pending_jmps:   []PendingJmp{}
		fn_offsets:     map[string]int{}
		string_cache:   map[string]int{}
	}
}

// gen supports gen handling for Gen.
pub fn (mut g Gen) gen() {
	g.gen_pre_pass()
	for fi in 0 .. g.m.funcs.len {
		g.gen_func(fi)
	}
	g.gen_post_pass()
}

// write_and_link writes and link output for arm64.
pub fn (mut g Gen) write_and_link(output string) {
	mut l := Linker.new(g.macho)
	l.link(output, '_main')
}

// reset_value_slots updates reset value slots state for arm64.
fn (mut g Gen) reset_value_slots() {
	n := g.m.values.len
	if g.stack_offsets.len != n {
		g.stack_offsets = []int{len: n}
		g.alloca_offsets = []int{len: n}
		g.alloca_sizes = []int{len: n}
		return
	}
	for i in 0 .. n {
		g.stack_offsets[i] = 0
		g.alloca_offsets[i] = 0
		g.alloca_sizes[i] = 0
	}
}

fn (mut g Gen) reset_block_offsets() {
	n := g.m.blocks.len
	if g.block_offsets.len != n {
		g.block_offsets = []int{len: n, init: -1}
		return
	}
	for i in 0 .. n {
		g.block_offsets[i] = -1
	}
}

// set_stack_slot updates set stack slot state for arm64.
fn (mut g Gen) set_stack_slot(val_id int, off int) {
	if val_id > 0 && val_id < g.stack_offsets.len {
		g.stack_offsets[val_id] = off
	}
}

// stack_slot supports stack slot handling for Gen.
fn (g &Gen) stack_slot(val_id int) ?int {
	if val_id > 0 && val_id < g.stack_offsets.len {
		off := g.stack_offsets[val_id]
		if off != 0 {
			return off
		}
	}
	return none
}

// set_alloca_slot updates set alloca slot state for arm64.
fn (mut g Gen) set_alloca_slot(val_id int, off int, size int) {
	if val_id > 0 && val_id < g.alloca_offsets.len {
		g.alloca_offsets[val_id] = off
		g.alloca_sizes[val_id] = size
	}
}

// alloca_slot supports alloca slot handling for Gen.
fn (g &Gen) alloca_slot(val_id int) ?int {
	if val_id > 0 && val_id < g.alloca_offsets.len {
		off := g.alloca_offsets[val_id]
		if off != 0 {
			return off
		}
	}
	return none
}

// alloca_byte_size supports alloca byte size handling for Gen.
fn (g &Gen) alloca_byte_size(val_id int) ?int {
	if val_id > 0 && val_id < g.alloca_sizes.len {
		size := g.alloca_sizes[val_id]
		if size != 0 {
			return size
		}
	}
	return none
}

// gen_pre_pass emits pre pass output for arm64.
fn (mut g Gen) gen_pre_pass() {
	mut data_offset := u64(0)
	for gi in 0 .. g.m.globals.len {
		data_offset = (data_offset + 7) & ~u64(7)
		g.macho.add_symbol('_' + g.m.globals[gi].name, data_offset, true, 3)
		size := g.m.type_size(g.m.globals[gi].typ)
		data_offset += u64(if size > 0 { size } else { 8 })
	}
}

// gen_post_pass emits post pass output for arm64.
fn (mut g Gen) gen_post_pass() {
	for gi in 0 .. g.m.globals.len {
		for g.macho.data_data.len % 8 != 0 {
			g.macho.data_data << 0
		}
		size := g.m.type_size(g.m.globals[gi].typ)
		actual := if size > 0 { size } else { 8 }
		for _ in 0 .. actual {
			g.macho.data_data << 0
		}
	}

	cstring_base := u64(g.macho.text_data.len)
	data_base := (cstring_base + u64(g.macho.str_data.len) + 7) & ~u64(7)
	for i in 0 .. g.macho.symbols.len {
		if g.macho.symbols[i].sect == 2 {
			g.macho.symbols[i].value += cstring_base
		} else if g.macho.symbols[i].sect == 3 {
			g.macho.symbols[i].value += data_base
		}
	}
}

// gen_func emits func output for arm64.
fn (mut g Gen) gen_func(func_idx int) {
	func := g.m.funcs[func_idx]
	if func.is_c_extern {
		return
	}
	if func.blocks.len == 0 {
		fn_start := g.macho.text_data.len
		sym_name := '_' + func.name
		g.macho.add_symbol(sym_name, u64(fn_start), false, 1)
		g.emit32(asm_ret())
		g.fn_offsets[func.name] = fn_start
		return
	}

	g.reset_value_slots()
	g.pending_jmps.clear()

	g.reset_block_offsets()

	// Frame layout (all at negative offsets from fp):
	// fp + 0: saved fp
	// fp + 8: saved lr
	// fp - 8: first local slot
	// fp - 16: second local slot ...
	mut slot_offset := 0
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
			if instr.op == .alloca {
				ptr_type := g.m.type_store.types[val.typ]
				elem_size := g.m.type_size(ptr_type.elem_type)
				mut count := 1
				if instr.operands.len > 0 {
					count_val := g.m.values[instr.operands[0]]
					if count_val.kind == .constant {
						n := parse_arm64_int(count_val.name)
						if n > 1 {
							count = int(n)
						} else {
							count = 1
						}
					} else {
						count = 1
					}
				}
				alloc_size := if elem_size > 0 { (elem_size * count + 7) & ~7 } else { 8 }
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += alloc_size
				g.set_alloca_slot(val_id, -slot_offset, alloc_size)
				slot_offset += 8
			} else if instr.op != .store && instr.op != .ret && instr.op != .br && instr.op != .jmp
				&& instr.op != .unreachable {
				result_size := g.m.type_size(val.typ)
				alloc_size := if result_size > 8 && val.typ > 0
					&& val.typ < g.m.type_store.types.len
					&& g.m.type_store.types[val.typ].kind == .struct_t {
					(result_size + 7) & ~7
				} else {
					8
				}
				slot_offset += alloc_size
				g.set_stack_slot(val_id, -slot_offset)
			}
		}
	}

	g.stack_size = (slot_offset + 15) & ~0xF

	fn_start := g.macho.text_data.len
	sym_name := '_' + func.name
	g.macho.add_symbol(sym_name, u64(fn_start), false, 1)
	g.fn_offsets[func.name] = fn_start

	// Prologue: stp fp, lr, [sp, -16]! ; mov fp, sp ; sub sp, sp, #frame
	g.emit32(asm_stp_fp_lr_pre())
	g.emit32(asm_mov_fp_sp())
	if g.stack_size > 0 {
		g.emit_sub_sp(g.stack_size)
	}
	if g.cur_func_sret_offset != 0 {
		g.emit_store_fp(8, g.cur_func_sret_offset)
	}

	if func.name == 'main' {
		g.store_entry_arg_to_global(0, 'g_main_argc')
		g.store_entry_arg_to_global(1, 'g_main_argv')
	}

	// Spill params from registers to stack. The AArch64 PCS allocates integer
	// (x0-x7) and float (d0-d7) argument registers from independent counters.
	mut reg_idx := 0
	mut float_reg_idx := 0
	mut stack_arg_off := 16
	for _, pid in func.params {
		param_val := g.m.values[pid]
		param_size := g.m.type_size(param_val.typ)
		if g.is_float_type(param_val.typ) {
			off := g.stack_slot(pid) or { 0 }
			if float_reg_idx < 8 {
				// Float arg arrives in dN; move the bit pattern to x8 and spill.
				g.emit32(asm_fmov_x_d(Reg(8), float_reg_idx))
				g.emit_store_fp(8, off)
			} else {
				g.emit_load_fp(8, stack_arg_off)
				g.emit_store_fp(8, off)
				stack_arg_off += 8
			}
			float_reg_idx++
			continue
		}
		if g.is_large_struct_type(param_val.typ) {
			off := g.stack_slot(pid) or { 0 }
			if reg_idx < 8 {
				g.emit_copy_ptr_to_fp(reg_idx, off, param_size)
				reg_idx++
			} else {
				g.emit_load_fp(8, stack_arg_off)
				g.emit_copy_ptr_to_fp(8, off, param_size)
				stack_arg_off += 8
			}
			continue
		}
		n_words := if param_size > 8 { (param_size + 7) / 8 } else { 1 }
		off := g.stack_slot(pid) or { 0 }
		if reg_idx + n_words <= 8 {
			for wi in 0 .. n_words {
				g.emit_store_fp(reg_idx, off + wi * 8)
				reg_idx++
			}
		} else {
			for wi in 0 .. n_words {
				g.emit_load_fp(8, stack_arg_off + wi * 8)
				g.emit_store_fp(8, off + wi * 8)
			}
			stack_arg_off += n_words * 8
		}
	}

	// Generate blocks
	for blk_id in func.blocks {
		g.block_offsets[blk_id] = g.macho.text_data.len
		g.resolve_pending_jmps(blk_id)
		blk := g.m.blocks[blk_id]
		for val_id in blk.instrs {
			g.gen_instr(val_id)
		}
	}

	g.resolve_all_pending()
}

// is_large_struct_type reports whether is large struct type applies in arm64.
fn (g &Gen) is_large_struct_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .struct_t && g.m.type_size(typ_id) > 16
}

// is_aggregate_type reports whether is aggregate type applies in arm64.
fn (g &Gen) is_aggregate_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .struct_t && g.m.type_size(typ_id) > 8
}

// is_zero_const reports whether is zero const applies in arm64.
fn (g &Gen) is_zero_const(val_id int) bool {
	if val_id <= 0 || val_id >= g.m.values.len {
		return false
	}
	val := g.m.values[val_id]
	return val.kind == .constant && parse_arm64_int(val.name) == 0
}

// emit_zero_aggregate emits emit zero aggregate output for arm64.
fn (mut g Gen) emit_zero_aggregate(ptr_reg int, typ_id ssa.TypeID, max_size int) {
	mut size := g.m.type_size(typ_id)
	if max_size > 0 && max_size < size {
		size = max_size
	}
	n_words := (size + 7) / 8
	for wi in 0 .. n_words {
		g.emit32(asm_str_imm(xzr, Reg(ptr_reg), u32(wi)))
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

// aggregate_load_size supports aggregate load size handling for Gen.
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
									field_off := int(parse_arm64_int(off_val.name))
									for fi in 0 .. base.fields.len {
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
						total_offset += int(parse_arm64_int(off_val.name))
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

// gen_instr emits instr output for arm64.
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
			off := g.alloca_slot(val_id) or { return }
			g.emit_lea_fp(8, off)
			g.store_val(8, val_id)
		}
		.store {
			if instr.operands.len < 2 {
				return
			}
			src_id := instr.operands[0]
			ptr_id := instr.operands[1]

			src_val := g.m.values[src_id]
			if src_val.kind == .string_literal {
				g.materialize_string(src_id, 8)
				ptr_reg := g.load_val(ptr_id, 9)
				g.emit32(asm_str(Reg(8), Reg(ptr_reg)))
				g.emit32(asm_str_imm(Reg(10), Reg(ptr_reg), 1))
			} else {
				src_size := g.m.type_size(src_val.typ)
				if src_size > 8 && src_val.typ > 0 && src_val.typ < g.m.type_store.types.len
					&& g.m.type_store.types[src_val.typ].kind == .struct_t {
					if src_off := g.stack_slot(src_id) {
						ptr_reg := g.load_val(ptr_id, 9)
						copy_size := g.aggregate_store_size(ptr_id, src_val.typ)
						n_words := (copy_size + 7) / 8
						for wi in 0 .. n_words {
							g.emit_load_fp(8, src_off + wi * 8)
							g.emit32(asm_str_imm(Reg(8), Reg(ptr_reg), u32(wi)))
						}
					} else {
						src_reg := g.load_val(src_id, 8)
						ptr_reg := g.load_val(ptr_id, 9)
						g.emit32(asm_str(Reg(src_reg), Reg(ptr_reg)))
					}
				} else {
					ptr_reg := g.load_val(ptr_id, 9)
					dest_type := g.ptr_elem_type(ptr_id)
					if g.is_zero_const(src_id) && g.is_aggregate_type(dest_type) {
						g.emit_zero_aggregate(ptr_reg, dest_type, g.aggregate_store_size(ptr_id,
							dest_type))
						return
					}
					src_reg := g.load_val(src_id, 8)
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

			if ptr_val.kind == .global {
				g.emit_global_addr(8, ptr_val.name)
				g.emit32(asm_ldr(Reg(8), Reg(8)))
				g.store_val(8, val_id)
			} else if ptr_val.kind == .string_literal {
				g.materialize_string(ptr_id, 8)
				g.store_val(8, val_id)
				if off := g.stack_slot(val_id) {
					g.emit_store_fp(10, off + 8)
				}
			} else {
				ptr_reg := g.load_val(ptr_id, 9)
				result_size := g.m.type_size(val.typ)
				if result_size > 8 && val.typ > 0 && val.typ < g.m.type_store.types.len {
					typ := g.m.type_store.types[val.typ]
					if typ.kind == .struct_t {
						if off := g.stack_slot(val_id) {
							if g.is_string_struct_type(val.typ) {
								copy_size := g.aggregate_load_size(ptr_id, val.typ)
								if copy_size > 0 {
									g.emit32(asm_ldr(Reg(8), Reg(ptr_reg)))
									g.emit_store_fp(8, off)
								} else {
									g.emit_mov_imm(8, 0)
									g.emit_store_fp(8, off)
								}
								if copy_size > 8 {
									g.emit32(asm_ldr_imm(Reg(10), Reg(ptr_reg), 1))
									g.emit_store_fp(10, off + 8)
								} else {
									g.emit_mov_imm(10, 0)
									g.emit_store_fp(10, off + 8)
								}
							} else {
								copy_size := g.aggregate_load_size(ptr_id, val.typ)
								copy_words := (copy_size + 7) / 8
								total_words := (result_size + 7) / 8
								for wi in 0 .. copy_words {
									g.emit32(asm_ldr_imm(Reg(8), Reg(ptr_reg), u32(wi)))
									g.emit_store_fp(8, off + wi * 8)
								}
								if copy_words < total_words {
									g.emit_mov_imm(8, 0)
									for wi in copy_words .. total_words {
										g.emit_store_fp(8, off + wi * 8)
									}
								}
							}
						}
						return
					}
				}
				g.emit_load_typed(8, ptr_reg, val.typ)
				g.store_val(8, val_id)
			}
		}
		.get_element_ptr {
			if instr.operands.len < 2 {
				return
			}
			base_reg := g.load_val(instr.operands[0], 8)
			off_reg := g.load_val(instr.operands[1], 9)
			g.emit32(asm_add_reg(Reg(8), Reg(base_reg), Reg(off_reg)))
			g.store_val(8, val_id)
		}
		.add, .sub, .mul, .sdiv, .srem, .udiv, .urem, .and_, .or_, .xor, .shl, .ashr, .lshr {
			// Float-typed arithmetic reaches here only via the builder's unary
			// minus lowering (`0 - x`), which emits an integer `.sub` over a
			// float value. Redirect any float-typed arithmetic to the FP path so
			// the bit pattern is negated/operated on as a real IEEE-754 value.
			if g.is_float_type(val.typ)
				&& instr.op in [.add, .sub, .mul, .sdiv, .udiv, .srem, .urem] {
				fop := match instr.op {
					.add { ssa.OpCode.fadd }
					.sub { ssa.OpCode.fsub }
					.mul { ssa.OpCode.fmul }
					.sdiv, .udiv { ssa.OpCode.fdiv }
					else { ssa.OpCode.frem }
				}

				g.gen_float_binop(fop, instr.operands[0], instr.operands[1], val_id)
				return
			}
			lhs_reg := g.load_val(instr.operands[0], 8)
			rhs_reg := g.load_val(instr.operands[1], 9)
			match instr.op {
				.add {
					g.emit32(asm_add_reg(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.sub {
					g.emit32(asm_sub_reg(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.mul {
					g.emit32(asm_mul(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.sdiv {
					g.emit32(asm_sdiv(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.srem {
					g.emit32(asm_sdiv(Reg(10), Reg(lhs_reg), Reg(rhs_reg)))
					g.emit32(asm_msub(Reg(8), Reg(10), Reg(rhs_reg), Reg(lhs_reg)))
				}
				.udiv {
					g.emit32(asm_udiv(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.urem {
					g.emit32(asm_udiv(Reg(10), Reg(lhs_reg), Reg(rhs_reg)))
					g.emit32(asm_msub(Reg(8), Reg(10), Reg(rhs_reg), Reg(lhs_reg)))
				}
				.and_ {
					g.emit32(asm_and(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.or_ {
					g.emit32(asm_orr(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.xor {
					g.emit32(asm_eor(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.shl {
					g.emit32(asm_lslv(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.ashr {
					g.emit32(asm_asrv(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				.lshr {
					g.emit32(asm_lsrv(Reg(8), Reg(lhs_reg), Reg(rhs_reg)))
				}
				else {}
			}

			g.store_val(8, val_id)
		}
		.fadd, .fsub, .fmul, .fdiv, .frem {
			g.gen_float_binop(instr.op, instr.operands[0], instr.operands[1], val_id)
		}
		.fptosi {
			g.load_float_operand(instr.operands[0], 0)
			g.emit32(asm_fcvtzs_x_d(Reg(8), 0))
			g.store_val(8, val_id)
		}
		.fptoui {
			g.load_float_operand(instr.operands[0], 0)
			g.emit32(asm_fcvtzu_x_d(Reg(8), 0))
			g.store_val(8, val_id)
		}
		.sitofp {
			src_reg := g.load_val(instr.operands[0], 8)
			g.emit32(asm_scvtf_d_x(0, Reg(src_reg)))
			g.store_float_result(val_id)
		}
		.uitofp {
			src_reg := g.load_val(instr.operands[0], 8)
			g.emit32(asm_ucvtf_d_x(0, Reg(src_reg)))
			g.store_float_result(val_id)
		}
		.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge {
			lhs_typ := g.m.values[instr.operands[0]].typ
			if g.is_float_type(lhs_typ) {
				// Float comparison: FCMP sets NZCV; the same condition codes used
				// for signed integers give the expected ordered results (matching
				// the v2 backend).
				g.load_float_operand(instr.operands[0], 0)
				g.load_float_operand(instr.operands[1], 1)
				g.emit32(asm_fcmp_d(Reg(0), Reg(1)))
			} else {
				lhs_reg := g.load_val(instr.operands[0], 8)
				rhs_reg := g.load_val(instr.operands[1], 9)
				g.emit32(asm_cmp_reg(Reg(lhs_reg), Reg(rhs_reg)))
			}
			match instr.op {
				.eq { g.emit32(asm_cset_eq(Reg(8))) }
				.ne { g.emit32(asm_cset_ne(Reg(8))) }
				.lt { g.emit32(asm_cset_lt(Reg(8))) }
				.gt { g.emit32(asm_cset_gt(Reg(8))) }
				.le { g.emit32(asm_cset_le(Reg(8))) }
				.ge { g.emit32(asm_cset_ge(Reg(8))) }
				.ult { g.emit32(asm_cset_lo(Reg(8))) }
				.ugt { g.emit32(asm_cset_hi(Reg(8))) }
				.ule { g.emit32(asm_cset_ls(Reg(8))) }
				.uge { g.emit32(asm_cset_hs(Reg(8))) }
				else {}
			}

			g.store_val(8, val_id)
		}
		.neg {
			src_reg := g.load_val(instr.operands[0], 8)
			g.emit32(asm_sub_reg(Reg(8), xzr, Reg(src_reg)))
			g.store_val(8, val_id)
		}
		.zext {
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				src_reg := g.load_val(src_id, 8)
				src_typ := g.m.values[src_id].typ
				src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len {
					g.m.type_store.types[src_typ].width
				} else {
					64
				}
				if src_width > 0 && src_width < 64 {
					g.emit32(asm_ubfx_lower(Reg(8), Reg(src_reg), u32(src_width)))
				} else if src_reg != 8 {
					g.emit32(asm_mov_reg(Reg(8), Reg(src_reg)))
				}
				g.store_val(8, val_id)
			}
		}
		.sext {
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				src_reg := g.load_val(src_id, 8)
				src_typ := g.m.values[src_id].typ
				src_width := if src_typ > 0 && src_typ < g.m.type_store.types.len {
					g.m.type_store.types[src_typ].width
				} else {
					64
				}
				match src_width {
					8 {
						g.emit32(asm_sxtb(Reg(8), Reg(src_reg)))
					}
					16 {
						g.emit32(asm_sxth(Reg(8), Reg(src_reg)))
					}
					32 {
						g.emit32(asm_sxtw(Reg(8), Reg(src_reg)))
					}
					else {
						if src_reg != 8 {
							g.emit32(asm_mov_reg(Reg(8), Reg(src_reg)))
						}
					}
				}

				g.store_val(8, val_id)
			}
		}
		.trunc {
			if instr.operands.len > 0 {
				src_reg := g.load_val(instr.operands[0], 8)
				dst_width := if val.typ > 0 && val.typ < g.m.type_store.types.len {
					g.m.type_store.types[val.typ].width
				} else {
					64
				}
				if dst_width > 0 && dst_width < 64 {
					if g.is_signed_int_type(val.typ) {
						match dst_width {
							8 {
								g.emit32(asm_sxtb(Reg(8), Reg(src_reg)))
							}
							16 {
								g.emit32(asm_sxth(Reg(8), Reg(src_reg)))
							}
							32 {
								g.emit32(asm_sxtw(Reg(8), Reg(src_reg)))
							}
							else {
								g.emit32(asm_ubfx_lower(Reg(8), Reg(src_reg), u32(dst_width)))
							}
						}
					} else {
						g.emit32(asm_ubfx_lower(Reg(8), Reg(src_reg), u32(dst_width)))
					}
				} else if src_reg != 8 {
					g.emit32(asm_mov_reg(Reg(8), Reg(src_reg)))
				}
				g.store_val(8, val_id)
			}
		}
		.bitcast {
			if instr.operands.len > 0 {
				src_id := instr.operands[0]
				src_typ := g.m.values[src_id].typ
				// The builder lowers f32<->f64 casts as a bitcast. Those need a
				// real representation conversion, not a bit copy: widen the source
				// into a `d` register and narrow back per the result type.
				if g.is_float_type(src_typ) && g.is_float_type(val.typ)
					&& g.is_f32_type(src_typ) != g.is_f32_type(val.typ) {
					g.load_float_operand(src_id, 0)
					g.store_float_result(val_id)
					return
				}
				src_reg := g.load_val(src_id, 8)
				if src_reg != 8 {
					g.emit32(asm_mov_reg(Reg(8), Reg(src_reg)))
				}
				g.store_val(8, val_id)
			}
		}
		.call {
			g.gen_call(val_id, instr)
		}
		.call_indirect {
			g.gen_call(val_id, instr)
		}
		.ret {
			if g.cur_func_sret_offset != 0 {
				ret_size := g.m.type_size(g.cur_func_ret_type)
				n_words := (ret_size + 7) / 8
				g.emit_load_fp(9, g.cur_func_sret_offset)
				if instr.operands.len > 0 && instr.operands[0] > 0 {
					ret_id := instr.operands[0]
					if off := g.stack_slot(ret_id) {
						for wi in 0 .. n_words {
							g.emit_load_fp(8, off + wi * 8)
							g.emit32(asm_str_imm(Reg(8), Reg(9), u32(wi)))
						}
					} else {
						g.emit_mov_imm(8, 0)
						for wi in 0 .. n_words {
							g.emit32(asm_str_imm(Reg(8), Reg(9), u32(wi)))
						}
					}
				} else {
					g.emit_mov_imm(8, 0)
					for wi in 0 .. n_words {
						g.emit32(asm_str_imm(Reg(8), Reg(9), u32(wi)))
					}
				}
				if g.stack_size > 0 {
					g.emit_add_sp(g.stack_size)
				}
				g.emit32(asm_ldp_fp_lr_post())
				g.emit32(asm_ret())
				return
			}
			// Float returns go in d0 (s0 for f32). Load the return value as a
			// double — load_float_operand widens an f32 source automatically, so
			// an implicit f32->f64 widening on `return` is handled here — then
			// narrow to s0 when the function itself returns f32.
			if g.is_float_type(g.cur_func_ret_type) && instr.operands.len > 0
				&& instr.operands[0] > 0 {
				g.load_float_operand(instr.operands[0], 0)
				if g.is_f32_type(g.cur_func_ret_type) {
					g.emit32(asm_fcvt_s_d(0, 0))
				}
				if g.stack_size > 0 {
					g.emit_add_sp(g.stack_size)
				}
				g.emit32(asm_ldp_fp_lr_post())
				g.emit32(asm_ret())
				return
			}
			if instr.operands.len > 0 && instr.operands[0] > 0 {
				ret_id := instr.operands[0]
				ret_val := g.m.values[ret_id]
				if ret_val.kind == .string_literal {
					g.materialize_string(ret_id, 0)
					g.emit32(asm_mov_reg(Reg(1), Reg(10)))
				} else {
					ret_size := g.m.type_size(ret_val.typ)
					if ret_size > 8 && ret_val.typ > 0 && ret_val.typ < g.m.type_store.types.len
						&& g.m.type_store.types[ret_val.typ].kind == .struct_t {
						if off := g.stack_slot(ret_id) {
							if g.is_string_struct_type(ret_val.typ) {
								g.emit_load_string_regs_from_fp(off, 0, 1, ret_val.typ)
							} else {
								n_words := (ret_size + 7) / 8
								for wi in 0 .. n_words {
									if wi < 8 {
										g.emit_load_fp(wi, off + wi * 8)
									}
								}
							}
						}
					} else {
						src_reg := g.load_val(ret_id, 0)
						if src_reg != 0 {
							g.emit32(asm_mov_reg(Reg(0), Reg(src_reg)))
						}
					}
				}
			} else {
				g.emit_mov_imm(0, 0)
			}
			// Epilogue
			if g.stack_size > 0 {
				g.emit_add_sp(g.stack_size)
			}
			g.emit32(asm_ldp_fp_lr_post())
			g.emit32(asm_ret())
		}
		.br {
			if instr.operands.len < 3 {
				return
			}
			cond_reg := g.load_val(instr.operands[0], 8)
			then_blk := int(instr.operands[1])
			else_blk := int(instr.operands[2])

			g.emit32(asm_cbnz(Reg(cond_reg), 2))
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
			g.emit32(asm_udf())
		}
		.phi {}
		.assign {
			// Phi-elimination copy: assign dest, src  ->  dest_slot = src.
			if instr.operands.len >= 2 {
				g.emit_phi_copy_value(instr.operands[1], instr.operands[0])
			}
		}
		.struct_init {}
		else {}
	}
}

// gen_call emits call output for arm64.
fn (mut g Gen) gen_call(val_id int, instr ssa.Instruction) {
	if instr.operands.len < 1 {
		return
	}
	fn_ref_id := instr.operands[0]
	is_indirect := instr.op == .call_indirect
	fn_ref := g.m.values[fn_ref_id]
	mut fn_name := ''
	if !is_indirect {
		fn_name = fn_ref.name
	}
	ret_indirect := g.is_large_struct_type(instr.typ)

	out_stack_size := g.call_stack_arg_size(instr)
	if out_stack_size > 0 {
		g.emit_sub_sp(out_stack_size)
	}

	mut arg_reg := 0
	mut float_reg := 0
	mut stack_off := 0
	for ai in 1 .. instr.operands.len {
		arg_id := instr.operands[ai]
		arg_val := g.m.values[arg_id]

		if arg_val.kind == .string_literal {
			if arg_reg + 2 <= 8 {
				g.materialize_string(arg_id, arg_reg)
				g.emit32(asm_mov_reg(Reg(arg_reg + 1), Reg(10)))
				arg_reg += 2
			} else {
				g.materialize_string(arg_id, 8)
				g.emit_store_sp(8, stack_off)
				g.emit_store_sp(10, stack_off + 8)
				stack_off += 16
			}
		} else if g.is_float_type(arg_val.typ) {
			// Float args go in d0-d7 (separate from x0-x7). Move the bit pattern
			// into an x register then fmov into the float arg register.
			if float_reg < 8 {
				g.load_float_bits_to_reg(arg_id, 9)
				g.emit32(asm_fmov_d_x(float_reg, Reg(9)))
				float_reg++
			} else {
				g.load_float_bits_to_reg(arg_id, 8)
				g.emit_store_sp(8, stack_off)
				stack_off += 8
			}
		} else {
			arg_type_id := arg_val.typ
			arg_size := g.m.type_size(arg_type_id)
			if arg_size > 8 && arg_type_id > 0 && arg_type_id < g.m.type_store.types.len {
				typ := g.m.type_store.types[arg_type_id]
				if typ.kind == .struct_t {
					if g.is_large_struct_type(arg_type_id) {
						if arg_reg < 8 {
							if !g.emit_value_address(arg_id, arg_reg) {
								g.emit_mov_imm(arg_reg, 0)
							}
							arg_reg += 1
						} else {
							if !g.emit_value_address(arg_id, 8) {
								g.emit_mov_imm(8, 0)
							}
							g.emit_store_sp(8, stack_off)
							stack_off += 8
						}
						continue
					}
					if g.is_string_struct_type(arg_type_id) {
						if arg_reg + 2 <= 8 {
							if off := g.stack_slot(arg_id) {
								g.emit_load_string_regs_from_fp(off, arg_reg, arg_reg + 1,
									arg_type_id)
							} else {
								src_reg := g.load_val(arg_id, arg_reg)
								if src_reg != arg_reg {
									g.emit32(asm_mov_reg(Reg(arg_reg), Reg(src_reg)))
								}
								g.emit_mov_imm(arg_reg + 1, 0)
							}
							arg_reg += 2
						} else {
							if off := g.stack_slot(arg_id) {
								g.emit_load_string_regs_from_fp(off, 8, 10, arg_type_id)
								g.emit_store_sp(8, stack_off)
								g.emit_store_sp(10, stack_off + 8)
							} else {
								src_reg := g.load_val(arg_id, 8)
								g.emit_store_sp(src_reg, stack_off)
								g.emit_mov_imm(10, 0)
								g.emit_store_sp(10, stack_off + 8)
							}
							stack_off += 16
						}
						continue
					}
					n_words := (arg_size + 7) / 8
					if arg_reg + n_words <= 8 {
						if off := g.stack_slot(arg_id) {
							for wi in 0 .. n_words {
								g.emit_load_fp(arg_reg + wi, off + wi * 8)
							}
						} else {
							src_reg := g.load_val(arg_id, arg_reg)
							if src_reg != arg_reg {
								g.emit32(asm_mov_reg(Reg(arg_reg), Reg(src_reg)))
							}
						}
						arg_reg += n_words
					} else {
						if off := g.stack_slot(arg_id) {
							for wi in 0 .. n_words {
								g.emit_load_fp(8, off + wi * 8)
								g.emit_store_sp(8, stack_off + wi * 8)
							}
						} else {
							src_reg := g.load_val(arg_id, 8)
							g.emit_store_sp(src_reg, stack_off)
						}
						stack_off += n_words * 8
					}
					continue
				}
			}

			if arg_val.kind == .instruction {
				arg_instr := g.m.instrs[arg_val.index]
				if arg_instr.op == .alloca {
					if alloca_off := g.alloca_slot(arg_id) {
						if arg_reg < 8 {
							g.emit_lea_fp(arg_reg, alloca_off)
							arg_reg += 1
						} else {
							g.emit_lea_fp(8, alloca_off)
							g.emit_store_sp(8, stack_off)
							stack_off += 8
						}
						continue
					}
				}
			}

			if arg_reg < 8 {
				src_reg := g.load_val(arg_id, arg_reg)
				if src_reg != arg_reg {
					g.emit32(asm_mov_reg(Reg(arg_reg), Reg(src_reg)))
				}
				arg_reg += 1
			} else {
				src_reg := g.load_val(arg_id, 8)
				g.emit_store_sp(src_reg, stack_off)
				stack_off += 8
			}
		}
	}

	if ret_indirect {
		if off := g.stack_slot(val_id) {
			g.emit_lea_fp(8, off)
		}
	}

	is_c_extern := fn_ref.kind == .func_ref && fn_ref.index >= 0 && fn_ref.index < g.m.funcs.len
		&& g.m.funcs[fn_ref.index].is_c_extern
	if is_indirect {
		target_reg := g.load_val(fn_ref_id, 16)
		g.emit32(asm_blr(Reg(target_reg)))
	} else if !is_c_extern && fn_name in g.fn_offsets {
		target := g.fn_offsets[fn_name]
		offset := (target - g.macho.text_data.len) / 4
		g.emit32(asm_bl(i32(offset)))
	} else {
		sym_idx := g.macho.add_undefined('_' + fn_name)
		g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_branch26, true)
		g.emit32(asm_bl(0))
	}

	if out_stack_size > 0 {
		g.emit_add_sp(out_stack_size)
	}

	if ret_indirect {
		return
	}

	if instr.typ != 0 {
		ret_size := g.m.type_size(instr.typ)
		if ret_size > 8 && instr.typ > 0 && instr.typ < g.m.type_store.types.len {
			typ := g.m.type_store.types[instr.typ]
			if typ.kind == .struct_t {
				if off := g.stack_slot(val_id) {
					if g.is_string_struct_type(instr.typ) {
						g.emit_store_fp(0, off)
						g.emit_store_fp(1, off + 8)
						return
					}
					n_words := (ret_size + 7) / 8
					for wi in 0 .. n_words {
						if wi < 8 {
							g.emit_store_fp(wi, off + wi * 8)
						}
					}
				}
				return
			}
		}
		// Float results are returned in d0: move the bit pattern into x0.
		if g.is_float_type(instr.typ) || g.call_returns_float(fn_ref) {
			g.emit32(asm_fmov_x_d(Reg(0), 0))
		}
		g.store_val(0, val_id)
	}
}

// call_returns_float updates call returns float state for Gen.
fn (g &Gen) call_returns_float(fn_ref ssa.Value) bool {
	if fn_ref.kind == .func_ref && fn_ref.index >= 0 && fn_ref.index < g.m.funcs.len {
		return g.is_float_type(g.m.funcs[fn_ref.index].typ)
	}
	return false
}

// load_float_bits_to_reg loads the raw IEEE-754 bit pattern of a float value
// into integer register `reg` (handling float constants, which `load_val`
// cannot parse).
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

// call_stack_arg_size updates call stack arg size state for Gen.
fn (g &Gen) call_stack_arg_size(instr ssa.Instruction) int {
	mut arg_reg := 0
	mut float_reg := 0
	mut stack_words := 0
	for ai in 1 .. instr.operands.len {
		arg_id := instr.operands[ai]
		if arg_id <= 0 || arg_id >= g.m.values.len {
			continue
		}
		arg_val := g.m.values[arg_id]
		if g.is_float_type(arg_val.typ) {
			if float_reg < 8 {
				float_reg++
			} else {
				stack_words += 1
			}
			continue
		}
		mut n_words := 1
		if arg_val.kind == .string_literal {
			n_words = 2
		} else {
			arg_size := g.m.type_size(arg_val.typ)
			if arg_size > 8 && arg_val.typ > 0 && arg_val.typ < g.m.type_store.types.len
				&& g.m.type_store.types[arg_val.typ].kind == .struct_t {
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

// emit_value_address emits emit value address output for arm64.
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
				if off := g.alloca_slot(val_id) {
					g.emit_lea_fp(reg, off)
					return true
				}
			}
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

// emit_copy_ptr_to_fp converts emit copy ptr to fp data for arm64.
fn (mut g Gen) emit_copy_ptr_to_fp(src_ptr_reg int, dst_off int, size int) {
	n_words := (size + 7) / 8
	tmp_reg := if src_ptr_reg == 8 { 10 } else { 8 }
	for wi in 0 .. n_words {
		g.emit32(asm_ldr_imm(Reg(tmp_reg), Reg(src_ptr_reg), u32(wi)))
		g.emit_store_fp(tmp_reg, dst_off + wi * 8)
	}
}

// ==================== Value loading/storing ====================

// load_val reads load val input for arm64.
fn (mut g Gen) load_val(val_id int, reg int) int {
	if val_id <= 0 || val_id >= g.m.values.len {
		g.emit_mov_imm(reg, 0)
		return reg
	}
	val := g.m.values[val_id]
	match val.kind {
		.constant {
			// A float constant's register value is its IEEE-754 bit pattern, not
			// the integer parse of its textual name.
			if g.is_float_type(val.typ) {
				g.load_float_bits_to_reg(val_id, reg)
				return reg
			}
			n := parse_arm64_int(val.name)
			g.emit_mov_imm(reg, n)
			return reg
		}
		.string_literal {
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
				if off := g.alloca_slot(val_id) {
					g.emit_lea_fp(reg, off)
					return reg
				}
			}
			if off := g.stack_slot(val_id) {
				g.emit_load_fp(reg, off)
				return reg
			}
			g.emit_mov_imm(reg, 0)
			return reg
		}
		.argument {
			if off := g.stack_slot(val_id) {
				g.emit_load_fp(reg, off)
				return reg
			}
			g.emit_mov_imm(reg, 0)
			return reg
		}
		else {
			g.emit_mov_imm(reg, 0)
			return reg
		}
	}
}

// store_val supports store val handling for Gen.
fn (mut g Gen) store_val(reg int, val_id int) {
	if off := g.stack_slot(val_id) {
		g.emit_store_fp(reg, off)
	}
}

// ==================== String materialization ====================

// materialize_string supports materialize string handling for Gen.
fn (mut g Gen) materialize_string(val_id int, reg int) {
	val := g.m.values[val_id]
	str_content := val.name
	str_len := str_content.len

	mut str_offset := 0
	if cached := g.string_cache[str_content] {
		str_offset = cached
	} else {
		str_offset = g.macho.str_data.len
		g.string_cache[str_content] = str_offset
		g.macho.str_data << str_content.bytes()
		g.macho.str_data << 0
	}

	str_sym_name := 'L_str_${str_offset}'
	str_sym_idx := g.macho.add_symbol(str_sym_name, u64(str_offset), false, 2)

	// ADRP + ADD to load address
	g.macho.add_reloc(g.macho.text_data.len, str_sym_idx, arm64_reloc_page21, true)
	g.emit32(asm_adrp(Reg(reg)))
	g.macho.add_reloc(g.macho.text_data.len, str_sym_idx, arm64_reloc_pageoff12, false)
	g.emit32(asm_add_pageoff(Reg(reg)))

	// x10 holds the string struct's second 8-byte word: `len` in the low 32 bits and
	// `is_lit` in the high 32 bits (matching `string{ str, len int, is_lit int }`).
	// Every caller stores/moves x10 as that whole word, so set is_lit=1 here: string
	// literals point at static data and must NOT be passed to free() (`string.free`
	// returns early only when is_lit==1). Without this, freeing a literal (e.g. the
	// `mut res := ''` in os.real_path) aborts with a libmalloc "pointer not allocated".
	g.emit_mov_imm(10, i64(str_len) | i64(u64(1) << 32))
}

// ==================== Global access ====================

// emit_global_addr emits emit global addr output for arm64.
fn (mut g Gen) emit_global_addr(reg int, name string) {
	sym_name := '_' + name
	mut sym_idx := 0
	if existing := g.macho.sym_by_name[sym_name] {
		sym_idx = existing
	} else {
		sym_idx = g.macho.add_undefined(sym_name)
	}
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_page21, true)
	g.emit32(asm_adrp(Reg(reg)))
	g.macho.add_reloc(g.macho.text_data.len, sym_idx, arm64_reloc_pageoff12, false)
	g.emit32(asm_add_pageoff(Reg(reg)))
}

// find_global_idx_by_name resolves find global idx by name information for arm64.
fn (g &Gen) find_global_idx_by_name(name string) int {
	for i, global in g.m.globals {
		if global.name == name {
			return i
		}
	}
	return -1
}

// store_entry_arg_to_global converts store entry arg to global data for arm64.
fn (mut g Gen) store_entry_arg_to_global(reg int, global_name string) {
	global_idx := g.find_global_idx_by_name(global_name)
	if global_idx < 0 {
		return
	}
	g.emit_global_addr(9, global_name)
	g.emit_store_typed(reg, 9, g.m.globals[global_idx].typ)
}

// ==================== Branch handling ====================

// emit_branch_to_block converts emit branch to block data for arm64.
fn (mut g Gen) emit_branch_to_block(blk_id int) {
	if blk_id >= 0 && blk_id < g.block_offsets.len && g.block_offsets[blk_id] >= 0 {
		target := g.block_offsets[blk_id]
		offset := (target - g.macho.text_data.len) / 4
		g.emit32(asm_b(i32(offset)))
	} else {
		g.pending_jmps << PendingJmp{
			text_pos: g.macho.text_data.len
			block_id: blk_id
		}
		g.emit32(asm_b(0))
	}
}

// emit_phi_edge_copies emits emit phi edge copies output for arm64.
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

// emit_phi_copy_value emits emit phi copy value output for arm64.
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
			g.materialize_string(src_id, 8)
			g.emit_store_fp(8, dst_off)
			if n_words > 1 {
				g.emit_store_fp(10, dst_off + 8)
			}
			return
		}
		if src_off := g.stack_slot(src_id) {
			for wi in 0 .. n_words {
				g.emit_load_fp(8, src_off + wi * 8)
				g.emit_store_fp(8, dst_off + wi * 8)
			}
			return
		}
		src_reg := g.load_val(src_id, 8)
		if src_reg != 8 {
			g.emit32(asm_mov_reg(Reg(8), Reg(src_reg)))
		}
		g.emit_store_fp(8, dst_off)
		g.emit_mov_imm(8, 0)
		for wi in 1 .. n_words {
			g.emit_store_fp(8, dst_off + wi * 8)
		}
		return
	}
	src_reg := g.load_val(src_id, 8)
	if src_reg != 8 {
		g.emit32(asm_mov_reg(Reg(8), Reg(src_reg)))
	}
	g.store_val(8, dst_id)
}

// resolve_pending_jmps resolves resolve pending jmps information for arm64.
fn (mut g Gen) resolve_pending_jmps(blk_id int) {
	target := g.macho.text_data.len
	mut remaining := []PendingJmp{}
	for pj in g.pending_jmps {
		if pj.block_id == blk_id {
			offset := (target - pj.text_pos) / 4
			g.patch_branch(pj.text_pos, offset)
		} else {
			remaining << pj
		}
	}
	g.pending_jmps = remaining
}

// resolve_all_pending resolves resolve all pending information for arm64.
fn (mut g Gen) resolve_all_pending() {
	for pj in g.pending_jmps {
		if pj.block_id >= 0 && pj.block_id < g.block_offsets.len
			&& g.block_offsets[pj.block_id] >= 0 {
			offset := (g.block_offsets[pj.block_id] - pj.text_pos) / 4
			g.patch_branch(pj.text_pos, offset)
		}
	}
	g.pending_jmps.clear()
}

// patch_branch supports patch branch handling for Gen.
fn (mut g Gen) patch_branch(text_pos int, offset int) {
	existing := read_u32_le(g.macho.text_data, text_pos)
	opcode := existing & 0xFC000000
	imm26 := u32(offset) & 0x03FFFFFF
	patched := opcode | imm26
	write_u32_le_at_arr(mut g.macho.text_data, text_pos, patched)
}

// ==================== Low-level emission helpers ====================

// emit32 supports emit32 handling for Gen.
fn (mut g Gen) emit32(instr u32) {
	g.macho.text_data << u8(instr)
	g.macho.text_data << u8(instr >> 8)
	g.macho.text_data << u8(instr >> 16)
	g.macho.text_data << u8(instr >> 24)
}

// emit_mov_imm emits emit mov imm output for arm64.
fn (mut g Gen) emit_mov_imm(reg int, val i64) {
	if val >= 0 && val < 65536 {
		g.emit32(asm_movz(Reg(reg), u32(val)))
	} else if val >= 0 && val < i64(0xFFFFFFFF) {
		lo := u32(val) & 0xFFFF
		hi := (u32(val) >> 16) & 0xFFFF
		g.emit32(asm_movz(Reg(reg), lo))
		if hi != 0 {
			g.emit32(asm_movk(Reg(reg), hi, 1))
		}
	} else if val < 0 && val >= -65536 {
		g.emit32(asm_movn(Reg(reg), u32(~val)))
	} else {
		uval := u64(val)
		g.emit32(asm_movz(Reg(reg), u32(uval & 0xFFFF)))
		if (uval >> 16) & 0xFFFF != 0 {
			g.emit32(asm_movk(Reg(reg), u32((uval >> 16) & 0xFFFF), 1))
		}
		if (uval >> 32) & 0xFFFF != 0 {
			g.emit32(asm_movk(Reg(reg), u32((uval >> 32) & 0xFFFF), 2))
		}
		if (uval >> 48) & 0xFFFF != 0 {
			g.emit32(asm_movk(Reg(reg), u32((uval >> 48) & 0xFFFF), 3))
		}
	}
}

// emit_store_fp emits emit store fp output for arm64.
fn (mut g Gen) emit_store_fp(reg int, offset int) {
	if offset >= -255 && offset < 0 {
		g.emit32(asm_stur(Reg(reg), fp, i32(offset)))
	} else if offset < -255 {
		g.emit_mov_imm(11, i64(offset))
		g.emit32(asm_add_reg(Reg(11), fp, Reg(11)))
		g.emit32(asm_str(Reg(reg), Reg(11)))
	} else {
		g.emit32(asm_str_imm(Reg(reg), fp, u32(offset / 8)))
	}
}

// emit_store_sp emits emit store sp output for arm64.
fn (mut g Gen) emit_store_sp(reg int, offset int) {
	if offset >= 0 && offset < 32768 && offset % 8 == 0 {
		g.emit32(asm_str_imm(Reg(reg), sp, u32(offset / 8)))
	} else {
		g.emit_mov_imm(11, i64(offset))
		g.emit32(asm_add_reg(Reg(11), sp, Reg(11)))
		g.emit32(asm_str(Reg(reg), Reg(11)))
	}
}

// emit_load_fp emits emit load fp output for arm64.
fn (mut g Gen) emit_load_fp(reg int, offset int) {
	if offset >= -255 && offset < 0 {
		g.emit32(asm_ldur(Reg(reg), fp, i32(offset)))
	} else if offset < -255 {
		g.emit_mov_imm(11, i64(offset))
		g.emit32(asm_add_reg(Reg(11), fp, Reg(11)))
		g.emit32(asm_ldr(Reg(reg), Reg(11)))
	} else {
		g.emit32(asm_ldr_imm(Reg(reg), fp, u32(offset / 8)))
	}
}

// emit_lea_fp emits emit lea fp output for arm64.
fn (mut g Gen) emit_lea_fp(reg int, offset int) {
	if offset >= 0 && offset < 4096 {
		g.emit32(asm_add_imm(Reg(reg), fp, u32(offset)))
	} else if offset < 0 && -offset < 4096 {
		g.emit32(asm_sub_imm(Reg(reg), fp, u32(-offset)))
	} else {
		g.emit_mov_imm(reg, i64(offset))
		g.emit32(asm_add_reg(Reg(reg), fp, Reg(reg)))
	}
}

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

// is_string_struct_type reports whether is string struct type applies in arm64.
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

// emit_load_string_regs_from_ptr converts emit load string regs from ptr data for arm64.
fn (mut g Gen) emit_load_string_regs_from_ptr(ptr_reg int, data_reg int, len_reg int, typ_id ssa.TypeID) {
	typ := g.m.type_store.types[typ_id]
	g.emit_load_typed(data_reg, ptr_reg, typ.fields[0])
	g.emit32(asm_add_imm(Reg(11), Reg(ptr_reg), 8))
	if typ.fields.len == 3 {
		g.emit32(asm_ldr(Reg(len_reg), Reg(11)))
	} else {
		g.emit_load_typed(len_reg, 11, typ.fields[1])
	}
}

// emit_load_string_regs_from_fp converts emit load string regs from fp data for arm64.
fn (mut g Gen) emit_load_string_regs_from_fp(off int, data_reg int, len_reg int, typ_id ssa.TypeID) {
	typ := g.m.type_store.types[typ_id]
	g.emit_load_fp(data_reg, off)
	if typ.fields.len == 3 {
		g.emit_load_fp(len_reg, off + 8)
	} else {
		g.emit_lea_fp(11, off + 8)
		g.emit_load_typed(len_reg, 11, typ.fields[1])
	}
}

// emit_store_typed emits emit store typed output for arm64.
fn (mut g Gen) emit_store_typed(src_reg int, ptr_reg int, typ ssa.TypeID) {
	size := g.m.type_size(typ)
	match size {
		1 { g.emit32(asm_str_b(Reg(src_reg), Reg(ptr_reg))) }
		2 { g.emit32(asm_str_h(Reg(src_reg), Reg(ptr_reg))) }
		4 { g.emit32(asm_str_w(Reg(src_reg), Reg(ptr_reg))) }
		else { g.emit32(asm_str(Reg(src_reg), Reg(ptr_reg))) }
	}
}

// emit_load_typed emits emit load typed output for arm64.
fn (mut g Gen) emit_load_typed(dst_reg int, ptr_reg int, typ ssa.TypeID) {
	size := g.m.type_size(typ)
	match size {
		1 {
			g.emit32(asm_ldr_b(Reg(dst_reg), Reg(ptr_reg)))
			if g.is_signed_int_type(typ) {
				g.emit32(asm_sxtb(Reg(dst_reg), Reg(dst_reg)))
			}
		}
		2 {
			g.emit32(asm_ldr_h(Reg(dst_reg), Reg(ptr_reg)))
			if g.is_signed_int_type(typ) {
				g.emit32(asm_sxth(Reg(dst_reg), Reg(dst_reg)))
			}
		}
		4 {
			if g.is_signed_int_type(typ) {
				g.emit32(asm_ldrsw(Reg(dst_reg), Reg(ptr_reg)))
			} else {
				g.emit32(asm_ldr_w(Reg(dst_reg), Reg(ptr_reg)))
			}
		}
		else {
			g.emit32(asm_ldr(Reg(dst_reg), Reg(ptr_reg)))
		}
	}
}

// is_signed_int_type reports whether is signed int type applies in arm64.
fn (g &Gen) is_signed_int_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .int_t && !typ.is_unsigned
}

// is_float_type reports whether is float type applies in arm64.
fn (g &Gen) is_float_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	return g.m.type_store.types[typ_id].kind == .float_t
}

// is_f32_type reports whether is f32 type applies in arm64.
fn (g &Gen) is_f32_type(typ_id ssa.TypeID) bool {
	if typ_id <= 0 || typ_id >= g.m.type_store.types.len {
		return false
	}
	typ := g.m.type_store.types[typ_id]
	return typ.kind == .float_t && typ.width == 32
}

// ==================== Floating point ====================
//
// Float values live as their raw IEEE-754 bit pattern in ordinary integer
// stack slots / x-registers, exactly like the v2 backend. They are only moved
// into the scalar SIMD `d` registers transiently for arithmetic, comparison and
// conversion. An f32 is stored as its 32-bit pattern; it is widened to f64 in
// the `d` register for computation and narrowed back before storing.

// load_float_operand materializes value `val_id` into the scalar float register
// `dreg` (as a double), using x8 as a scratch integer register.
fn (mut g Gen) load_float_operand(val_id int, dreg int) {
	if val_id <= 0 || val_id >= g.m.values.len {
		g.emit_mov_imm(8, 0)
		g.emit32(asm_fmov_d_x(dreg, Reg(8)))
		return
	}
	val := g.m.values[val_id]
	is_f32 := g.is_f32_type(val.typ)
	if val.kind == .constant {
		if is_f32 {
			f_val := f32(val.name.f64())
			bits := *unsafe { &u32(&f_val) }
			g.emit_mov_imm(8, i64(bits))
			g.emit32(asm_fmov_s_w(dreg, Reg(8)))
			g.emit32(asm_fcvt_d_s(dreg, dreg))
		} else {
			f_val := val.name.f64()
			bits := *unsafe { &u64(&f_val) }
			g.emit_mov_imm(8, i64(bits))
			g.emit32(asm_fmov_d_x(dreg, Reg(8)))
		}
	} else {
		reg := g.load_val(val_id, 8)
		if is_f32 {
			g.emit32(asm_fmov_s_w(dreg, Reg(reg)))
			g.emit32(asm_fcvt_d_s(dreg, dreg))
		} else {
			g.emit32(asm_fmov_d_x(dreg, Reg(reg)))
		}
	}
}

// store_float_result writes the double in d0 back to `val_id`'s slot as the
// appropriate bit pattern (narrowing to f32 first when the result is an f32).
fn (mut g Gen) store_float_result(val_id int) {
	if g.is_f32_type(g.m.values[val_id].typ) {
		g.emit32(asm_fcvt_s_d(0, 0))
		g.emit32(asm_fmov_w_s(Reg(8), 0))
	} else {
		g.emit32(asm_fmov_x_d(Reg(8), 0))
	}
	g.store_val(8, val_id)
}

// gen_float_binop emits a binary float op with both operands loaded into d0/d1
// and the result left in d0, then stored to `val_id`.
fn (mut g Gen) gen_float_binop(fop ssa.OpCode, lhs_id int, rhs_id int, val_id int) {
	g.load_float_operand(lhs_id, 0) // d0
	g.load_float_operand(rhs_id, 1) // d1
	match fop {
		.fadd {
			g.emit32(asm_fadd_d0_d0_d1())
		}
		.fsub {
			g.emit32(asm_fsub_d0_d0_d1())
		}
		.fmul {
			g.emit32(asm_fmul_d0_d0_d1())
		}
		.fdiv {
			g.emit32(asm_fdiv_d0_d0_d1())
		}
		.frem {
			// No single frem instruction: d0 = d0 - trunc(d0/d1) * d1
			g.emit32(asm_fdiv_d2_d0_d1())
			g.emit32(asm_frintz_d2())
			g.emit32(asm_fmsub_d0_d2_d1_d0())
		}
		else {}
	}

	g.store_float_result(val_id)
}

// emit_sub_sp emits emit sub sp output for arm64.
fn (mut g Gen) emit_sub_sp(size int) {
	if size > 0 && size < 4096 {
		g.emit32(asm_sub_imm(sp, sp, u32(size)))
	} else if size >= 4096 {
		g.emit_mov_imm(11, i64(size))
		g.emit32(asm_sub_sp_reg(Reg(11)))
	}
}

// emit_add_sp emits emit add sp output for arm64.
fn (mut g Gen) emit_add_sp(size int) {
	if size > 0 && size < 4096 {
		g.emit32(asm_add_imm(sp, sp, u32(size)))
	} else if size >= 4096 {
		g.emit_mov_imm(11, i64(size))
		g.emit32(asm_add_sp_reg(Reg(11)))
	}
}

// parse_arm64_int reads parse arm64 int input for arm64.
fn parse_arm64_int(s string) i64 {
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
