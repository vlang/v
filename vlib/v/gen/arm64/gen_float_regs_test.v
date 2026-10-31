module arm64

import v.ssa

// The words below are counted, not just searched: the tests assert how long the
// sequence for one instruction is, because "the instruction names the registers
// it reads" is only worth writing down if the surrounding loads and stores got
// shorter.

// binop_words computes `fop(lhs, rhs)` where `lhs` and `rhs` are two earlier
// instructions in the same block that already hold floats in their own slots,
// and returns the words that one instruction produced. The frame is 32 bytes
// and every value sits at its own negative slot, which is the addressing a real
// function reaches: the operand load is one instruction from the slot rather
// than a load plus an `fmov`.
fn binop_words(mut m ssa.Module, fop ssa.OpCode, width int, same_operand bool) []u32 {
	return float_instr_words(mut m, fop, width, same_operand, false)
}

// compare_words is binop_words for a float comparison, whose result is a bool
// rather than a float.
fn compare_words(mut m ssa.Module, fop ssa.OpCode, width int, same_operand bool) []u32 {
	return float_instr_words(mut m, fop, width, same_operand, true)
}

fn float_instr_words(mut m ssa.Module, fop ssa.OpCode, width int, same_operand bool, is_compare bool) []u32 {
	arg_type := m.type_store.get_float(width)
	ret_type := if is_compare { m.type_store.get_int(1) } else { arg_type }
	func_id := m.new_function('caller', ret_type)
	block_id := m.add_block(func_id, 'entry')
	one := m.add_value(.constant, arg_type, '1', 0)
	two := m.add_value(.constant, arg_type, '2', 0)
	left := m.add_instr(.fadd, block_id, arg_type, [one, two])
	right := m.add_instr(.fmul, block_id, arg_type, [one, two])
	operands := if same_operand {
		[ssa.ValueID(left), ssa.ValueID(left)]
	} else {
		[ssa.ValueID(left), ssa.ValueID(right)]
	}
	result := m.add_instr(fop, block_id, ret_type, operands)
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[func_id])
	// A 32-byte frame puts both operands and the result in the sp-relative
	// scaled range, exactly as gen_func does for a real function.
	g.stack_size = 32
	g.set_stack_slot(left, -8)
	g.set_stack_slot(right, -16)
	g.set_stack_slot(result, -24)
	g.gen_instr(int(result))
	g.materialize_text()
	return collected_float_words(g)
}

fn collected_float_words(g &Gen) []u32 {
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	return words
}

// Two float constants are the shape where a value cannot be loaded from a slot:
// the constant's bits are materialized into x8 either way, so the only saving
// left is the store of the result.
fn const_binop_words(mut m ssa.Module, fop ssa.OpCode, width int) []u32 {
	arg_type := m.type_store.get_float(width)
	func_id := m.new_function('caller', arg_type)
	block_id := m.add_block(func_id, 'entry')
	lhs := m.add_value(.constant, arg_type, '1', 0)
	rhs := m.add_value(.constant, arg_type, '2', 0)
	result := m.add_instr(fop, block_id, arg_type, [lhs, rhs])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[func_id])
	g.stack_size = 16
	g.set_stack_slot(result, -8)
	g.gen_instr(int(result))
	g.materialize_text()
	return collected_float_words(g)
}

fn test_f64_add_loads_both_operands_into_the_registers_it_names() {
	mut m := ssa.Module.new()
	words := binop_words(mut m, .fadd, 64, false)
	// Measured: 7 words before this change, 4 after. One load per operand,
	// straight into the register the fadd reads, and one store of the result:
	// nothing is copied through x8 on the way in or out.
	assert asm_ldr_d_imm(0, sp, 3) in words
	assert asm_ldr_d_imm(1, sp, 2) in words
	assert asm_fadd_d(0, 0, 1) in words
	assert asm_str_d_imm(0, sp, 1) in words
	assert asm_fmov_d_x(0, Reg(8)) !in words
	assert asm_fmov_x_d(Reg(8), 0) !in words
	assert words.len == 4
}

fn test_same_value_as_both_operands_is_loaded_once() {
	mut m := ssa.Module.new()
	words := binop_words(mut m, .fsub, 64, true)
	// Measured: 7 words before, 3 after. `x - x` reads its operand twice, so the
	// three-register form does the reading: one load, then fsub d0, d0, d0.
	assert asm_ldr_d_imm(0, sp, 3) in words
	assert asm_ldr_d_imm(1, sp, 2) !in words
	assert asm_fsub_d(0, 0, 0) in words
	assert asm_str_d_imm(0, sp, 1) in words
	assert words.len == 3
}

fn test_f32_add_of_the_same_shape_stays_single_precision() {
	mut m := ssa.Module.new()
	words := binop_words(mut m, .fadd, 32, false)
	// Measured: 7 words before this change, 5 after. The same shape at f32
	// width: the loads name s0 and s1, the arithmetic is the single-precision
	// one, and no widening conversion appears at all.
	assert asm_ldr_s_imm(0, sp, 6) in words
	assert asm_ldr_s_imm(1, sp, 4) in words
	assert asm_fadd_s(0, 0, 1) in words
	assert asm_fmov_s_w(0, Reg(8)) !in words
	assert asm_fcvt_d_s(0, 0) !in words
	// The result store still goes through w8: it writes the whole 8-byte slot,
	// which the readers of that slot move as one word.
	assert asm_fmov_w_s(Reg(8), 0) in words
	assert words.len == 5
}

fn test_f64_add_of_two_constants_is_one_word_shorter() {
	mut m := ssa.Module.new()
	words := const_binop_words(mut m, .fadd, 64)
	// Measured: 9 words before this change, 8 after. Both constants still cost
	// a movz, a movk and an `fmov` each, because a 64-bit bit pattern has no
	// shorter route into a `d` register — what is gone is the `fmov` that copied
	// the result back out through x8, so the result is stored from d0 itself.
	assert asm_fadd_d(0, 0, 1) in words
	assert asm_str_d_imm(0, sp, 1) in words
	assert asm_fmov_x_d(Reg(8), 0) !in words
	assert words.len == 8
}

fn test_f32_add_of_two_constants_keeps_its_word_count() {
	mut m := ssa.Module.new()
	words := const_binop_words(mut m, .fadd, 32)
	// Measured: 9 words before this change and 9 after. f32 constants are one
	// movz/movk pair each rather than the double's three words, and the store is
	// unchanged, so this shape is neither shorter nor longer.
	assert asm_fadd_s(0, 0, 1) in words
	assert asm_fcvt_s_d(0, 0) !in words
	assert words.len == 9
}

fn test_f64_comparison_loads_the_registers_fcmp_names() {
	mut m := ssa.Module.new()
	words := compare_words(mut m, .lt, 64, false)
	// Measured: 7 words before this change, 5 after.
	assert asm_ldr_d_imm(0, sp, 3) in words
	assert asm_ldr_d_imm(1, sp, 2) in words
	assert asm_fcmp_d(Reg(0), Reg(1)) in words
	assert words.len == 5
}

fn test_f64_comparison_of_a_value_with_itself_loads_once() {
	mut m := ssa.Module.new()
	words := compare_words(mut m, .eq, 64, true)
	// Measured: 7 words before this change, 4 after.
	assert asm_ldr_d_imm(0, sp, 3) in words
	assert asm_ldr_d_imm(1, sp, 2) !in words
	assert asm_fcmp_d(Reg(0), Reg(0)) in words
	assert words.len == 4
}

fn test_f32_comparison_compares_at_the_operands_width() {
	mut m := ssa.Module.new()
	words := compare_words(mut m, .le, 32, false)
	// Measured: 7 words before this change, 5 after. The s registers the fcmp
	// names are filled by one load each, and the comparison stays single
	// precision even though the result is a bool.
	assert asm_ldr_s_imm(0, sp, 6) in words
	assert asm_fcmp_s(Reg(0), Reg(1)) in words
	assert asm_fcmp_d(Reg(0), Reg(1)) !in words
	assert words.len == 5
}

fn test_f32_slot_load_preserves_offsets_across_scaled_immediate_boundary() {
	// Frames are 16-byte aligned, and every local occupies an eight-byte slot.
	// The scalar f32 load has only 12 offset bits scaled by four (16 KiB).
	for offset in [16376, 16384, 16392, 32760, 32768] {
		mut m := ssa.Module.new()
		float_type := m.type_store.get_float(32)
		func_id := m.new_function('caller', float_type)
		block_id := m.add_block(func_id, 'entry')
		value := m.add_instr(.fadd, block_id, float_type, [])
		mut g := Gen.new(m)
		g.reset_value_slots(&m.funcs[func_id])
		// 16400 - 16 = 16384 exercises the first unencodable direct offset.
		g.stack_size = (offset + 31) / 16 * 16
		g.set_stack_slot(value, offset - g.stack_size)
		assert g.load_s_from_slot(value, 3)
		g.materialize_text()
		words := collected_float_words(g)
		if offset < 16384 {
			assert words == [asm_ldr_s_imm(3, sp, u32(offset / 4))]
		} else {
			assert words == [asm_add_imm_lsl12(Reg(11), sp, u32(offset >> 12)),
				asm_ldr_s_imm(3, Reg(11), u32((offset & 0xFFF) / 4))]
		}
	}
}

fn test_f32_slot_load_counts_transient_stack_at_scaled_immediate_boundary() {
	mut m := ssa.Module.new()
	float_type := m.type_store.get_float(32)
	func_id := m.new_function('caller', float_type)
	block_id := m.add_block(func_id, 'entry')
	value := m.add_instr(.fadd, block_id, float_type, [])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[func_id])
	g.stack_size = 16384
	g.transient_stack_size = 16
	g.set_stack_slot(value, -16)
	assert g.load_s_from_slot(value, 3)
	g.materialize_text()
	assert collected_float_words(g) == [asm_add_imm_lsl12(Reg(11), sp, 4),
		asm_ldr_s_imm(3, Reg(11), 0)]
}

fn test_f64_remainder_of_the_same_value_uses_its_loaded_register() {
	mut m := ssa.Module.new()
	words := binop_words(mut m, .frem, 64, true)
	assert asm_fdiv_d(2, 0, 0) in words
	// The general FMSUB encoder must not force multiplier d1: only d0 is loaded.
	assert u32(0x1F408040) in words
	assert u32(0x1F418040) !in words
}
