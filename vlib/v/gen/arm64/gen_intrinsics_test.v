module arm64

import v.ssa

// A four-lane f32 value in SSA shape: a struct whose only field is a fixed
// array of four f32, which is what `vlib/simd`'s F32x4 lowers to.
fn simd_vec4_type(mut m ssa.Module) ssa.TypeID {
	f32_type := m.type_store.get_float(32)
	lanes := m.type_store.get_array(f32_type, 4)
	return m.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [lanes]
	})
}

fn simd_call_words(mut m ssa.Module, callee string, operands []ssa.ValueID, ret_type ssa.TypeID) []u32 {
	callee_id := m.new_function(callee, ret_type)
	fn_ref := m.add_value(.func_ref, ssa.TypeID(0), callee, callee_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	mut call_operands := []ssa.ValueID{cap: operands.len + 1}
	call_operands << fn_ref
	for _, operand in operands {
		call_operands << operand
	}
	call := m.add_instr(.call, block_id, ret_type, call_operands)
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	for _, operand in operands {
		g.set_stack_slot(operand, -16)
	}
	g.set_stack_slot(call, -32)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	g.materialize_text()
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	return words
}

fn test_simd_binary_call_emits_one_vector_instruction() {
	mut m := ssa.Module.new()
	vec4 := simd_vec4_type(mut m)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	left := m.add_instr(.struct_init, block_id, vec4, [])
	right := m.add_instr(.struct_init, block_id, vec4, [])
	words := simd_call_words(mut m, 'simd.add_native', [left, right], vec4)
	assert asm_ldr_q_imm(0, Reg(8), 0) in words
	assert asm_ldr_q_imm(1, Reg(8), 0) in words
	assert asm_fadd_v4s(0, 0, 1) in words
	assert asm_str_q_imm(0, Reg(8), 0) in words
	// The call itself disappears: no branch-with-link reaches the helper.
	assert asm_bl(0) !in words
}

fn test_user_callee_with_a_simd_helper_name_is_left_alone() {
	// A main-module function can have the same name and representation while
	// computing something unrelated. Only helpers in simd are intrinsic targets.
	mut m := ssa.Module.new()
	vec4 := simd_vec4_type(mut m)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	left := m.add_instr(.struct_init, block_id, vec4, [])
	right := m.add_instr(.struct_init, block_id, vec4, [])
	words := simd_call_words(mut m, 'add_native', [left, right], vec4)
	assert simd_intrinsic_op('add_native') == none
	assert asm_fadd_v4s(0, 0, 1) !in words
	assert asm_bl(0) in words
}

fn test_simd_operator_spelling_also_matches() {
	// The operator itself always reaches the backend as a call, so it is
	// lowered directly even when the helper it names has been expanded away.
	mut m := ssa.Module.new()
	vec4 := simd_vec4_type(mut m)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	left := m.add_instr(.struct_init, block_id, vec4, [])
	right := m.add_instr(.struct_init, block_id, vec4, [])
	words := simd_call_words(mut m, 'simd.F32x4.+', [left, right], vec4)
	assert asm_fadd_v4s(0, 0, 1) in words
	assert asm_bl(0) !in words
}

fn test_simd_sqrt_call_uses_one_operand() {
	mut m := ssa.Module.new()
	vec4 := simd_vec4_type(mut m)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	value := m.add_instr(.struct_init, block_id, vec4, [])
	words := simd_call_words(mut m, 'simd.sqrt_native', [value], vec4)
	assert asm_ldr_q_imm(0, Reg(8), 0) in words
	assert asm_fsqrt_v4s(0, 0) in words
	assert asm_str_q_imm(0, Reg(8), 0) in words
}

fn test_simd_other_lane_operations_map_to_matching_instructions() {
	mut m := ssa.Module.new()
	vec4 := simd_vec4_type(mut m)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	left := m.add_instr(.struct_init, block_id, vec4, [])
	right := m.add_instr(.struct_init, block_id, vec4, [])
	sub_words := simd_call_words(mut m, 'simd.sub_native', [left, right], vec4)
	assert asm_fsub_v4s(0, 0, 1) in sub_words
	mul_words := simd_call_words(mut m, 'simd.mul_native', [left, right], vec4)
	assert asm_fmul_v4s(0, 0, 1) in mul_words
	div_words := simd_call_words(mut m, 'simd.div_native', [left, right], vec4)
	assert asm_fdiv_v4s(0, 0, 1) in div_words
}

fn test_simd_call_with_four_f64_lanes_is_not_a_vector_operation() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	quad4 := m.type_store.get_array(f64_type, 4)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	left := m.add_instr(.struct_init, block_id, quad4, [])
	right := m.add_instr(.struct_init, block_id, quad4, [])
	words := simd_call_words(mut m, 'simd.add_native', [left, right], quad4)
	// Four f64 lanes do not fit a 128-bit register pair, so the helper is called
	// and its own body does the work.
	assert asm_fadd_v4s(0, 0, 1) !in words
	assert asm_ldr_q_imm(0, Reg(8), 0) !in words
	assert asm_bl(0) in words
}

fn test_simd_unknown_callee_is_left_alone() {
	mut m := ssa.Module.new()
	vec4 := simd_vec4_type(mut m)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	left := m.add_instr(.struct_init, block_id, vec4, [])
	right := m.add_instr(.struct_init, block_id, vec4, [])
	words := simd_call_words(mut m, 'simd.mystery', [left, right], vec4)
	// A callee that is not one of the helpers must not produce four-lane
	// arithmetic, whatever else it is compiled to. The lookup returns none for
	// this name; if it ever returned the enum's zero value instead, `vadd` would
	// be emitted for every unrelated callee.
	assert simd_intrinsic_op('simd.mystery') == none
	assert asm_fadd_v4s(0, 0, 1) !in words
	assert asm_ldr_q_imm(0, Reg(8), 0) !in words
}

fn math_call_words(mut m ssa.Module, callee string, arg_type ssa.TypeID, ret_type ssa.TypeID,
	value string) []u32 {
	callee_id := m.new_function(callee, ret_type)
	mut callee_fn := m.funcs[callee_id]
	callee_fn.is_c_extern = true
	m.funcs[callee_id] = callee_fn
	fn_ref := m.add_value(.func_ref, ssa.TypeID(0), callee, callee_id)
	caller_id := m.new_function('caller', ret_type)
	block_id := m.add_block(caller_id, 'entry')
	input := m.add_value(.constant, arg_type, value, 0)
	call := m.add_instr(.call, block_id, ret_type, [fn_ref, input])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(call, -8)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	g.materialize_text()
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	return words
}

fn test_libm_sqrt_becomes_one_instruction() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	words := math_call_words(mut m, 'sqrt', f64_type, f64_type, '4')
	assert asm_fsqrt_d(0, 0) in words
	assert asm_bl(0) !in words
}

fn test_libm_sqrtf_keeps_single_precision() {
	mut m := ssa.Module.new()
	f32_type := m.type_store.get_float(32)
	words := math_call_words(mut m, 'sqrtf', f32_type, f32_type, '4')
	assert asm_fsqrt_s(0, 0) in words
	assert asm_fsqrt_d(0, 0) !in words
	assert asm_bl(0) !in words
}

fn test_other_libm_calls_are_left_alone() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	words := math_call_words(mut m, 'sin', f64_type, f64_type, '4')
	assert asm_fsqrt_d(0, 0) !in words
	assert asm_bl(0) in words
}
