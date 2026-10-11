module arm64

import v.ssa

// collected_words reads the text emitted so far back as little-endian words.
fn collected_words(g &Gen) []u32 {
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	return words
}

// asm_word_count counts how often `word` was emitted, so a test can claim
// "exactly one instruction of this shape" rather than merely "at least one".
fn asm_word_count(words []u32, word u32) int {
	mut count := 0
	for emitted in words {
		if emitted == word {
			count++
		}
	}
	return count
}

// hfa_call_words emits a C-extern call passing an aggregate of `quad_type` as
// its last argument, preceded by `extra` scalar arguments, and returns every
// word the call produced.
fn hfa_call_words(mut m ssa.Module, callee string, quad_type ssa.TypeID, extra []ssa.ValueID) []u32 {
	external_id := m.new_function(callee, ssa.TypeID(0))
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	m.funcs[external_id] = external
	callee_ref := m.add_value(.func_ref, ssa.TypeID(0), callee, external_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	quad := m.add_instr(.struct_init, block_id, quad_type, [])
	mut operands := []ssa.ValueID{cap: extra.len + 2}
	operands << callee_ref
	for _, value in extra {
		operands << value
	}
	operands << quad
	call := m.add_instr(.call, block_id, ssa.TypeID(0), operands)
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(quad, -16)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	g.materialize_text()
	return collected_words(g)
}

// f32x4_struct_type is four f32 fields in a C struct: the HFA whose four lanes
// one `q` register holds.
fn f32x4_struct_type(mut m ssa.Module) ssa.TypeID {
	f32_type := m.type_store.get_float(32)
	return m.type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [f32_type, f32_type, f32_type, f32_type]
		is_c_struct: true
	})
}

fn test_c_f32x4_struct_argument_loads_all_lanes_with_single_register_loads() {
	mut m := ssa.Module.new()
	quad_type := f32x4_struct_type(mut m)
	// The shape has to be the one being claimed before the assertions below
	// mean anything: four f32 lanes, four bytes apart, sixteen bytes in all.
	assert m.type_size(quad_type) == 16
	mut g := Gen.new(m)
	layout := g.c_homogeneous_float_aggregate(quad_type) or {
		panic('four contiguous f32 lanes must remain an HFA')
	}
	assert layout.elements.len == 4
	for i, element in layout.elements {
		assert element.offset == i * 4
	}
	words := hfa_call_words(mut m, 'consume', quad_type, [])
	// One load per FP register the callee reads, from that lane's own offset. A
	// single `ldr q` would fill V0's four lanes and leave V1..V3 untouched.
	for lane in 0 .. 4 {
		assert asm_word_count(words, asm_ldr_s_imm(lane, Reg(8), u32(lane))) == 1
	}
	assert asm_word_count(words, asm_ldr_q_imm(0, Reg(8), 0)) == 0
	// No lane is moved through an integer register any more.
	for lane in 0 .. 4 {
		assert asm_fmov_s_w(lane, Reg(9)) !in words
	}
	assert asm_fmov_d_x(0, Reg(9)) !in words
}

fn test_c_f32x4_array_argument_loads_all_lanes_with_single_register_loads() {
	mut m := ssa.Module.new()
	array_type := m.type_store.get_array(m.type_store.get_float(32), 4)
	assert m.type_size(array_type) == 16
	words := hfa_call_words(mut m, 'consume', array_type, [])
	for lane in 0 .. 4 {
		assert asm_word_count(words, asm_ldr_s_imm(lane, Reg(8), u32(lane))) == 1
	}
	for lane in 0 .. 4 {
		assert asm_fmov_s_w(lane, Reg(9)) !in words
	}
}

fn test_c_f64_pair_argument_keeps_the_element_by_element_path() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	pair_type := m.type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [f64_type, f64_type]
		is_c_struct: true
	})
	words := hfa_call_words(mut m, 'consume_pair', pair_type, [])
	assert asm_fmov_d_x(0, Reg(9)) in words
	assert asm_fmov_d_x(1, Reg(9)) in words
	assert asm_word_count(words, asm_fmov_d_x(1, Reg(9))) == 1
	assert asm_ldr_q_imm(0, Reg(8), 0) !in words
}

fn test_c_f32x4_argument_overflow_uses_the_stack() {
	mut m := ssa.Module.new()
	quad_type := f32x4_struct_type(mut m)
	f64_type := m.type_store.get_float(64)
	// Seven scalar float arguments leave one FP register free, so the four
	// lanes cannot all reach an FP argument register.
	mut extra := []ssa.ValueID{cap: 7}
	for i in 1 .. 8 {
		extra << m.add_value(.constant, f64_type, i.str(), 0)
	}
	words := hfa_call_words(mut m, 'consume', quad_type, extra)
	assert asm_ldr_q_imm(0, Reg(8), 0) !in words
	for lane in 0 .. 4 {
		assert asm_fmov_s_w(lane, Reg(9)) !in words
	}
	// Both 8-byte words of the aggregate are pushed instead.
	assert asm_str_imm(Reg(8), sp, 0) in words
	assert asm_str_imm(Reg(8), sp, 1) in words
	assert asm_ldr_imm(Reg(8), sp, 0) in words
	assert asm_ldr_imm(Reg(8), sp, 1) in words
}

fn test_non_contiguous_f32x4_argument_keeps_the_element_by_element_path() {
	mut m := ssa.Module.new()
	f32_type := m.type_store.get_float(32)
	padded_type := m.type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [f32_type, f32_type, f32_type, f32_type]
		is_c_struct: true
		alignment:   32
	})
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	quad := m.add_instr(.struct_init, block_id, padded_type, [])
	mut g := Gen.new(m)
	// The trailing padding the alignment forces makes the aggregate too large
	// to be an HFA at all, so no call reaches the argument path with it.
	assert m.type_size(padded_type) == 32
	assert g.c_homogeneous_float_aggregate(padded_type) == none
	// A layout the classifier rejects for a different reason — the fourth lane
	// sits past the packed run — must still be handled one lane at a time,
	// reading that lane from where it lives.
	layout := Arm64HfaLayout{
		elements: [
			Arm64HfaElement{
				typ:    f32_type
				offset: 0
			},
			Arm64HfaElement{
				typ:    f32_type
				offset: 4
			},
			Arm64HfaElement{
				typ:    f32_type
				offset: 8
			},
			Arm64HfaElement{
				typ:    f32_type
				offset: 16
			},
		]
	}
	assert !g.is_f32x4_layout(layout)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(quad, -16)
	g.emit_homogeneous_float_aggregate(quad, layout, 0)
	g.materialize_text()
	words := collected_words(g)
	assert asm_ldr_q_imm(0, Reg(8), 0) !in words
	for lane in 0 .. 4 {
		assert asm_fmov_s_w(lane, Reg(9)) in words
	}
	// The fourth lane's offset is still turned into a bumped address, so the
	// load does not silently take its value from the packed position.
	assert asm_add_reg(Reg(10), Reg(8), Reg(10)) in words
	assert asm_ldr_w(Reg(9), Reg(10)) in words
}
