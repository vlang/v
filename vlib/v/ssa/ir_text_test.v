module ssa

// build_ir_text_sample builds a module exercising constants, globals,
// arguments, branches, phis, calls, structs, maps, and asm specs.
fn build_ir_text_sample() &Module {
	mut m := Module.new()
	void_t := TypeID(0)
	i32_type := m.type_store.get_int(32)
	i64_type := m.type_store.get_int(64)
	pair := m.type_store.register(Type{
		kind:        .struct_t
		fields:      [i32_type, i64_type]
		field_names: ['tag', 'payload']
	})
	_ = m.add_global('counter', i32_type)
	callee := m.new_function('add1', i32_type)
	centry := m.add_block(callee, 'entry')
	one := m.get_or_add_const(i32_type, '1')
	px := m.add_value(.argument, i32_type, 'x', 0)
	m.func_add_param(callee, px)
	sum := m.add_instr(.add, centry, i32_type, [px, one])
	asm_val := m.add_instr(.inline_asm, centry, void_t, [one])
	spec_id := m.add_inline_asm_spec(InlineAsmSpec{
		arch:      'x86_64'
		insts:     ['nop']
		clobbered: ['memory']
	})
	m.instruction_set_asm_id(asm_val, spec_id)
	_ = m.add_instr(.ret, centry, i32_type, [sum])
	main_fn := m.new_function('main', i32_type)
	arg := m.add_value(.argument, i32_type, 'argc', 0)
	m.func_add_param(main_fn, arg)
	entry := m.add_block(main_fn, 'entry')
	left := m.add_block(main_fn, 'left')
	right := m.add_block(main_fn, 'right')
	merge := m.add_block(main_fn, 'merge')
	zero := m.get_or_add_const(i32_type, '0')
	ten := m.get_or_add_const(i32_type, '10')
	cmp := m.add_instr(.gt, entry, i32_type, [arg, zero])
	_ = m.add_instr(.br, entry, void_t, [cmp, ValueID(left), ValueID(right)])
	lv := m.add_instr(.add, left, i32_type, [arg, ten])
	_ = m.add_instr(.jmp, left, void_t, [ValueID(merge)])
	add1_ref := m.add_value(.func_ref, i32_type, 'add1', callee)
	rv := m.add_instr(.call, right, i32_type, [add1_ref, arg])
	_ = m.add_instr(.jmp, right, void_t, [ValueID(merge)])
	phi := m.add_instr(.phi, merge, i32_type, [lv, ValueID(left), rv, ValueID(right)])
	_ = m.add_instr(.ret, merge, i32_type, [phi])
	m.c_struct_names[int(pair)] = 'Pair'
	m.export_c_names['main'] = 'c_main'
	m.noinline_fns['add1'] = true
	return m
}

fn test_ir_text_golden_simple_add() {
	mut m := Module.new()
	i32_type := m.type_store.get_int(32)
	fid := m.new_function('main', i32_type)
	entry := m.add_block(fid, 'entry')
	c1 := m.get_or_add_const(i32_type, '1')
	c2 := m.get_or_add_const(i32_type, '2')
	s := m.add_instr(.add, entry, i32_type, [c1, c2])
	_ = m.add_instr(.ret, entry, i32_type, [s])
	text := module_to_text(m)!
	expected := 'vir 1\n' + 'target os - arch - ptr 8 endian little\n' + 'type %t0 = void\n' +
		'type %t1 = i32\n' + 'const %v1 %t1 "1"\n' + 'const %v2 %t1 "2"\n' +
		'func 0 "main" -> %t1 linkage external callconv c_decl params ()\n' +
		'@b0 "entry" :\n' + '  %v3 = add %t1, %v1, %v2\n' + '  %v4 = ret %t1, %v3\n'
	assert text == expected
}

fn test_ir_text_round_trip_is_stable() {
	m := build_ir_text_sample()
	text1 := module_to_text(m)!
	mut m2 := module_from_text(text1)!
	text2 := module_to_text(m2)!
	assert text1 == text2
	assert m2.funcs.len == 2
	assert m2.values.len == m.values.len
	assert m2.type_store.types.len == m.type_store.types.len
	assert m2.globals.len == 1
	assert m2.asm_specs.len == 1
	assert m2.const_cache.len == 3
	// CFG edges were rebuilt from terminators: merge has two predecessors.
	assert m2.blocks[4].preds == [BlockID(2), BlockID(3)]
	assert m2.blocks[4].succs == []
	// Use-lists were rebuilt: the comparison feeds the branch.
	cmp_id := ValueID(10)
	assert m2.values[cmp_id].uses.len == 1
	// Side tables survived.
	assert m2.export_c_names['main'] == 'c_main'
	assert m2.noinline_fns['add1'] == true
	assert m2.asm_specs[0].arch == 'x86_64'
	assert m2.asm_specs[0].insts == ['nop']
}

fn test_ir_text_detached_instructions_round_trip() {
	mut m := Module.new()
	i32_type := m.type_store.get_int(32)
	fid := m.new_function('main', i32_type)
	entry := m.add_block(fid, 'entry')
	c1 := m.get_or_add_const(i32_type, '1')
	c2 := m.get_or_add_const(i32_type, '2')
	s := m.add_instr(.add, entry, i32_type, [c1, c2])
	_ = m.add_instr(.ret, entry, i32_type, [c2])
	// Simulate optimizer dead-code removal: detach the add from its block.
	m.detach_instruction_uses(s)
	mut blk := m.blocks[entry]
	blk.instrs.delete(0)
	m.blocks[entry] = blk
	text := module_to_text(m)!
	assert text.contains('detached\n')
	mut m2 := module_from_text(text)!
	assert module_to_text(m2)! == text
	assert m2.values[s].kind == .instruction
	assert m2.values[s].index >= 0
}

fn test_ir_text_phi_result_value_round_trip() {
	mut m := Module.new()
	i32_type := m.type_store.get_int(32)
	fid := m.new_function('main', i32_type)
	entry := m.add_block(fid, 'entry')
	c1 := m.get_or_add_const(i32_type, '1')
	// A phi result left behind by phi elimination: detached, by value.
	pr := m.add_value(.phi_result, i32_type, '', -1)
	_ = m.add_instr(.ret, entry, i32_type, [c1])
	text := module_to_text(m)!
	assert text.contains('phi_result')
	mut m2 := module_from_text(text)!
	assert module_to_text(m2)! == text
	assert m2.values[pr].kind == .phi_result
}

fn test_ir_text_rejects_bad_input() {
	_ = module_from_text('') or {
		assert err.msg().contains('empty')
		return
	}
	assert false, 'expected error for empty input'
	_ = module_from_text('vir 2\n') or {
		assert err.msg().contains('version')
		return
	}
	assert false, 'expected error for bad version'
	_ = module_from_text('vir 1\nbogus line here\n') or {
		assert err.msg().contains('cannot parse')
		return
	}
	assert false, 'expected error for bogus line'
	m := build_ir_text_sample()
	good := module_to_text(m)!
	bad_op := good.replace('= add ', '= plus ')
	_ = module_from_text(bad_op) or {
		assert err.msg().contains('unknown opcode')
		return
	}
	assert false, 'expected error for unknown opcode'
	bad_type := good.replace('%t1 = i32', '%t1 = i33x')
	_ = module_from_text(bad_type) or {
		assert err.msg().contains('bad type') || err.msg().contains('bad integer')
		return
	}
	assert false, 'expected error for bad type'
}
