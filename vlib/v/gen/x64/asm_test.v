module x64

import v.ssa

fn test_x64_prologue_and_stack_encodings() {
	mut g := Gen.new(ssa.Module.new())
	asm_endbr64(mut g)
	asm_push_rbp(mut g)
	asm_mov_rbp_rsp(mut g)
	asm_sub_rsp_imm8(mut g, 0x20)
	asm_store_rbp_disp_reg(mut g, -0x18, rax)
	asm_add_rsp_imm8(mut g, 0x20)
	asm_pop_rbp(mut g)
	asm_ret(mut g)
	assert g.elf.text_data == [
		u8(0xf3),
		0x0f,
		0x1e,
		0xfa,
		0x55,
		0x48,
		0x89,
		0xe5,
		0x48,
		0x83,
		0xec,
		0x20,
		0x48,
		0x89,
		0x45,
		0xe8,
		0x48,
		0x83,
		0xc4,
		0x20,
		0x5d,
		0xc3,
	]
}

fn test_x64_extended_register_encodings() {
	mut g := Gen.new(ssa.Module.new())
	asm_store_rbp_disp_reg(mut g, 128, r8)
	asm_mov_reg_imm64(mut g, r9, 0x1122334455667788)
	asm_call_r10(mut g)
	assert g.elf.text_data == [
		u8(0x4c),
		0x89,
		0x85,
		0x80,
		0x00,
		0x00,
		0x00,
		0x49,
		0xb9,
		0x88,
		0x77,
		0x66,
		0x55,
		0x44,
		0x33,
		0x22,
		0x11,
		0x41,
		0xff,
		0xd2,
	]
}

fn test_x64_condition_and_select_encodings() {
	mut g := Gen.new(ssa.Module.new())
	asm_cmp_rax_rcx(mut g)
	asm_setcc_al_movzx(mut g, cc_l)
	asm_test_rax_rax(mut g)
	asm_cmovne_rcx_rdx(mut g)
	asm_je_rel32(mut g)
	assert g.elf.text_data == [
		u8(0x48),
		0x39,
		0xc8,
		0x0f,
		0x9c,
		0xc0,
		0x48,
		0x0f,
		0xb6,
		0xc0,
		0x48,
		0x85,
		0xc0,
		0x48,
		0x0f,
		0x45,
		0xca,
		0x0f,
		0x84,
	]
}

fn test_x64_missing_implementations_remain_undefined() {
	mut m := ssa.Module.new()
	m.new_function('unsupported__body', 0)
	mut g := Gen.new(m)
	for name in ['unsupported__body', '__getpid', 'custom__external'] {
		g.elf.add_undefined(name)
	}
	g.gen()
	assert g.elf.text_data.len == 0
	for symbol in g.elf.symbols {
		if symbol.name in ['unsupported__body', '__getpid', 'custom__external'] {
			assert symbol.shndx == 0, symbol.name
		}
	}
}

fn test_c_integer_returns_are_extended_before_use() {
	for unsigned in [false, true] {
		mut m := ssa.Module.new()
		typ := if unsigned { m.type_store.get_uint(32) } else { m.type_store.get_int(32) }
		function := m.new_function('external_int', typ)
		m.funcs[function].is_c_extern = true
		callee := m.add_value(.func_ref, typ, 'external_int', function)
		result := m.add_value(.instruction, typ, '', 0)
		mut g := Gen.new(m)
		g.reg_map[result] = int(rax)
		g.gen_call(result, ssa.Instruction{ op: .call, typ: typ, operands: [callee] })
		expected := if unsigned {
			[u8(0x31), 0xc0, 0xe8, 0, 0, 0, 0, 0x89, 0xc0]
		} else {
			[u8(0x31), 0xc0, 0xe8, 0, 0, 0, 0, 0x48, 0x63, 0xc0]
		}
		assert g.elf.text_data == expected
	}
}
