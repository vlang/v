module amd64

fn test_amd64_leaf_zero_return_is_literal_31_c0_c3() {
	mut text := []u8{}
	emit_xor_eax_eax(mut text)
	emit_ret(mut text)
	assert text == [u8(0x31), 0xc0, 0xc3]
}

fn test_amd64_call_rel32_placeholder_reports_its_literal_field() {
	mut text := []u8{}
	site := emit_call_rel32_placeholder(mut text)
	assert text == [u8(0xe8), 0x00, 0x00, 0x00, 0x00]
	assert site.field_offset == 1
}

fn test_amd64_jmp_rel32_placeholder_is_literal_e9_cd() {
	mut text := []u8{}
	site := emit_jmp_rel32_placeholder(mut text)
	assert text == [u8(0xe9), 0x00, 0x00, 0x00, 0x00]
	assert site.field_offset == 1
}

fn test_amd64_jmp_rel32_uses_next_instruction_for_forward_backward_and_self() {
	mut forward := []u8{}
	forward_site := emit_jmp_rel32_placeholder(mut forward)
	forward << [u8(0x90), 0x90, 0x90]
	patch_jmp_rel32(mut forward, forward_site, 8) or { panic(err.msg()) }
	assert forward == [u8(0xe9), 0x03, 0x00, 0x00, 0x00, 0x90, 0x90, 0x90]

	mut backward := [u8(0x90)]
	backward_site := emit_jmp_rel32_placeholder(mut backward)
	patch_jmp_rel32(mut backward, backward_site, 0) or { panic(err.msg()) }
	assert backward == [u8(0x90), 0xe9, 0xfa, 0xff, 0xff, 0xff]

	mut self := []u8{}
	self_site := emit_jmp_rel32_placeholder(mut self)
	patch_jmp_rel32(mut self, self_site, 0) or { panic(err.msg()) }
	assert self == [u8(0xe9), 0xfb, 0xff, 0xff, 0xff]
}

fn test_amd64_jmp_rel32_preflight_accepts_edges_and_rejects_overflow() {
	assert (checked_jmp_rel32_displacement(0, 2_147_483_647) or { panic(err.msg()) }) == max_i32
	assert (checked_jmp_rel32_displacement(2_147_483_648, 0) or { panic(err.msg()) }) == min_i32

	mut positive_failed := false
	_ := checked_jmp_rel32_displacement(0, 2_147_483_648) or {
		assert err.msg() == 'amd64: JMP rel32: displacement 2147483648 is outside signed 32-bit range'
		positive_failed = true
		0
	}
	assert positive_failed

	mut negative_failed := false
	_ := checked_jmp_rel32_displacement(2_147_483_649, 0) or {
		assert err.msg() == 'amd64: JMP rel32: displacement -2147483649 is outside signed 32-bit range'
		negative_failed = true
		0
	}
	assert negative_failed
}

fn test_amd64_xor_eax_eax_is_literal_31_c0() {
	mut text := []u8{}
	emit_xor_eax_eax(mut text)
	assert text == [u8(0x31), 0xc0]
}

fn test_amd64_non_leaf_stack_and_call_bytes_are_literal() {
	mut text := []u8{}
	emit_sub_rsp_8(mut text)
	first := emit_call_rel32_placeholder(mut text)
	second := emit_call_rel32_placeholder(mut text)
	emit_xor_eax_eax(mut text)
	emit_add_rsp_8(mut text)
	emit_ret(mut text)

	assert text == [
		u8(0x48),
		0x83,
		0xec,
		0x08,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
		0x31,
		0xc0,
		0x48,
		0x83,
		0xc4,
		0x08,
		0xc3,
	]
	assert first.field_offset == 5
	assert second.field_offset == 10
}

fn test_amd64_windows_non_leaf_stack_bytes_are_literal() {
	mut text := []u8{}
	emit_sub_rsp_40(mut text)
	site := emit_call_rel32_placeholder(mut text)
	emit_xor_eax_eax(mut text)
	emit_add_rsp_40(mut text)
	emit_ret(mut text)

	assert text == [
		u8(0x48),
		0x83,
		0xec,
		0x28,
		0xe8,
		0x00,
		0x00,
		0x00,
		0x00,
		0x31,
		0xc0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xc3,
	]
	assert site.field_offset == 5
}

fn test_amd64_mov_rax_imm64_and_ret_are_exact_little_endian_bytes() {
	mut text := []u8{}
	emit_mov_rax_imm64(mut text, u64(0x0123_4567_89ab_cdef))
	emit_ret(mut text)
	assert text == [
		u8(0x48),
		0xb8,
		0xef,
		0xcd,
		0xab,
		0x89,
		0x67,
		0x45,
		0x23,
		0x01,
		0xc3,
	]
}

fn test_amd64_mov_rax_imm64_preserves_zero_and_all_64_bits() {
	mut zero := []u8{}
	emit_mov_rax_imm64(mut zero, u64(0))
	assert zero == [u8(0x48), 0xb8, 0, 0, 0, 0, 0, 0, 0, 0]

	mut all_bits := []u8{}
	emit_mov_rax_imm64(mut all_bits, u64(0xffff_ffff_ffff_ffff))
	assert all_bits == [u8(0x48), 0xb8, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
}

fn test_amd64_first_integer_argument_immediates_are_exact_little_endian_bytes() {
	mut sysv := []u8{}
	emit_mov_rdi_imm64(mut sysv, u64(0x0123_4567_89ab_cdef))
	assert sysv == [u8(0x48), 0xbf, 0xef, 0xcd, 0xab, 0x89, 0x67, 0x45, 0x23, 0x01]

	mut windows := []u8{}
	emit_mov_rcx_imm64(mut windows, u64(0xffff_ffff_ffff_ffff))
	assert windows == [u8(0x48), 0xb9, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]

	mut zero := []u8{}
	emit_mov_rdi_imm64(mut zero, u64(0))
	emit_mov_rcx_imm64(mut zero, u64(0))
	assert zero == [u8(0x48), 0xbf, 0, 0, 0, 0, 0, 0, 0, 0, 0x48, 0xb9, 0, 0, 0, 0, 0, 0, 0, 0]
}

fn test_amd64_first_integer_argument_moves_to_rax_are_exact() {
	mut text := []u8{}
	emit_mov_rax_rdi(mut text)
	emit_mov_rax_rcx(mut text)
	assert text == [u8(0x48), 0x89, 0xf8, 0x48, 0x89, 0xc8]
}

fn test_amd64_scalar_abi_integer_move_widths_and_stack_slots_are_exact() {
	mut text := []u8{}
	emit_mov_gpr_imm(mut text, .rdi, 1, 0x7f) or { panic(err) }
	emit_mov_gpr_imm(mut text, .rsi, 2, 0x1234) or { panic(err) }
	emit_mov_gpr_imm(mut text, .r8, 4, 0xaabb_ccdd) or { panic(err) }
	emit_mov_rax_gpr(mut text, .rdi, 1) or { panic(err) }
	emit_mov_rax_gpr(mut text, .r8, 4) or { panic(err) }
	emit_mov_rax_rsp_offset(mut text, 8, 8) or { panic(err) }
	emit_mov_rsp_offset_rax(mut text, 32, 4) or { panic(err) }
	assert text == [
		u8(0x40),
		0xb7,
		0x7f,
		0x66,
		0xbe,
		0x34,
		0x12,
		0x41,
		0xb8,
		0xdd,
		0xcc,
		0xbb,
		0xaa,
		0x40,
		0x88,
		0xf8,
		0x44,
		0x89,
		0xc0,
		0x48,
		0x8b,
		0x44,
		0x24,
		0x08,
		0x89,
		0x44,
		0x24,
		0x20,
	]
}
