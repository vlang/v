module arm64

// The words below are the encodings `clang --target=aarch64-linux-gnu` gives for
// the mnemonic in each comment, as in asm_test.v.

fn test_inline_asm_flag_setting_and_carry_encoders_match_reference() {
	// adds x5, x6, x7 / subs x5, x6, x7
	assert asm_adds_reg(5, 6, 7) == 0xAB0700C5
	assert asm_subs_reg(5, 6, 7) == 0xEB0700C5
	// adc x5, x6, x7 / sbc x5, x6, x7
	assert asm_adc(5, 6, 7) == 0x9A0700C5
	assert asm_sbc(5, 6, 7) == 0xDA0700C5
	// adc x0, x1, xzr
	assert asm_adc(0, 1, xzr) == 0x9A1F0020
}

fn test_inline_asm_multiply_and_trap_encoders_match_reference() {
	// umulh x5, x6, x7
	assert asm_umulh(5, 6, 7) == 0x9BC77CC5
	// mul x0, x1, x2 / umulh x0, x1, x2
	assert asm_mul(0, 1, 2) == 0x9B027C20
	assert asm_umulh(0, 1, 2) == 0x9BC27C20
	// svc #7 / svc #0
	assert asm_svc(7) == 0xD40000E1
	assert asm_svc(0) == 0xD4000001
	// nop
	assert asm_nop() == 0xD503201F
}

fn test_inline_asm_register_names_resolve_to_numbers() {
	assert arm64_asm_register_number('x0') or { -1 } == 0
	assert arm64_asm_register_number('x30') or { -1 } == 30
	assert arm64_asm_register_number('xzr') or { -1 } == 31
	assert arm64_asm_register_number('x31') == none
	assert arm64_asm_register_number('scn') == none
}
