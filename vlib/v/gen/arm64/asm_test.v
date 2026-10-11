module arm64

// The expected words in this file are not derived from the encoder functions
// they test: each one is the encoding that `clang --target=aarch64-linux-gnu`
// produces for the mnemonic in the comment beside it. A hand-written encoder
// that disagrees with an assembler is wrong even when the binary it builds
// happens to run, so the two are asserted against each other here.

const rd_mask = u32(0x1F)
const rn_shift = 5
const rm_shift = 16

fn test_double_precision_encoders_match_reference() {
	// fadd d0, d1, d2 / fadd d5, d6, d7
	assert asm_fadd_d(0, 1, 2) == 0x1E622820
	assert asm_fadd_d(5, 6, 7) == 0x1E6728C5
	// fsub d0, d1, d2 / fsub d5, d6, d7
	assert asm_fsub_d(0, 1, 2) == 0x1E623820
	assert asm_fsub_d(5, 6, 7) == 0x1E6738C5
	// fmul d0, d1, d2 / fmul d5, d6, d7
	assert asm_fmul_d(0, 1, 2) == 0x1E620820
	assert asm_fmul_d(5, 6, 7) == 0x1E6708C5
	// fdiv d0, d1, d2 / fdiv d5, d6, d7
	assert asm_fdiv_d(0, 1, 2) == 0x1E621820
	assert asm_fdiv_d(5, 6, 7) == 0x1E6718C5
	// fsqrt d0, d1 / fsqrt d5, d6
	assert asm_fsqrt_d(0, 1) == 0x1E61C020
	assert asm_fsqrt_d(5, 6) == 0x1E61C0C5
	// fadd d0, d0, d1 / fsub d0, d0, d1 / fmul d0, d0, d1 / fdiv d0, d0, d1
	assert asm_fadd_d0_d0_d1() == 0x1E612800
	assert asm_fsub_d0_d0_d1() == 0x1E613800
	assert asm_fmul_d0_d0_d1() == 0x1E610800
	assert asm_fdiv_d0_d0_d1() == 0x1E611800
	// fdiv d2, d0, d1 / fmsub d0, d2, d1, d0 / fcmp d0, d1
	assert asm_fdiv_d2_d0_d1() == 0x1E611802
	assert asm_fmsub_d0_d2_d1_d0() == 0x1F418040
	assert asm_fcmp_d(Reg(0), Reg(1)) == 0x1E612000
}

fn test_single_precision_encoders_match_reference() {
	// fadd s0, s1, s2 / fadd s5, s6, s7
	assert asm_fadd_s(0, 1, 2) == 0x1E222820
	assert asm_fadd_s(5, 6, 7) == 0x1E2728C5
	// fsub s0, s1, s2 / fsub s5, s6, s7
	assert asm_fsub_s(0, 1, 2) == 0x1E223820
	assert asm_fsub_s(5, 6, 7) == 0x1E2738C5
	// fmul s0, s1, s2 / fmul s5, s6, s7
	assert asm_fmul_s(0, 1, 2) == 0x1E220820
	assert asm_fmul_s(5, 6, 7) == 0x1E2708C5
	// fdiv s0, s1, s2 / fdiv s5, s6, s7
	assert asm_fdiv_s(0, 1, 2) == 0x1E221820
	assert asm_fdiv_s(5, 6, 7) == 0x1E2718C5
	// fsqrt s0, s1 / fsqrt s5, s6
	assert asm_fsqrt_s(0, 1) == 0x1E21C020
	assert asm_fsqrt_s(5, 6) == 0x1E21C0C5
	// frintz s0, s1 / frintz s5, s6
	assert asm_frintz_s(0, 1) == 0x1E25C020
	assert asm_frintz_s(5, 6) == 0x1E25C0C5
	// fmsub s0, s2, s1, s0 / fmsub s5, s2, s1, s5
	assert asm_fmsub_s(0, 2, 1, 0) == 0x1F018040
	assert asm_fmsub_s(5, 2, 1, 5) == 0x1F019445
	// fcmp s0, s1 / fcmp s5, s6
	assert asm_fcmp_s(Reg(0), Reg(1)) == 0x1E212000
	assert asm_fcmp_s(Reg(5), Reg(6)) == 0x1E2620A0
}

fn test_single_print_registers_occupy_the_same_bits_as_double() {
	// The single-precision opcodes are the double-precision ones with a
	// different type field, so a dispatch shared by both widths can rely on the
	// register fields staying in place.
	single := asm_fadd_s(3, 4, 5)
	double := asm_fadd_d(3, 4, 5)
	assert single & rd_mask == 3
	assert double & rd_mask == 3
	assert (single >> rm_shift) & rd_mask == 5
	assert (double >> rm_shift) & rd_mask == 5
	assert (single >> rn_shift) & rd_mask == 4
	assert (double >> rn_shift) & rd_mask == 4
}

fn test_vector_four_lane_encoders_match_reference() {
	// fadd v0.4s, v1.4s, v2.4s / fadd v5.4s, v6.4s, v7.4s
	assert asm_fadd_v4s(0, 1, 2) == 0x4E22D420
	assert asm_fadd_v4s(5, 6, 7) == 0x4E27D4C5
	// fsub v0.4s, v1.4s, v2.4s / fsub v5.4s, v6.4s, v7.4s
	assert asm_fsub_v4s(0, 1, 2) == 0x4EA2D420
	assert asm_fsub_v4s(5, 6, 7) == 0x4EA7D4C5
	// fmul v0.4s, v1.4s, v2.4s / fmul v5.4s, v6.4s, v7.4s
	assert asm_fmul_v4s(0, 1, 2) == 0x6E22DC20
	assert asm_fmul_v4s(5, 6, 7) == 0x6E27DCC5
	// fdiv v0.4s, v1.4s, v2.4s / fdiv v5.4s, v6.4s, v7.4s
	assert asm_fdiv_v4s(0, 1, 2) == 0x6E22FC20
	assert asm_fdiv_v4s(5, 6, 7) == 0x6E27FCC5
	// fsqrt v0.4s, v1.4s / fsqrt v5.4s, v6.4s
	assert asm_fsqrt_v4s(0, 1) == 0x6EA1F820
	assert asm_fsqrt_v4s(5, 6) == 0x6EA1F8C5
}

fn test_vector_load_store_encoders_match_reference() {
	// ldr q0, [x1] / ldr q7, [x8, #16] / ldr q7, [x8, #112]
	assert asm_ldr_q_imm(0, Reg(1), 0) == 0x3DC00020
	assert asm_ldr_q_imm(7, Reg(8), 1) == 0x3DC00507
	assert asm_ldr_q_imm(7, Reg(8), 7) == 0x3DC01D07
	// str q0, [x1] / str q7, [x8, #16] / str q7, [x8, #112]
	assert asm_str_q_imm(0, Reg(1), 0) == 0x3D800020
	assert asm_str_q_imm(7, Reg(8), 1) == 0x3D800507
	assert asm_str_q_imm(7, Reg(8), 7) == 0x3D801D07
	// The immediate counts 16-byte units, so the byte offset must be a multiple
	// of 16; anything else is truncated rather than trapped.
	assert ((asm_ldr_q_imm(3, Reg(2), 1) >> 10) & 0xFFF) == 1
	assert ((asm_str_q_imm(3, Reg(2), 2) >> 10) & 0xFFF) == 2
}

fn test_vector_registers_occupy_the_same_bits_as_scalar_ones() {
	// vd sits in the same five bits as dd, vn as dn and vm as dm, which is what
	// lets one dispatch serve the scalar and four-lane forms of an operation.
	vector := asm_fmul_v4s(3, 4, 5)
	scalar := asm_fmul_d(3, 4, 5)
	assert vector & rd_mask == scalar & rd_mask
	assert (vector >> rn_shift) & rd_mask == (scalar >> rn_shift) & rd_mask
	assert (vector >> rm_shift) & rd_mask == (scalar >> rm_shift) & rd_mask
}
