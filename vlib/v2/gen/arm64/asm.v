// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

// ARM64 Instruction Encoding Helpers
// These functions provide type-safe instruction encoding for the ARM64 backend.

// Register type for type safety
type Reg = int

// Common register constants
const xzr = Reg(31) // Zero register

const sp = Reg(31) // Stack pointer (context-dependent)

const fp = Reg(29) // Frame pointer

const lr = Reg(30) // Link register

// === Prologue/Epilogue ===

// stp fp, lr, [sp, -16]! (pre-indexed)
fn asm_stp_fp_lr_pre() u32 {
	return 0xA9BF7BFD
}

// mov fp, sp
fn asm_mov_fp_sp() u32 {
	return 0x910003FD
}

// ldp fp, lr, [sp], 16 (post-indexed)
fn asm_ldp_fp_lr_post() u32 {
	return 0xA8C17BFD
}

// ret
fn asm_ret() u32 {
	return 0xD65F03C0
}

// === Store/Load Pair ===

// stp r1, r2, [sp, -16]! (callee-saved pair)
fn asm_stp_pair_pre(r1 Reg, r2 Reg) u32 {
	return 0xA9BF0000 | (u32(r2) << 10) | (31 << 5) | u32(r1)
}

// ldp r1, r2, [sp], 16 (restore callee-saved pair)
fn asm_ldp_pair_post(r1 Reg, r2 Reg) u32 {
	return 0xA8C10000 | (u32(r2) << 10) | (31 << 5) | u32(r1)
}

// === Arithmetic ===

// add rd, rn, #imm12
fn asm_add_imm(rd Reg, rn Reg, imm u32) u32 {
	return 0x91000000 | (imm << 10) | (u32(rn) << 5) | u32(rd)
}

// add rd, rn, rm
fn asm_add_reg(rd Reg, rn Reg, rm Reg) u32 {
	return 0x8B000000 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// add rd, rn, rm, lsl #3 (for GEP: index * 8)
fn asm_add_reg_lsl3(rd Reg, rn Reg, rm Reg) u32 {
	return 0x8B200C00 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// sub rd, rn, #imm12
fn asm_sub_imm(rd Reg, rn Reg, imm u32) u32 {
	return 0xD1000000 | (imm << 10) | (u32(rn) << 5) | u32(rd)
}

// sub rd, rn, #imm12, lsl #12
fn asm_sub_imm_lsl12(rd Reg, rn Reg, imm u32) u32 {
	return 0xD1400000 | (imm << 10) | (u32(rn) << 5) | u32(rd)
}

// sub rd, rn, rm
fn asm_sub_reg(rd Reg, rn Reg, rm Reg) u32 {
	return 0xCB000000 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// madd rd, rn, rm, xzr (multiply: rd = rn * rm)
fn asm_mul(rd Reg, rn Reg, rm Reg) u32 {
	return 0x9B007C00 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// sdiv rd, rn, rm
fn asm_sdiv(rd Reg, rn Reg, rm Reg) u32 {
	return 0x9AC00C00 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// udiv rd, rn, rm
fn asm_udiv(rd Reg, rn Reg, rm Reg) u32 {
	return 0x9AC00800 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// msub rd, rn, rm, ra (rd = ra - rn * rm)
fn asm_msub(rd Reg, rn Reg, rm Reg, ra Reg) u32 {
	return 0x9B008000 | (u32(rm) << 16) | (u32(ra) << 10) | (u32(rn) << 5) | u32(rd)
}

// === Logical ===

// and rd, rn, rm
fn asm_and(rd Reg, rn Reg, rm Reg) u32 {
	return 0x8A000000 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// orr rd, rn, rm
fn asm_orr(rd Reg, rn Reg, rm Reg) u32 {
	return 0xAA000000 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// eor rd, rn, rm (xor)
fn asm_eor(rd Reg, rn Reg, rm Reg) u32 {
	return 0xCA000000 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// orr rd, xzr, rm (mov rd, rm)
fn asm_mov_reg(rd Reg, rm Reg) u32 {
	return 0xAA0003E0 | (u32(rm) << 16) | u32(rd)
}

// === Shifts ===

// lslv rd, rn, rm (logical shift left variable)
fn asm_lslv(rd Reg, rn Reg, rm Reg) u32 {
	return 0x9AC02000 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// asrv rd, rn, rm (arithmetic shift right variable)
fn asm_asrv(rd Reg, rn Reg, rm Reg) u32 {
	return 0x9AC02800 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// lsrv rd, rn, rm (logical shift right variable)
fn asm_lsrv(rd Reg, rn Reg, rm Reg) u32 {
	return 0x9AC02400 | (u32(rm) << 16) | (u32(rn) << 5) | u32(rd)
}

// lsr xd, xn, #shift (logical shift right immediate, 64-bit)
// Alias for UBFM Xd, Xn, #shift, #63
fn asm_lsr_imm(rd Reg, rn Reg, shift u32) u32 {
	return 0xD340FC00 | ((shift & 0x3F) << 16) | (u32(rn) << 5) | u32(rd)
}

// ubfx xd, xn, #0, #width  (unsigned bitfield extract, zero-extend lower bits)
// Alias for UBFM Xd, Xn, #0, #(width-1)
fn asm_ubfx_lower(rd Reg, rn Reg, width u32) u32 {
	imms := (width - 1) & 0x3F
	return 0xD3400000 | (imms << 10) | (u32(rn) << 5) | u32(rd)
}

// === Compare ===

// cmp rn, rm (subs xzr, rn, rm) — 64-bit
fn asm_cmp_reg(rn Reg, rm Reg) u32 {
	return 0xEB00001F | (u32(rm) << 16) | (u32(rn) << 5)
}

// cmp wn, wm (subs wzr, wn, wm) — 32-bit, sign-aware for i32
fn asm_cmp_reg_w(rn Reg, rm Reg) u32 {
	return 0x6B00001F | (u32(rm) << 16) | (u32(rn) << 5)
}

// === Conditional Set ===

// cset rd, eq
fn asm_cset_eq(rd Reg) u32 {
	return 0x9A9F17E0 | u32(rd)
}

// cset rd, ne
fn asm_cset_ne(rd Reg) u32 {
	return 0x9A9F07E0 | u32(rd)
}

// cset rd, lt
fn asm_cset_lt(rd Reg) u32 {
	return 0x9A9FA7E0 | u32(rd)
}

// cset rd, gt
fn asm_cset_gt(rd Reg) u32 {
	return 0x9A9FD7E0 | u32(rd)
}

// cset rd, le
fn asm_cset_le(rd Reg) u32 {
	return 0x9A9FC7E0 | u32(rd)
}

// cset rd, ge
fn asm_cset_ge(rd Reg) u32 {
	return 0x9A9FB7E0 | u32(rd)
}

// cset rd, hi (unsigned greater than)
fn asm_cset_hi(rd Reg) u32 {
	return 0x9A9F97E0 | u32(rd)
}

// cset rd, hs (unsigned greater or equal)
fn asm_cset_hs(rd Reg) u32 {
	return 0x9A9F37E0 | u32(rd)
}

// cset rd, lo (unsigned less than)
fn asm_cset_lo(rd Reg) u32 {
	return 0x9A9F27E0 | u32(rd)
}

// cset rd, ls (unsigned less or equal)
fn asm_cset_ls(rd Reg) u32 {
	return 0x9A9F87E0 | u32(rd)
}

// === Float Compare ===

// fcmp dn, dm (compare two double-precision floats, sets NZCV)
fn asm_fcmp_d(dn Reg, dm Reg) u32 {
	return 0x1E602000 | (u32(dm) << 16) | (u32(dn) << 5)
}

// === Memory ===

// str rt, [rn] (store 64-bit)
fn asm_str(rt Reg, rn Reg) u32 {
	return 0xF9000000 | (u32(rn) << 5) | u32(rt)
}

// str wt, [rn] (store 32-bit)
fn asm_str_w(rt Reg, rn Reg) u32 {
	return 0xB9000000 | (u32(rn) << 5) | u32(rt)
}

// strh wt, [rn] (store 16-bit)
fn asm_str_h(rt Reg, rn Reg) u32 {
	return 0x79000000 | (u32(rn) << 5) | u32(rt)
}

// strb wt, [rn] (store 8-bit)
fn asm_str_b(rt Reg, rn Reg) u32 {
	return 0x39000000 | (u32(rn) << 5) | u32(rt)
}

// str rt, [rn, #imm12] (scaled by 8)
fn asm_str_imm(rt Reg, rn Reg, imm12 u32) u32 {
	return 0xF9000000 | (imm12 << 10) | (u32(rn) << 5) | u32(rt)
}

// stur xt, [xn, #simm9] (unscaled 64-bit store)
fn asm_stur(rt Reg, rn Reg, simm9 i32) u32 {
	return 0xF8000000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// stur wt, [xn, #simm9] (unscaled 32-bit store)
fn asm_stur_w(rt Reg, rn Reg, simm9 i32) u32 {
	return 0xB8000000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// sturh wt, [xn, #simm9] (unscaled 16-bit store)
fn asm_stur_h(rt Reg, rn Reg, simm9 i32) u32 {
	return 0x78000000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// sturb wt, [xn, #simm9] (unscaled 8-bit store)
fn asm_stur_b(rt Reg, rn Reg, simm9 i32) u32 {
	return 0x38000000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// ldr rt, [rn] (load 64-bit)
fn asm_ldr(rt Reg, rn Reg) u32 {
	return 0xF9400000 | (u32(rn) << 5) | u32(rt)
}

// ldr wt, [rn] (load 32-bit, zero-extend to x)
fn asm_ldr_w(rt Reg, rn Reg) u32 {
	return 0xB9400000 | (u32(rn) << 5) | u32(rt)
}

// ldrsw xt, [rn] (load 32-bit, sign-extend to 64-bit x)
// Used for signed i32 loads to preserve negative values in 64-bit registers.
fn asm_ldrsw(rt Reg, rn Reg) u32 {
	return 0xB9800000 | (u32(rn) << 5) | u32(rt)
}

// ldrh wt, [rn] (load 16-bit, zero-extend to x)
fn asm_ldr_h(rt Reg, rn Reg) u32 {
	return 0x79400000 | (u32(rn) << 5) | u32(rt)
}

// ldrb wt, [rn] (load 8-bit, zero-extend to x)
fn asm_ldr_b(rt Reg, rn Reg) u32 {
	return 0x39400000 | (u32(rn) << 5) | u32(rt)
}

// ldr rt, [rn, #imm12] (load 64-bit with unsigned scaled offset)
// imm12 is the offset divided by 8 (scaled by element size)
fn asm_ldr_imm(rt Reg, rn Reg, imm12 u32) u32 {
	return 0xF9400000 | ((imm12 & 0xFFF) << 10) | (u32(rn) << 5) | u32(rt)
}

// ldur xt, [xn, #simm9] (unscaled 64-bit load)
fn asm_ldur(rt Reg, rn Reg, simm9 i32) u32 {
	return 0xF8400000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// ldur wt, [xn, #simm9] (unscaled 32-bit load, zero-extend)
fn asm_ldur_w(rt Reg, rn Reg, simm9 i32) u32 {
	return 0xB8400000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// ldurh wt, [xn, #simm9] (unscaled 16-bit load, zero-extend)
fn asm_ldur_h(rt Reg, rn Reg, simm9 i32) u32 {
	return 0x78400000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// ldurb wt, [xn, #simm9] (unscaled 8-bit load, zero-extend)
fn asm_ldur_b(rt Reg, rn Reg, simm9 i32) u32 {
	return 0x38400000 | (u32(simm9 & 0x1FF) << 12) | (u32(rn) << 5) | u32(rt)
}

// === Branches ===

// b #imm26 (unconditional branch)
fn asm_b(rel26 i32) u32 {
	return 0x14000000 | (u32(rel26) & 0x3FFFFFF)
}

// bl #imm26 (branch with link / call)
fn asm_bl(rel26 i32) u32 {
	return 0x94000000 | (u32(rel26) & 0x3FFFFFF)
}

// bl (placeholder for relocation)
fn asm_bl_reloc() u32 {
	return 0x94000000
}

// blr rn (branch with link to register)
fn asm_blr(rn Reg) u32 {
	return 0xD63F0000 | (u32(rn) << 5)
}

// cbnz rt, #imm19
fn asm_cbnz(rt Reg, rel19 i32) u32 {
	return 0xB5000000 | ((u32(rel19) & 0x7FFFF) << 5) | u32(rt)
}

// cbz rt, #imm19
fn asm_cbz(rt Reg, rel19 i32) u32 {
	return 0xB4000000 | ((u32(rel19) & 0x7FFFF) << 5) | u32(rt)
}

// b.cond #imm19 (conditional branch)
fn asm_b_cond(cond u32, rel19 i32) u32 {
	return 0x54000000 | ((u32(rel19) & 0x7FFFF) << 5) | cond
}

// Condition codes for b.cond
const cond_eq = u32(0x0) // equal

const cond_ne = u32(0x1) // not equal

const cond_lt = u32(0xB) // less than (signed)

const cond_gt = u32(0xC) // greater than (signed)

const cond_le = u32(0xD) // less or equal (signed)

const cond_ge = u32(0xA) // greater or equal (signed)

// === Move Immediate ===

// movz rd, #imm16 (move wide with zero)
fn asm_movz(rd Reg, imm16 u32) u32 {
	return 0xD2800000 | (imm16 << 5) | u32(rd)
}

// movk rd, #imm16, lsl #shift (move wide with keep)
fn asm_movk(rd Reg, imm16 u32, shift int) u32 {
	hw := u32(shift / 16)
	return 0xF2800000 | (hw << 21) | (imm16 << 5) | u32(rd)
}

// movn rd, #imm16 (move wide with NOT)
fn asm_movn(rd Reg, imm16 u32) u32 {
	return 0x92800000 | (imm16 << 5) | u32(rd)
}

// === PC-Relative Addressing ===

// adrp rd, #imm21 (load page address - placeholder for reloc)
fn asm_adrp(rd Reg) u32 {
	return 0x90000000 | u32(rd)
}

// add rd, rd, #imm12 (add page offset - typically follows adrp)
fn asm_add_pageoff(rd Reg) u32 {
	return 0x91000000 | u32(rd) | (u32(rd) << 5)
}

// ldr rd, [rd, #imm12] (load from page offset - for GOT access, typically follows adrp)
fn asm_ldr_pageoff(rd Reg) u32 {
	return 0xF9400000 | u32(rd) | (u32(rd) << 5)
}

// === Stack Operations ===

// sub sp, sp, x10
fn asm_sub_sp_reg(rm Reg) u32 {
	return 0xCB000000 | (u32(rm) << 16) | (31 << 5) | 31
}

// add sp, sp, x10
fn asm_add_sp_reg(rm Reg) u32 {
	return 0x8B000000 | (u32(rm) << 16) | (31 << 5) | 31
}

// sub x10, x29, x10
fn asm_sub_fp_to_reg(rd Reg, rm Reg) u32 {
	return 0xCB0003A0 | (u32(rm) << 16) | u32(rd)
}

// === Float Operations ===

// fadd dd, dn, dm
fn asm_fadd_d(dd int, dn int, dm int) u32 {
	return 0x1E602800 | (u32(dm) << 16) | (u32(dn) << 5) | u32(dd)
}

// fadd d0, d0, d1 (common case)
fn asm_fadd_d0_d0_d1() u32 {
	return 0x1E612800
}

// fsub d0, d0, d1
fn asm_fsub_d0_d0_d1() u32 {
	return 0x1E613800
}

// fmul d0, d0, d1
fn asm_fmul_d0_d0_d1() u32 {
	return 0x1E610800
}

// fdiv d0, d0, d1
fn asm_fdiv_d0_d0_d1() u32 {
	return 0x1E611800
}

// fdiv d2, d0, d1
fn asm_fdiv_d2_d0_d1() u32 {
	return 0x1E611802
}

// frintz d2, d2 (truncate to integer)
fn asm_frintz_d2() u32 {
	return 0x1E65C042
}

// fnmsub d0, d2, d1, d0 (d0 = d0 - d2*d1)
fn asm_fnmsub_d0_d2_d1_d0() u32 {
	return 0x1F618000
}

// === Float Conversions ===

// fmov xd, dn (copy float bits to integer)
fn asm_fmov_x_d(xd Reg, dn int) u32 {
	return 0x9E660000 | (u32(dn) << 5) | u32(xd)
}

// fmov dd, xn (copy integer bits to float)
fn asm_fmov_d_x(dd int, xn Reg) u32 {
	return 0x9E670000 | (u32(xn) << 5) | u32(dd)
}

// fcvtzs xd, dn (float to signed int, truncate toward zero)
fn asm_fcvtzs_x_d(xd Reg, dn int) u32 {
	return 0x9E780000 | (u32(dn) << 5) | u32(xd)
}

// scvtf dd, xn (signed int to float)
fn asm_scvtf_d_x(dd int, xn Reg) u32 {
	return 0x9E620000 | (u32(xn) << 5) | u32(dd)
}

// ucvtf dd, xn (unsigned int to float)
fn asm_ucvtf_d_x(dd int, xn Reg) u32 {
	return 0x9E630000 | (u32(xn) << 5) | u32(dd)
}

// fcvtzu xd, dn (float to unsigned int, truncate toward zero)
fn asm_fcvtzu_x_d(xd Reg, dn int) u32 {
	return 0x9E790000 | (u32(dn) << 5) | u32(xd)
}

// fmov sd, wn (copy 32-bit integer to single-precision float register)
fn asm_fmov_s_w(sd int, wn Reg) u32 {
	return 0x1E270000 | (u32(wn) << 5) | u32(sd)
}

// fmov wd, sn (copy single-precision float to 32-bit integer register)
fn asm_fmov_w_s(wd Reg, sn int) u32 {
	return 0x1E260000 | (u32(sn) << 5) | u32(wd)
}

// fcvt dd, sn (convert single-precision to double-precision float)
fn asm_fcvt_d_s(dd int, sn int) u32 {
	return 0x1E22C000 | (u32(sn) << 5) | u32(dd)
}

// fcvt sd, dn (convert double-precision to single-precision float)
fn asm_fcvt_s_d(sd int, dn int) u32 {
	return 0x1E624000 | (u32(dn) << 5) | u32(sd)
}

// === Special ===

// udf #0 (undefined - trap)
fn asm_udf() u32 {
	return 0x00000000
}
