// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

// x64 Instruction Encoding Helpers
// These functions provide type-safe instruction encoding for the x64 backend.

// Register type for type safety
type Reg = int

// Common register constants (System V AMD64 ABI)
const rax = Reg(0)
const rcx = Reg(1)
const rdx = Reg(2)
const rbx = Reg(3)
const rsp = Reg(4)
const rbp = Reg(5)
const rsi = Reg(6)
const rdi = Reg(7)
const r8 = Reg(8)
const r9 = Reg(9)
const r10 = Reg(10)
const r11 = Reg(11)
const r12 = Reg(12)
const r13 = Reg(13)
const r14 = Reg(14)
const r15 = Reg(15)

// === Prologue/Epilogue ===

// endbr64 (CET/IBT protection)
fn asm_endbr64(mut g Gen) {
	g.emit(0xF3)
	g.emit(0x0F)
	g.emit(0x1E)
	g.emit(0xFA)
}

// push rbp
fn asm_push_rbp(mut g Gen) {
	g.emit(0x55)
}

// mov rbp, rsp
fn asm_mov_rbp_rsp(mut g Gen) {
	g.emit(0x48)
	g.emit(0x89)
	g.emit(0xE5)
}

// pop rbp
fn asm_pop_rbp(mut g Gen) {
	g.emit(0x5D)
}

// ret
fn asm_ret(mut g Gen) {
	g.emit(0xC3)
}

// === Push/Pop ===

// push reg (handles REX for r8-r15)
fn asm_push(mut g Gen, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	if hw_reg >= 8 {
		g.emit(0x41)
		g.emit(0x50 | (hw_reg & 7))
	} else {
		g.emit(0x50 | hw_reg)
	}
}

// pop reg (handles REX for r8-r15)
fn asm_pop(mut g Gen, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	if hw_reg >= 8 {
		g.emit(0x41)
		g.emit(0x58 | (hw_reg & 7))
	} else {
		g.emit(0x58 | hw_reg)
	}
}

// === Stack Arithmetic ===

// sub rsp, imm8
fn asm_sub_rsp_imm8(mut g Gen, imm u8) {
	g.emit(0x48)
	g.emit(0x83)
	g.emit(0xEC)
	g.emit(imm)
}

// sub rsp, imm32
fn asm_sub_rsp_imm32(mut g Gen, imm u32) {
	g.emit(0x48)
	g.emit(0x81)
	g.emit(0xEC)
	g.emit_u32(imm)
}

// add rsp, imm8
fn asm_add_rsp_imm8(mut g Gen, imm u8) {
	g.emit(0x48)
	g.emit(0x83)
	g.emit(0xC4)
	g.emit(imm)
}

// add rsp, imm32
fn asm_add_rsp_imm32(mut g Gen, imm u32) {
	g.emit(0x48)
	g.emit(0x81)
	g.emit(0xC4)
	g.emit_u32(imm)
}

// === Arithmetic ===

// add rax, rcx
fn asm_add_rax_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x01)
	g.emit(0xC8)
}

// sub rax, rcx
fn asm_sub_rax_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x29)
	g.emit(0xC8)
}

// imul rax, rcx
fn asm_imul_rax_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xAF)
	g.emit(0xC1)
}

// cqo (sign-extend rax to rdx:rax)
fn asm_cqo(mut g Gen) {
	g.emit(0x48)
	g.emit(0x99)
}

// idiv rcx (rdx:rax / rcx -> quotient in rax, remainder in rdx)
fn asm_idiv_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0xF7)
	g.emit(0xF9)
}

// div rcx (unsigned: rdx:rax / rcx -> quotient in rax, remainder in rdx)
fn asm_div_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0xF7)
	g.emit(0xF1)
}

// xor rdx, rdx (zero rdx before unsigned division)
fn asm_xor_rdx_rdx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x31)
	g.emit(0xD2)
}

// mov rax, rdx (for getting remainder after idiv)
fn asm_mov_rax_rdx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x89)
	g.emit(0xD0)
}

// === Logical ===

// and rax, rcx
fn asm_and_rax_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x21)
	g.emit(0xC8)
}

// or rax, rcx
fn asm_or_rax_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x09)
	g.emit(0xC8)
}

// xor rax, rcx
fn asm_xor_rax_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x31)
	g.emit(0xC8)
}

// xor eax, eax (clear rax, 2 bytes, also clears AL for variadic calls)
fn asm_xor_eax_eax(mut g Gen) {
	g.emit(0x31)
	g.emit(0xC0)
}

// xor reg, reg (clear register - handles r8-r15)
fn asm_xor_reg_reg(mut g Gen, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	if hw_reg >= 8 {
		g.emit(0x45) // REX.RB
		g.emit(0x31)
		g.emit(0xC0 | ((hw_reg & 7) << 3) | (hw_reg & 7))
	} else {
		g.emit(0x31)
		g.emit(0xC0 | (hw_reg << 3) | hw_reg)
	}
}

// and rax, imm32 (for masking narrow integers)
fn asm_and_rax_imm32(mut g Gen, imm u32) {
	g.emit(0x48)
	g.emit(0x25)
	g.emit_u32(imm)
}

// === Shifts ===

// shl rax, cl
fn asm_shl_rax_cl(mut g Gen) {
	g.emit(0x48)
	g.emit(0xD3)
	g.emit(0xE0)
}

// sar rax, cl (arithmetic shift right)
fn asm_sar_rax_cl(mut g Gen) {
	g.emit(0x48)
	g.emit(0xD3)
	g.emit(0xF8)
}

// shr rax, cl (logical shift right)
fn asm_shr_rax_cl(mut g Gen) {
	g.emit(0x48)
	g.emit(0xD3)
	g.emit(0xE8)
}

// shl rcx, 3 (for GEP: index * 8)
fn asm_shl_rcx_3(mut g Gen) {
	g.emit(0x48)
	g.emit(0xC1)
	g.emit(0xE1)
	g.emit(0x03)
}

// imul rcx, rax (for GEP with non-8 element sizes)
fn asm_imul_rcx_rax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xAF)
	g.emit(0xC8)
}

// === Compare ===

// cmp rax, rcx
fn asm_cmp_rax_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x39)
	g.emit(0xC8)
}

// test rax, rax
fn asm_test_rax_rax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x85)
	g.emit(0xC0)
}

// test reg, reg (handles r8-r15)
fn asm_test_reg_reg(mut g Gen, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 5 // REX.RB
	}
	g.emit(rex)
	g.emit(0x85)
	g.emit(0xC0 | ((hw_reg & 7) << 3) | (hw_reg & 7))
}

// === Conditional Set ===

// Condition codes for setcc
const cc_e = u8(0x94) // equal

const cc_ne = u8(0x95) // not equal

const cc_l = u8(0x9C) // less (signed)

const cc_g = u8(0x9F) // greater (signed)

const cc_le = u8(0x9E) // less or equal (signed)

const cc_ge = u8(0x9D) // greater or equal (signed)

// Unsigned condition codes
const cc_b = u8(0x92) // below (unsigned <)

const cc_a = u8(0x97) // above (unsigned >)

const cc_be = u8(0x96) // below or equal (unsigned <=)

const cc_ae = u8(0x93) // above or equal (unsigned >=)

// setcc al + movzx rax, al
fn asm_setcc_al_movzx(mut g Gen, cc u8) {
	g.emit(0x0F)
	g.emit(cc)
	g.emit(0xC0) // setcc al
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xB6)
	g.emit(0xC0) // movzx rax, al
}

// === Memory ===

// mov [rcx], rax
fn asm_mov_mem_rcx_rax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x89)
	g.emit(0x01)
}

// mov rax, [rcx]
fn asm_mov_rax_mem_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x8B)
	g.emit(0x01)
}

// mov rax, [base + disp]
fn asm_mov_rax_mem_base_disp(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	mut rex := u8(0x48)
	if base_hw >= 8 {
		rex |= 1 // REX.B
	}
	g.emit(rex)
	g.emit(0x8B)

	rm := base_hw & 7
	needs_sib := rm == 4 // rsp/r12

	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// mov [base + disp], rax
fn asm_mov_mem_base_disp_rax(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	mut rex := u8(0x48)
	if base_hw >= 8 {
		rex |= 1 // REX.B
	}
	g.emit(rex)
	g.emit(0x89)

	rm := base_hw & 7
	needs_sib := rm == 4 // rsp/r12

	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// === Sized Memory Access ===

// mov dword [rbp + disp], eax (32-bit store)
fn asm_store_rbp_disp32_reg32(mut g Gen, disp int, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	// No REX.W for 32-bit; only REX.R if reg is r8-r15
	if hw_reg >= 8 {
		g.emit(0x44) // REX.R
	}
	g.emit(0x89)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3))
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3))
		g.emit_u32(u32(disp))
	}
}

// mov word [rbp + disp], ax (16-bit store)
fn asm_store_rbp_disp16_reg16(mut g Gen, disp int, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	g.emit(0x66) // operand size prefix
	if hw_reg >= 8 {
		g.emit(0x44) // REX.R
	}
	g.emit(0x89)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3))
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3))
		g.emit_u32(u32(disp))
	}
}

// mov byte [rbp + disp], al (8-bit store)
fn asm_store_rbp_disp8_reg8(mut g Gen, disp int, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	// REX prefix needed for spl/bpl/sil/dil or r8b-r15b
	if hw_reg >= 4 {
		mut rex := u8(0x40)
		if hw_reg >= 8 {
			rex |= 4 // REX.R
		}
		g.emit(rex)
	}
	g.emit(0x88)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3))
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3))
		g.emit_u32(u32(disp))
	}
}

// mov byte [rcx], al (8-bit store through pointer)
fn asm_store_mem_rcx_al(mut g Gen) {
	g.emit(0x88)
	g.emit(0x01)
}

// mov word [rcx], ax (16-bit store through pointer)
fn asm_store_mem_rcx_ax(mut g Gen) {
	g.emit(0x66)
	g.emit(0x89)
	g.emit(0x01)
}

// mov dword [rcx], eax (32-bit store through pointer)
fn asm_store_mem_rcx_eax(mut g Gen) {
	g.emit(0x89)
	g.emit(0x01)
}

// movzx eax, byte [rcx] (8-bit load zero-extend)
fn asm_load_byte_mem_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xB6)
	g.emit(0x01)
}

// movzx eax, word [rcx] (16-bit load zero-extend)
fn asm_load_word_mem_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xB7)
	g.emit(0x01)
}

// mov eax, dword [rcx] (32-bit load zero-extend through reg-size)
fn asm_load_dword_mem_rcx(mut g Gen) {
	g.emit(0x8B)
	g.emit(0x01)
}

// movsxd rax, dword [rcx] (32-bit load sign-extend to 64-bit)
fn asm_load_dword_sx_mem_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0x63)
	g.emit(0x01)
}

// movzx rax, byte [rbp + disp] (8-bit load from stack)
fn asm_load_byte_rbp_disp(mut g Gen, disp int) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xB6)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45) // ModRM: rax, [rbp + disp8]
		g.emit(u8(disp))
	} else {
		g.emit(0x85) // ModRM: rax, [rbp + disp32]
		g.emit_u32(u32(disp))
	}
}

// movzx rax, word [rbp + disp] (16-bit load from stack)
fn asm_load_word_rbp_disp(mut g Gen, disp int) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xB7)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45)
		g.emit(u8(disp))
	} else {
		g.emit(0x85)
		g.emit_u32(u32(disp))
	}
}

// movsxd rax, dword [rbp + disp] (32-bit load sign-extend from stack)
fn asm_load_dword_sx_rbp_disp(mut g Gen, disp int) {
	g.emit(0x48)
	g.emit(0x63)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45)
		g.emit(u8(disp))
	} else {
		g.emit(0x85)
		g.emit_u32(u32(disp))
	}
}

// === Sized store through base + displacement ===

// mov byte [base + disp], al
fn asm_store_byte_base_disp(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	if base_hw >= 4 {
		mut rex := u8(0x40)
		if base_hw >= 8 {
			rex |= 1 // REX.B
		}
		g.emit(rex)
	}
	g.emit(0x88)
	rm := base_hw & 7
	needs_sib := rm == 4
	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// mov word [base + disp], ax
fn asm_store_word_base_disp(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	g.emit(0x66)
	if base_hw >= 8 {
		g.emit(0x41)
	}
	g.emit(0x89)
	rm := base_hw & 7
	needs_sib := rm == 4
	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// mov dword [base + disp], eax
fn asm_store_dword_base_disp(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	if base_hw >= 8 {
		g.emit(0x41)
	}
	g.emit(0x89)
	rm := base_hw & 7
	needs_sib := rm == 4
	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// === Sized load through base + displacement ===

// movzx rax, byte [base + disp]
fn asm_load_byte_base_disp(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	mut rex := u8(0x48)
	if base_hw >= 8 {
		rex |= 1
	}
	g.emit(rex)
	g.emit(0x0F)
	g.emit(0xB6)
	rm := base_hw & 7
	needs_sib := rm == 4
	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// movzx rax, word [base + disp]
fn asm_load_word_base_disp(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	mut rex := u8(0x48)
	if base_hw >= 8 {
		rex |= 1
	}
	g.emit(rex)
	g.emit(0x0F)
	g.emit(0xB7)
	rm := base_hw & 7
	needs_sib := rm == 4
	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// movsxd rax, dword [base + disp]
fn asm_load_dword_sx_base_disp(mut g Gen, base Reg, disp int) {
	base_hw := g.map_reg(int(base))
	mut rex := u8(0x48)
	if base_hw >= 8 {
		rex |= 1
	}
	g.emit(rex)
	g.emit(0x63)
	rm := base_hw & 7
	needs_sib := rm == 4
	if disp == 0 && rm != 5 {
		g.emit(rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
}

// lea rax, [rbp + disp8]
fn asm_lea_rax_rbp_disp8(mut g Gen, disp i8) {
	g.emit(0x48)
	g.emit(0x8D)
	g.emit(0x45) // ModRM 01 = disp8
	g.emit(u8(disp))
}

// lea rax, [rbp + disp32]
fn asm_lea_rax_rbp_disp32(mut g Gen, disp i32) {
	g.emit(0x48)
	g.emit(0x8D)
	g.emit(0x85) // ModRM 10 = disp32
	g.emit_u32(u32(disp))
}

// lea reg, [rbp + disp32] (generic register)
fn asm_lea_reg_rbp_disp32(mut g Gen, reg Reg, disp i32) {
	hw_reg := g.map_reg(int(reg))
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 4
	}
	g.emit(rex)
	g.emit(0x8D)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3))
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3))
		g.emit_u32(u32(disp))
	}
}

// lea reg, [rip + disp32] (for globals/strings)
fn asm_lea_reg_rip(mut g Gen, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 4
	}
	g.emit(rex)
	g.emit(0x8D)
	g.emit(0x05 | ((hw_reg & 7) << 3))
}

// === Move Register ===

// mov dst, src (64-bit, handles REX)
fn asm_mov_reg_reg(mut g Gen, dst Reg, src Reg) {
	dst_hw := g.map_reg(int(dst))
	src_hw := g.map_reg(int(src))
	mut rex := u8(0x48)
	if src_hw >= 8 {
		rex |= 4
	}
	if dst_hw >= 8 {
		rex |= 1
	}
	g.emit(rex)
	g.emit(0x89)
	g.emit(0xC0 | ((src_hw & 7) << 3) | (dst_hw & 7))
}

// === Move Immediate ===

// mov reg, imm32 (zero-extends to 64-bit)
fn asm_mov_reg_imm32(mut g Gen, reg Reg, imm u32) {
	hw_reg := g.map_reg(int(reg))
	if hw_reg >= 8 {
		g.emit(0x41) // REX.B
	}
	g.emit(0xB8 | (hw_reg & 7))
	g.emit_u32(imm)
}

// movabs reg, imm64
fn asm_mov_reg_imm64(mut g Gen, reg Reg, imm u64) {
	hw_reg := g.map_reg(int(reg))
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 1
	}
	g.emit(rex)
	g.emit(0xB8 | (hw_reg & 7))
	g.emit_u64(imm)
}

// === Load/Store with displacement ===

// mov reg, [rbp + disp] (load)
fn asm_load_reg_rbp_disp(mut g Gen, reg Reg, disp int) {
	hw_reg := g.map_reg(int(reg))
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 4
	}
	g.emit(rex)
	g.emit(0x8B)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3)) // ModRM 01 = disp8
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3)) // ModRM 10 = disp32
		g.emit_u32(u32(disp))
	}
}

// mov [rbp + disp], reg (store)
fn asm_store_rbp_disp_reg(mut g Gen, disp int, reg Reg) {
	hw_reg := g.map_reg(int(reg))
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 4
	}
	g.emit(rex)
	g.emit(0x89)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3)) // ModRM 01 = disp8
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3)) // ModRM 10 = disp32
		g.emit_u32(u32(disp))
	}
}

// === Branches ===

// jmp rel32
fn asm_jmp_rel32(mut g Gen) {
	g.emit(0xE9)
}

// je rel32
fn asm_je_rel32(mut g Gen) {
	g.emit(0x0F)
	g.emit(0x84)
}

// jne rel32
fn asm_jne_rel32(mut g Gen) {
	g.emit(0x0F)
	g.emit(0x85)
}

// === Call ===

// call rel32
fn asm_call_rel32(mut g Gen) {
	g.emit(0xE8)
}

// call *r10 (indirect call through r10)
fn asm_call_r10(mut g Gen) {
	g.emit(0x41) // REX.B for r10
	g.emit(0xFF) // call opcode
	g.emit(0xD2) // ModRM: call *r10
}

// === Sign/Zero Extension ===

// movsx rax, al (sign-extend byte to qword)
fn asm_movsx_rax_al(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xBE)
	g.emit(0xC0)
}

// movsx rax, ax (sign-extend word to qword)
fn asm_movsx_rax_ax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xBF)
	g.emit(0xC0)
}

// movsxd rax, eax (sign-extend dword to qword)
fn asm_movsxd_rax_eax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x63)
	g.emit(0xC0)
}

// movzx rax, al (zero-extend byte to qword, already part of setcc but standalone)
fn asm_movzx_rax_al(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xB6)
	g.emit(0xC0)
}

// movzx rax, ax (zero-extend word to qword)
fn asm_movzx_rax_ax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xB7)
	g.emit(0xC0)
}

// === SSE2 Float Operations ===

// movsd xmm0, [rax] (load f64 from memory)
fn asm_movsd_xmm0_mem_rax(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x10)
	g.emit(0x00)
}

// movsd [rcx], xmm0 (store f64 to memory)
fn asm_movsd_mem_rcx_xmm0(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x11)
	g.emit(0x01)
}

// movq xmm0, rax (move 64-bit int to xmm0)
fn asm_movq_xmm0_rax(mut g Gen) {
	g.emit(0x66)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x6E)
	g.emit(0xC0)
}

// movq rax, xmm0 (move xmm0 to 64-bit int)
fn asm_movq_rax_xmm0(mut g Gen) {
	g.emit(0x66)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x7E)
	g.emit(0xC0)
}

// movq xmm1, rcx (move 64-bit int to xmm1)
fn asm_movq_xmm1_rcx(mut g Gen) {
	g.emit(0x66)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x6E)
	g.emit(0xC9)
}

// addsd xmm0, xmm1 (f64 add)
fn asm_addsd_xmm0_xmm1(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x58)
	g.emit(0xC1)
}

// subsd xmm0, xmm1 (f64 sub)
fn asm_subsd_xmm0_xmm1(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x5C)
	g.emit(0xC1)
}

// mulsd xmm0, xmm1 (f64 mul)
fn asm_mulsd_xmm0_xmm1(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x59)
	g.emit(0xC1)
}

// divsd xmm0, xmm1 (f64 div)
fn asm_divsd_xmm0_xmm1(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x5E)
	g.emit(0xC1)
}

// movsd xmm2, xmm0 (copy xmm0 to xmm2)
fn asm_movsd_xmm2_xmm0(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x10)
	g.emit(0xD0)
}

// divsd xmm2, xmm1 (f64 div xmm2/xmm1)
fn asm_divsd_xmm2_xmm1(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x5E)
	g.emit(0xD1)
}

// roundsd xmm2, xmm2, 3 (truncate toward zero)
fn asm_roundsd_xmm2_trunc(mut g Gen) {
	g.emit(0x66)
	g.emit(0x0F)
	g.emit(0x3A)
	g.emit(0x0B)
	g.emit(0xD2)
	g.emit(0x03) // _MM_FROUND_TO_ZERO
}

// mulsd xmm2, xmm1
fn asm_mulsd_xmm2_xmm1(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x59)
	g.emit(0xD1)
}

// subsd xmm0, xmm2 (xmm0 = xmm0 - xmm2)
fn asm_subsd_xmm0_xmm2(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x5C)
	g.emit(0xC2)
}

// === SSE2 Float Comparisons ===

// ucomisd xmm0, xmm1 (compare f64, set flags)
fn asm_ucomisd_xmm0_xmm1(mut g Gen) {
	g.emit(0x66)
	g.emit(0x0F)
	g.emit(0x2E)
	g.emit(0xC1)
}

// === SSE2 Float Conversions ===

// cvttsd2si rax, xmm0 (f64 to signed i64, truncate)
fn asm_cvttsd2si_rax_xmm0(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x2C)
	g.emit(0xC0)
}

// cvtsi2sd xmm0, rax (signed i64 to f64)
fn asm_cvtsi2sd_xmm0_rax(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x2A)
	g.emit(0xC0)
}

// cvtsd2ss xmm0, xmm0 (f64 to f32)
fn asm_cvtsd2ss_xmm0(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x0F)
	g.emit(0x5A)
	g.emit(0xC0)
}

// cvtss2sd xmm0, xmm0 (f32 to f64)
fn asm_cvtss2sd_xmm0(mut g Gen) {
	g.emit(0xF3)
	g.emit(0x0F)
	g.emit(0x5A)
	g.emit(0xC0)
}

// movd xmm0, eax (move 32-bit int to xmm0)
fn asm_movd_xmm0_eax(mut g Gen) {
	g.emit(0x66)
	g.emit(0x0F)
	g.emit(0x6E)
	g.emit(0xC0)
}

// movd eax, xmm0 (move xmm0 low 32 bits to eax)
fn asm_movd_eax_xmm0(mut g Gen) {
	g.emit(0x66)
	g.emit(0x0F)
	g.emit(0x7E)
	g.emit(0xC0)
}

// === Special ===

// ud2 (undefined instruction - trap)
fn asm_ud2(mut g Gen) {
	g.emit(0x0F)
	g.emit(0x0B)
}

// rep stosb (memset: rdi=dest, rcx=count, al=value)
fn asm_rep_stosb(mut g Gen) {
	g.emit(0xF3)
	g.emit(0xAA)
}

// rep movsb (memcpy: rdi=dest, rsi=src, rcx=count)
fn asm_rep_movsb(mut g Gen) {
	g.emit(0xF3)
	g.emit(0xA4)
}
