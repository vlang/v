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

// div rcx (unsigned rdx:rax / rcx -> quotient in rax, remainder in rdx)
fn asm_div_rcx(mut g Gen) {
	g.emit(0x48)
	g.emit(0xF7)
	g.emit(0xF1)
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

// xor edx, edx (clear rdx before unsigned division)
fn asm_xor_edx_edx(mut g Gen) {
	g.emit(0x31)
	g.emit(0xD2)
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

// Condition codes
const cc_e = u8(0x94) // equal

const cc_ne = u8(0x95) // not equal

const cc_l = u8(0x9C) // less (signed)

const cc_g = u8(0x9F) // greater (signed)

const cc_le = u8(0x9E) // less or equal (signed)

const cc_ge = u8(0x9D) // greater or equal (signed)

const cc_b = u8(0x92) // below (unsigned less)

const cc_a = u8(0x97) // above (unsigned greater)

const cc_be = u8(0x96) // below or equal (unsigned)

const cc_ae = u8(0x93) // above or equal (unsigned)

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

fn asm_emit_modrm_base_disp(mut g Gen, reg_bits u8, base_hw u8, disp int) {
	rm := base_hw & 7
	needs_sib := rm == 4 // rsp/r12
	if disp == 0 && rm != 5 {
		g.emit((reg_bits << 3) | rm)
		if needs_sib {
			g.emit(0x24)
		}
	} else if disp >= -128 && disp <= 127 {
		g.emit(0x40 | (reg_bits << 3) | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit(u8(disp))
	} else {
		g.emit(0x80 | (reg_bits << 3) | rm)
		if needs_sib {
			g.emit(0x24)
		}
		g.emit_u32(u32(disp))
	}
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

fn asm_load_reg_mem_base_disp_size(mut g Gen, reg Reg, base Reg, disp int, size int) {
	reg_hw := g.map_reg(int(reg))
	base_hw := g.map_reg(int(base))
	if size == 8 {
		mut rex := u8(0x48)
		if reg_hw >= 8 {
			rex |= 4 // REX.R
		}
		if base_hw >= 8 {
			rex |= 1 // REX.B
		}
		g.emit(rex)
		g.emit(0x8B)
		asm_emit_modrm_base_disp(mut g, reg_hw & 7, base_hw, disp)
		return
	}
	if size == 4 {
		mut rex := u8(0)
		if reg_hw >= 8 {
			rex |= 4
		}
		if base_hw >= 8 {
			rex |= 1
		}
		if rex != 0 {
			g.emit(0x40 | rex)
		}
		g.emit(0x8B)
		asm_emit_modrm_base_disp(mut g, reg_hw & 7, base_hw, disp)
		return
	}
	if size == 2 || size == 1 {
		mut rex := u8(0)
		if reg_hw >= 8 {
			rex |= 4
		}
		if base_hw >= 8 {
			rex |= 1
		}
		if rex != 0 {
			g.emit(0x40 | rex)
		}
		g.emit(0x0F)
		g.emit(if size == 2 { u8(0xB7) } else { u8(0xB6) })
		asm_emit_modrm_base_disp(mut g, reg_hw & 7, base_hw, disp)
		return
	}
	asm_load_reg_mem_base_disp_size(mut g, reg, base, disp, 8)
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

fn asm_store_mem_base_disp_reg_size(mut g Gen, base Reg, disp int, reg Reg, size int) {
	reg_hw := g.map_reg(int(reg))
	base_hw := g.map_reg(int(base))
	if size == 8 {
		mut rex := u8(0x48)
		if reg_hw >= 8 {
			rex |= 4 // REX.R
		}
		if base_hw >= 8 {
			rex |= 1 // REX.B
		}
		g.emit(rex)
		g.emit(0x89)
		asm_emit_modrm_base_disp(mut g, reg_hw & 7, base_hw, disp)
		return
	}
	if size == 4 || size == 2 || size == 1 {
		if size == 2 {
			g.emit(0x66)
		}
		mut rex := u8(0)
		if reg_hw >= 8 {
			rex |= 4
		}
		if base_hw >= 8 {
			rex |= 1
		}
		if rex != 0 {
			g.emit(0x40 | rex)
		}
		g.emit(if size == 1 { u8(0x88) } else { u8(0x89) })
		asm_emit_modrm_base_disp(mut g, reg_hw & 7, base_hw, disp)
		return
	}
	asm_store_mem_base_disp_reg_size(mut g, base, disp, reg, 8)
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

fn asm_store_rbp_disp_reg_size(mut g Gen, disp int, reg Reg, size int) {
	asm_store_mem_base_disp_reg_size(mut g, rbp, disp, reg, size)
}

fn asm_cvtsi2ss_xmm0_rax(mut g Gen) {
	g.emit(0xF3)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x2A)
	g.emit(0xC0)
}

fn asm_cvtsi2sd_xmm0_rax(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x2A)
	g.emit(0xC0)
}

fn asm_cvttss2si_rax_xmm0(mut g Gen) {
	g.emit(0xF3)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x2C)
	g.emit(0xC0)
}

fn asm_cvttsd2si_rax_xmm0(mut g Gen) {
	g.emit(0xF2)
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0x2C)
	g.emit(0xC0)
}

fn asm_store_xmm0_rbp_disp(mut g Gen, disp int, size int) {
	if size == 4 {
		g.emit(0xF3)
	} else {
		g.emit(0xF2)
	}
	g.emit(0x0F)
	g.emit(0x11)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45)
		g.emit(u8(disp))
	} else {
		g.emit(0x85)
		g.emit_u32(u32(disp))
	}
}

fn asm_load_xmm_rbp_disp(mut g Gen, xmm int, disp int, size int) {
	if size == 4 {
		g.emit(0xF3)
	} else {
		g.emit(0xF2)
	}
	g.emit(0x0F)
	g.emit(0x10)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | (u8(xmm & 7) << 3))
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | (u8(xmm & 7) << 3))
		g.emit_u32(u32(disp))
	}
}

fn asm_float_binop_xmm0_xmm1(mut g Gen, op u8, size int) {
	if size == 4 {
		g.emit(0xF3)
	} else {
		g.emit(0xF2)
	}
	g.emit(0x0F)
	g.emit(op)
	g.emit(0xC1)
}

fn asm_movsxd_rax_eax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x63)
	g.emit(0xC0)
}

fn asm_movsx_rax_ax(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xBF)
	g.emit(0xC0)
}

fn asm_movsx_rax_al(mut g Gen) {
	g.emit(0x48)
	g.emit(0x0F)
	g.emit(0xBE)
	g.emit(0xC0)
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

// === Special ===

// ud2 (undefined instruction - trap)
fn asm_ud2(mut g Gen) {
	g.emit(0x0F)
	g.emit(0x0B)
}
