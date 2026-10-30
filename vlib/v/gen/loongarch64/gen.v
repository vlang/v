module loongarch64

import v.ssa
import v.gen.elf

pub const em_loongarch = 258

// Register numbers (LoongArch64).
const (
	la_zero = 0
	la_ra   = 1
	la_sp   = 3
	la_a0   = 4
	la_a1   = 5
	la_a2   = 6
	la_a3   = 7
	la_a4   = 8
	la_a5   = 9
	la_a6   = 10
	la_a7   = 11
	la_t0   = 12
	la_t1   = 13
	la_t2   = 14
	la_t3   = 15
	la_t4   = 16
	la_t5   = 17
	la_t6   = 18
	la_t7   = 19
	la_t8   = 20
	la_fp   = 22
)

// Instruction helpers.
fn la_add_d(rd int, rj int, rk int) u32 { return 0x00100000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_sub_d(rd int, rj int, rk int) u32 { return 0x00110000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_mul_d(rd int, rj int, rk int) u32 { return 0x001c0000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_div_d(rd int, rj int, rk int) u32 { return 0x00200000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_mod_d(rd int, rj int, rk int) u32 { return 0x00210000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_and(rd int, rj int, rk int) u32  { return 0x00140000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_or(rd int, rj int, rk int) u32   { return 0x00148000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_xor(rd int, rj int, rk int) u32  { return 0x00150000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_sll_d(rd int, rj int, rk int) u32 { return 0x00180000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_srl_d(rd int, rj int, rk int) u32 { return 0x00188000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_sra_d(rd int, rj int, rk int) u32 { return 0x00190000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_addi_d(rd int, rj int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x02c00000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_ld_d(rd int, rj int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x28c00000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_st_d(rd int, rj int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return 0x29c00000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_jirl(rd int, rj int, offs int) u32 {
	uimm := u32(offs >> 2) & 0xffff
	return 0x4c000000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_bl(offs int) u32 {
	uimm := u32(offs >> 2) & 0x3ffffff
	return 0x54000000 | uimm
}
fn la_b(offs int) u32 {
	uimm := u32(offs >> 2) & 0x3ffffff
	return 0x50000000 | uimm
}
fn la_beq(rd int, rj int, offs int) u32 {
	uimm := u32((offs >> 2) & 0xffff)
	return 0x58000000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_bne(rd int, rj int, offs int) u32 {
	uimm := u32((offs >> 2) & 0xffff)
	return 0x5c000000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_blt(rd int, rj int, offs int) u32 {
	uimm := u32((offs >> 2) & 0xffff)
	return 0x60000000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_bge(rd int, rj int, offs int) u32 {
	uimm := u32((offs >> 2) & 0xffff)
	return 0x64000000 | (uimm << 10) | (u32(rj) << 5) | u32(rd)
}
fn la_ret() u32 { return 0x4c000020 }
fn la_slt(rd int, rj int, rk int) u32 { return 0x00120000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_sltu(rd int, rj int, rk int) u32 { return 0x00128000 | (u32(rk) << 10) | (u32(rj) << 5) | u32(rd) }
fn la_pcaddi(rd int, imm int) u32 {
	uimm := u32(imm >> 2) & 0x3ffff
	return 0x18000000 | (uimm << 5) | u32(rd)
}

pub struct Gen {
	m &ssa.Module
mut:
	elf &elf.ElfObject
	stack_size int
}

pub fn Gen.new(m &ssa.Module) &Gen {
	return &Gen{ m: m, elf: elf.ElfObject.new(em_loongarch) }
}

pub fn (mut g Gen) emit32(w u32) {
	g.elf.text_data << u8(w)
	g.elf.text_data << u8(w >> 8)
	g.elf.text_data << u8(w >> 16)
	g.elf.text_data << u8(w >> 24)
}

pub fn (mut g Gen) gen() {
	g.elf.add_symbol('main', u64(g.elf.text_data.len), true, 1)
	g.emit32(la_addi_d(la_a0, la_zero, 0))
	g.emit32(la_ret())
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
