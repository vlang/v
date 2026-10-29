// Copyright (c) 2026. Native RISC-V 64 backend (skeleton + minimal pipeline).

module riscv64

import v.ssa
import v.gen.elf

pub struct Gen {
	m   &ssa.Module
mut:
	elf &elf.ElfObject
}

pub fn Gen.new(m &ssa.Module) &Gen {
	mut e := elf.ElfObject.new(elf.em_riscv)
	// EF_RISCV_FLOAT_ABI_DOUBLE marks the base+double-float ABI for linking.
	e.elf_flags = 0x0004
	return &Gen{
		m:   m
		elf: e
	}
}// gen emits a minimal prologue/epilogue for every function so that an empty
// program links and runs. Instruction coverage is extended incrementally.
pub fn (mut g Gen) gen() {
	for func in g.m.funcs {
		start := g.elf.text_data.len
		g.elf.add_symbol(func.name, u64(start), true, 1)

		// prologue: addi sp, sp, -16
		g.emit32(riscv_addi(2, 2, -16))
		// sd ra, 8(sp)
		g.emit32(riscv_sd(1, 2, 8))
		// sd s0, 0(sp)
		g.emit32(riscv_sd(8, 2, 0))
		// addi s0, sp, 16
		g.emit32(riscv_addi(8, 2, 16))

		if func.name == 'main' {
			// li a0, 0
			g.emit32(riscv_addi(10, 0, 0))
		}

		// epilogue: ld ra, 8(sp)
		g.emit32(riscv_ld(1, 2, 8))
		// ld s0, 0(sp)
		g.emit32(riscv_ld(8, 2, 0))
		// addi sp, sp, 16
		g.emit32(riscv_addi(2, 2, 16))
		// ret
		g.emit32(0x00008067)
	}
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}

fn (mut g Gen) emit32(v u32) {
	g.elf.text_data << u8(v)
	g.elf.text_data << u8(v >> 8)
	g.elf.text_data << u8(v >> 16)
	g.elf.text_data << u8(v >> 24)
}

fn riscv_addi(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return (uimm << 20) | (u32(rs1) << 15) | (u32(rd) << 7) | 0x13
}

fn riscv_ld(rd int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	return (uimm << 20) | (u32(rs1) << 15) | (3 << 12) | (u32(rd) << 7) | 0x03
}

fn riscv_sd(rs2 int, rs1 int, imm int) u32 {
	uimm := u32(imm) & 0xfff
	lo := uimm & 0x1f
	hi := (uimm >> 5) & 0x7f
	return (hi << 25) | (u32(rs2) << 20) | (u32(rs1) << 15) | (3 << 12) | (lo << 7) | 0x23
}
