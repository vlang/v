// Copyright (c) 2026. Native PowerPC 64 LE backend (integer core).

module ppc64le

import v.ssa
import v.gen.elf

// PowerPC64 LE register constants
const reg_r0 = 0
const reg_r1 = 1 // sp
const reg_r2 = 2 // toc
const reg_r3 = 3 // arg0 / return
const reg_r4 = 4
const reg_r5 = 5
const reg_r6 = 6
const reg_r7 = 7
const reg_r8 = 8
const reg_r9 = 9
const reg_r10 = 10
const reg_r11 = 11
const reg_r12 = 12
const reg_r31 = 31 // frame pointer

pub struct Gen {
	m   &ssa.Module
mut:
	elf &elf.ElfObject
}

pub fn Gen.new(m &ssa.Module) &Gen {
	return &Gen{
		m:   m
		elf: elf.ElfObject.new(elf.em_ppc64)
	}
}

pub fn (mut g Gen) gen() {
	for fi in 0 .. g.m.funcs.len {
		func := g.m.funcs[fi]
		if func.is_c_extern {
			continue
		}
		fn_start := g.text_offset()
		g.elf.add_symbol(func.name, u64(fn_start), true, 1)
		if func.blocks.len == 0 {
			// blr (return)
			g.emit32(0x4e800020)
			continue
		}
		// Minimal: emit li r3, 0; blr
		g.emit32(0x38600000) // li r3, 0
		g.emit32(0x4e800020) // blr
	}
}

pub fn (g &Gen) text_offset() int {
	return g.elf.text_data.len
}

fn (mut g Gen) emit32(w u32) {
	g.elf.text_data << u8(w)
	g.elf.text_data << u8(w >> 8)
	g.elf.text_data << u8(w >> 16)
	g.elf.text_data << u8(w >> 24)
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
