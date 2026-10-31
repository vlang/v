// Copyright (c) 2026. Native ARM32 backend (skeleton).

module arm32

import v.ssa
import v.gen.elf

pub struct Gen {
	m &ssa.Module
mut:
	elf &elf.ElfObject32
}

pub fn Gen.new(m &ssa.Module) &Gen {
	mut e := elf.ElfObject32.new(elf.em_arm)
	e.elf_flags = 0x05000000
	return &Gen{
		m:   m
		elf: e
	}
}

pub fn (mut g Gen) gen() {
	for fi in 0 .. g.m.funcs.len {
		func := g.m.funcs[fi]
		if func.is_c_extern {
			continue
		}
		fn_start := g.text_offset()
		g.elf.add_symbol(func.name, u32(fn_start), true, 1)
		if func.blocks.len == 0 {
			g.emit32(0xe12fff1e) // bx lr
			continue
		}
		// Minimal: emit mov r0, #0; bx lr
		g.emit32(0xe3a00000) // mov r0, #0
		g.emit32(0xe12fff1e) // bx lr
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
