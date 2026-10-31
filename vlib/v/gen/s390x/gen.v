// Copyright (c) 2026. Native s390x backend (minimal).

module s390x

import v.ssa
import v.gen.elf

pub struct Gen {
	m   &ssa.Module
mut:
	elf &elf.ElfObject
}

pub fn Gen.new(m &ssa.Module) &Gen {
	mut e := elf.ElfObject.new(elf.em_s390)
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
		g.elf.add_symbol(func.name, u64(fn_start), true, 1)
		if func.blocks.len == 0 {
			g.emit16(0x07fe)
			continue
		}
		g.emit16(0x07fe)
	}
}

pub fn (g &Gen) text_offset() int {
	return g.elf.text_data.len
}

fn (mut g Gen) emit16(w u16) {
	g.elf.text_data << u8(w >> 8)
	g.elf.text_data << u8(w)
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write_be(path)
}