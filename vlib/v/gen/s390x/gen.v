// Copyright (c) 2026. Native s390x backend (skeleton).

module s390x

import v.ssa
import v.gen.elf

pub struct Gen {
	m   &ssa.Module
mut:
	elf &elf.ElfObject
}

pub fn Gen.new(m &ssa.Module) &Gen {
	return &Gen{
		m:   m
		elf: elf.ElfObject.new(elf.em_s390)
	}
}

pub fn (mut g Gen) gen() {
	g.elf.add_symbol('main', u64(g.elf.text_data.len), true, 1)
	// lghi r2, 0 ; br r14 (big-endian)
	g.elf.text_data << u8(0xa7)
	g.elf.text_data << u8(0x29)
	g.elf.text_data << u8(0x00)
	g.elf.text_data << u8(0x00)
	g.elf.text_data << u8(0x07)
	g.elf.text_data << u8(0xfe)
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
