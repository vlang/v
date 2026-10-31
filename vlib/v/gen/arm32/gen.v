// Copyright (c) 2026. Native ARM32 backend (skeleton).
// NOTE: requires an ELF32 writer; currently reuses ELF64 as a placeholder.

module arm32

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
		elf: elf.ElfObject.new(elf.em_arm)
	}
}

pub fn (mut g Gen) gen() {
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
