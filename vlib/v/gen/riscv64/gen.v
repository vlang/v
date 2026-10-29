// Copyright (c) 2026. Native RISC-V 64 backend (skeleton).
// Incremental implementation tracked separately.

module riscv64

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
		elf: elf.ElfObject.new(elf.em_riscv)
	}
}

// gen is a placeholder; instruction selection is implemented incrementally.
pub fn (mut g Gen) gen() {
}

// write_file emits the relocatable object (ELF64, RISC-V).
pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
