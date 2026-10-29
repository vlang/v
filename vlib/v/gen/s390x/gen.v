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
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
