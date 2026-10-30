module ppc64le

import v.ssa
import v.gen.elf

pub const em_ppc64 = 21

pub struct Gen {
	m &ssa.Module
mut:
	elf &elf.ElfObject
}

pub fn Gen.new(m &ssa.Module) &Gen {
	return &Gen{ m: m, elf: elf.ElfObject.new(em_ppc64) }
}

pub fn (mut g Gen) emit32(w u32) {
	g.elf.text_data << u8(w)
	g.elf.text_data << u8(w >> 8)
	g.elf.text_data << u8(w >> 16)
	g.elf.text_data << u8(w >> 24)
}

pub fn (mut g Gen) gen() {
	g.elf.add_symbol('main', u64(g.elf.text_data.len), true, 1)
	g.emit32(0x38600000) // li r3, 0
	g.emit32(0x4e800020) // blr
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
