module loongarch64

import v.ssa
import v.gen.elf

pub const em_loongarch = 258

pub struct Gen {
	m &ssa.Module
mut:
	elf &elf.ElfObject
}

pub fn Gen.new(m &ssa.Module) &Gen {
	return &Gen{ m: m, elf: elf.ElfObject.new(em_loongarch) }
}

pub fn (mut g Gen) gen() {
	g.elf.add_symbol('main', u64(g.elf.text_data.len), true, 1)
	g.emit32(0x02c00004) // addi.d a0, zero, 0
	g.emit32(0x4c000020) // ret
}

pub fn (mut g Gen) emit32(w u32) {
	g.elf.text_data << u8(w)
	g.elf.text_data << u8(w >> 8)
	g.elf.text_data << u8(w >> 16)
	g.elf.text_data << u8(w >> 24)
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}
