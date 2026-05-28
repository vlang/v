// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import encoding.binary

pub enum ObjectFormat {
	elf
	macho
	coff
}

enum ObjectSection {
	text
	data
	rodata
}

fn (format ObjectFormat) section_index(section ObjectSection) u8 {
	return match format {
		.elf {
			match section {
				.text { 1 }
				.data { 2 }
				.rodata { 3 }
			}
		}
		.macho {
			match section {
				.text { 1 }
				.rodata { 2 }
				.data { 3 }
			}
		}
		.coff {
			match section {
				.text { 1 }
				.rodata { 2 }
				.data { 3 }
			}
		}
	}
}

fn (format ObjectFormat) symbol_name(name string) string {
	if format == .macho && !name.starts_with('L_') {
		return '_' + name
	}
	return name
}

fn (mut g Gen) text_len() int {
	return match g.obj_format {
		.elf { g.elf.text_data.len }
		.macho { g.macho.text_data.len }
		.coff { g.coff.text_data.len }
	}
}

fn (mut g Gen) data_len() int {
	return match g.obj_format {
		.elf { g.elf.data_data.len }
		.macho { g.macho.data_data.len }
		.coff { g.coff.data_data.len }
	}
}

fn (mut g Gen) rodata_len() int {
	return match g.obj_format {
		.elf { g.elf.rodata.len }
		.macho { g.macho.rodata.len }
		.coff { g.coff.rodata.len }
	}
}

fn (mut g Gen) add_data_byte(b u8) {
	match g.obj_format {
		.elf { g.elf.data_data << b }
		.macho { g.macho.data_data << b }
		.coff { g.coff.data_data << b }
	}
}

fn (mut g Gen) add_data(bytes []u8) {
	match g.obj_format {
		.elf { g.elf.data_data << bytes }
		.macho { g.macho.data_data << bytes }
		.coff { g.coff.data_data << bytes }
	}
}

fn (mut g Gen) add_rodata_byte(b u8) {
	match g.obj_format {
		.elf { g.elf.rodata << b }
		.macho { g.macho.rodata << b }
		.coff { g.coff.rodata << b }
	}
}

fn (mut g Gen) add_rodata(bytes []u8) {
	match g.obj_format {
		.elf { g.elf.rodata << bytes }
		.macho { g.macho.rodata << bytes }
		.coff { g.coff.rodata << bytes }
	}
}

fn (mut g Gen) add_symbol(name string, value u64, is_func bool, section ObjectSection) int {
	sym_name := g.obj_format.symbol_name(name)
	sect := g.obj_format.section_index(section)
	return match g.obj_format {
		.elf { g.elf.add_symbol(sym_name, value, is_func, u16(sect)) }
		.macho { g.macho.add_symbol(sym_name, value, !sym_name.starts_with('L_'), sect) }
		.coff { g.coff.add_symbol(sym_name, value, is_func, sect) }
	}
}

fn (mut g Gen) add_undefined(name string) int {
	sym_name := g.obj_format.symbol_name(name)
	return match g.obj_format {
		.elf { g.elf.add_undefined(sym_name) }
		.macho { g.macho.add_undefined(sym_name) }
		.coff { g.coff.add_undefined(sym_name) }
	}
}

fn (mut g Gen) add_call_reloc(sym_idx int) {
	offset := g.text_len()
	match g.obj_format {
		.elf { g.elf.add_text_reloc(u64(offset), sym_idx, r_x86_64_plt32, -4) }
		.macho { g.macho.add_reloc(offset, sym_idx, x86_64_reloc_branch, true, 2) }
		.coff { g.coff.add_text_reloc(offset, sym_idx, coff_image_rel_amd64_rel32) }
	}
}

fn (mut g Gen) add_rip_reloc(sym_idx int) {
	offset := g.text_len()
	match g.obj_format {
		.elf { g.elf.add_text_reloc(u64(offset), sym_idx, r_x86_64_pc32, -4) }
		.macho { g.macho.add_reloc(offset, sym_idx, x86_64_reloc_signed, true, 2) }
		.coff { g.coff.add_text_reloc(offset, sym_idx, coff_image_rel_amd64_rel32) }
	}
}

fn (mut g Gen) emit(b u8) {
	match g.obj_format {
		.elf { g.elf.text_data << b }
		.macho { g.macho.text_data << b }
		.coff { g.coff.text_data << b }
	}
}

fn (mut g Gen) emit_u32(v u32) {
	g.emit(u8(v))
	g.emit(u8(v >> 8))
	g.emit(u8(v >> 16))
	g.emit(u8(v >> 24))
}

fn (mut g Gen) emit_u64(v u64) {
	g.emit_u32(u32(v))
	g.emit_u32(u32(v >> 32))
}

fn (mut g Gen) write_u32(off int, v u32) {
	match g.obj_format {
		.elf { binary.little_endian_put_u32(mut g.elf.text_data[off..off + 4], v) }
		.macho { binary.little_endian_put_u32(mut g.macho.text_data[off..off + 4], v) }
		.coff { binary.little_endian_put_u32(mut g.coff.text_data[off..off + 4], v) }
	}
}

pub fn (mut g Gen) write_file(path string) {
	match g.obj_format {
		.elf { g.elf.write(path) }
		.macho { g.macho.write(path) }
		.coff { g.coff.write(path) }
	}
}

pub fn (mut g Gen) link_executable(path string) ! {
	match g.obj_format {
		.coff {
			if g.abi != .windows {
				return error('x64 PE linking requires Windows ABI code generation')
			}
			mut linker := PeLinker.new(g.coff)
			linker.write(path)!
		}
		else {
			return error('x64 built-in executable linking is only implemented for COFF')
		}
	}
}
