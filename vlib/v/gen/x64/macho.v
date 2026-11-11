// Copyright (c) 2026. Mach-O (x86_64) object writer for the V x64 native backend.
// Ported from vlib/v/gen/arm64/macho.v and adapted for x86_64 Mach-O relocs.
module x64

import os
import v.gen.elf

const mh_magic_64_x64 = u32(0xfeedfacf)
const cpu_type_x86_64 = 0x01000007
const cpu_subtype_x86_64_all = 3

const lc_segment_64_x64 = 0x19
const lc_symtab_x64 = 0x2

// x86_64 Mach-O relocation types
const x86_64_reloc_unsigned = 0
const x86_64_reloc_signed = 1
const x86_64_reloc_branch = 2

pub struct MachOObject {
pub mut:
	text_data []u8
	str_data  []u8 // __cstring
	data_data []u8

	relocs      []RelocationInfoX64
	symbols     []SymbolX64
	str_table   []u8
	sym_by_name map[string]int
}

struct RelocationInfoX64 {
	addr    int
	sym_idx int
	pcrel   bool
	length  int
	extern  bool
	type_   int
}

struct SymbolX64 {
mut:
	name     string
	type_    u8
	sect     u8
	desc     u16
	value    u64
	name_off int
}

pub fn MachOObject.new() &MachOObject {
	mut m := &MachOObject{
		text_data:   []u8{}
		str_data:    []u8{}
		data_data:   []u8{}
		relocs:      []RelocationInfoX64{}
		symbols:     []SymbolX64{}
		str_table:   [u8(0)]
		sym_by_name: map[string]int{}
	}
	return m
}

pub fn (mut m MachOObject) add_symbol(name string, addr u64, is_ext bool, sect u8) int {
	typ := if is_ext { u8(0x0f) } else { u8(0x0e) }
	if i := m.sym_by_name[name] {
		mut s := &m.symbols[i]
		s.type_ = typ
		s.sect = sect
		s.value = addr
		return i
	}
	idx := m.symbols.len
	name_off := m.str_table.len
	unsafe { m.str_table.push_many(name.str, name.len) }
	m.str_table << 0
	m.symbols << SymbolX64{
		name:     name
		type_:    typ
		sect:     sect
		desc:     0
		value:    addr
		name_off: name_off
	}
	m.sym_by_name[name] = idx
	return idx
}

pub fn (mut m MachOObject) add_undefined(name string) int {
	if i := m.sym_by_name[name] {
		return i
	}
	idx := m.symbols.len
	name_off := m.str_table.len
	unsafe { m.str_table.push_many(name.str, name.len) }
	m.str_table << 0
	m.symbols << SymbolX64{
		name:     name
		type_:    0x01
		sect:     0
		desc:     0
		value:    0
		name_off: name_off
	}
	m.sym_by_name[name] = idx
	return idx
}

pub fn (mut m MachOObject) add_reloc(addr int, sym_idx int, typ int, pcrel bool) {
	m.relocs << RelocationInfoX64{
		addr:    addr
		sym_idx: sym_idx
		type_:   typ
		pcrel:   pcrel
		length:  2
		extern:  true
	}
}

// write converts an x64 MachOObject to a Mach-O relocatable object file.
pub fn (mut m MachOObject) write(path string) {
	mut buf := []u8{}

	n_sects := 3
	header_size := 32
	seg_cmd_size := 72 + (80 * n_sects)
	symtab_cmd_size := 24
	load_cmds_size := seg_cmd_size + symtab_cmd_size

	text_len := m.text_data.len
	cstring_len := m.str_data.len
	data_len := m.data_data.len

	text_off := header_size + load_cmds_size
	cstring_off := text_off + text_len
	data_off := cstring_off + cstring_len
	reloc_off := data_off + data_len
	sym_off := reloc_off + (m.relocs.len * 8)
	sym_len := m.symbols.len * 16
	str_off := sym_off + sym_len
	str_size := m.str_table.len

	// 1. Header
	write_u32_le_x64(mut buf, mh_magic_64_x64)
	write_u32_le_x64(mut buf, u32(cpu_type_x86_64))
	write_u32_le_x64(mut buf, u32(cpu_subtype_x86_64_all))
	write_u32_le_x64(mut buf, 1) // MH_OBJECT
	write_u32_le_x64(mut buf, 2) // ncmds
	write_u32_le_x64(mut buf, u32(load_cmds_size))
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)

	// 2. LC_SEGMENT_64
	write_u32_le_x64(mut buf, u32(lc_segment_64_x64))
	write_u32_le_x64(mut buf, u32(seg_cmd_size))
	for _ in 0 .. 16 {
		buf << 0
	}
	write_u64_le_x64(mut buf, 0)
	write_u64_le_x64(mut buf, u64(text_len + cstring_len + data_len))
	write_u64_le_x64(mut buf, u64(text_off))
	write_u64_le_x64(mut buf, u64(text_len + cstring_len + data_len))
	write_u32_le_x64(mut buf, 7)
	write_u32_le_x64(mut buf, 7)
	write_u32_le_x64(mut buf, u32(n_sects))
	write_u32_le_x64(mut buf, 0)

	// Section 1: __text
	write_string_fixed_x64(mut buf, '__text', 16)
	write_string_fixed_x64(mut buf, '__TEXT', 16)
	write_u64_le_x64(mut buf, 0)
	write_u64_le_x64(mut buf, u64(text_len))
	write_u32_le_x64(mut buf, u32(text_off))
	write_u32_le_x64(mut buf, 4)
	write_u32_le_x64(mut buf, u32(reloc_off))
	write_u32_le_x64(mut buf, u32(m.relocs.len))
	write_u32_le_x64(mut buf, u32(0x80000400))
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)

	// Section 2: __cstring
	write_string_fixed_x64(mut buf, '__cstring', 16)
	write_string_fixed_x64(mut buf, '__TEXT', 16)
	write_u64_le_x64(mut buf, u64(text_len))
	write_u64_le_x64(mut buf, u64(cstring_len))
	write_u32_le_x64(mut buf, u32(cstring_off))
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 2)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)

	// Section 3: __data
	write_string_fixed_x64(mut buf, '__data', 16)
	write_string_fixed_x64(mut buf, '__DATA', 16)
	write_u64_le_x64(mut buf, u64(text_len + cstring_len))
	write_u64_le_x64(mut buf, u64(data_len))
	write_u32_le_x64(mut buf, u32(data_off))
	write_u32_le_x64(mut buf, 3)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)
	write_u32_le_x64(mut buf, 0)

	// 3. LC_SYMTAB
	write_u32_le_x64(mut buf, u32(lc_symtab_x64))
	write_u32_le_x64(mut buf, u32(symtab_cmd_size))
	write_u32_le_x64(mut buf, u32(sym_off))
	write_u32_le_x64(mut buf, u32(m.symbols.len))
	write_u32_le_x64(mut buf, u32(str_off))
	write_u32_le_x64(mut buf, u32(str_size))

	buf << m.text_data
	buf << m.str_data
	buf << m.data_data

	for r in m.relocs {
		write_u32_le_x64(mut buf, u32(r.addr))
		mut info := u32(r.sym_idx)
		if r.pcrel {
			info |= (1 << 24)
		}
		info |= (u32(r.length) << 25)
		if r.extern {
			info |= (1 << 27)
		}
		info |= (u32(r.type_) << 28)
		write_u32_le_x64(mut buf, info)
	}

	for s in m.symbols {
		write_u32_le_x64(mut buf, u32(s.name_off))
		buf << s.type_
		buf << s.sect
		write_u16_le_x64(mut buf, s.desc)
		write_u64_le_x64(mut buf, s.value)
	}

	buf << m.str_table

	os.write_file_array(path, buf) or { panic(err) }
}

// write_macho_from_elf converts an ELF64 relocatable (as produced by the x64
// gen) into an x86_64 Mach-O relocatable object file.
pub fn write_macho_from_elf(e &elf.ElfObject, path string) {
	mut mo := MachOObject.new()
	mo.text_data = e.text_data.clone()
	mo.str_data = e.rodata.clone() // strings live in .rodata (shndx 3)
	mo.data_data = e.data_data.clone()

	// Symbols: prepend leading underscore for Mach-O; map sections.
	for sym in e.symbols {
		if sym.name == '' {
			continue
		}
		name := '_' + sym.name
		mut sect := u8(0)
		if sym.shndx == 1 {
			sect = 1
		} else if sym.shndx == 3 {
			sect = 2
		} else if sym.shndx == 2 {
			sect = 3
		}
		is_ext := sym.shndx == 0
		if is_ext {
			mo.add_undefined(name)
		} else {
			mo.add_symbol(name, sym.value, true, sect)
		}
	}

	// Relocations: map ELF types to Mach-O x86_64 types.
	for r in e.text_relocs {
		sym_idx := int(r.info >> 32)
		etype := int(r.info & 0xffffffff)
		mut mtype := x86_64_reloc_signed
		if etype == 4 { // R_X86_64_PLT32
			mtype = x86_64_reloc_branch
		}
		mo.add_reloc(int(r.offset), sym_idx, mtype, true)
	}

	mo.write(path)
}

// --- little-endian helpers ---
fn write_u32_le_x64(mut b []u8, v u32) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
}

fn write_u64_le_x64(mut b []u8, v u64) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
	b << u8(v >> 32)
	b << u8(v >> 40)
	b << u8(v >> 48)
	b << u8(v >> 56)
}

fn write_u16_le_x64(mut b []u8, v u16) {
	b << u8(v)
	b << u8(v >> 8)
}

fn write_string_fixed_x64(mut b []u8, s string, len int) {
	mut bytes := s.bytes()
	for bytes.len < len {
		bytes << 0
	}
	for i in 0 .. len {
		b << bytes[i]
	}
}
