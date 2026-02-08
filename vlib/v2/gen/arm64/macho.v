// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

// import encoding.binary
import os

// Mach-O Constants for ARM64
const mh_magic_64 = u32(0xfeedfacf)
const cpu_type_arm64 = 0x0100000c
const cpu_subtype_arm64_all = 0

const lc_segment_64 = 0x19
const lc_symtab = 0x2

const arm64_reloc_branch26 = 2
const arm64_reloc_page21 = 3
const arm64_reloc_pageoff12 = 4
const arm64_reloc_got_load_page21 = 5
const arm64_reloc_got_load_pageoff12 = 6

pub struct MachOObject {
pub mut:
	text_data []u8
	str_data  []u8
	data_data []u8

	relocs    []RelocationInfo
	symbols   []Symbol
	str_table []u8
}

struct RelocationInfo {
	addr    int
	sym_idx int
	pcrel   bool
	length  int
	extern  bool
	type_   int
}

struct Symbol {
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
		str_table: [u8(0)]
	}
	return m
}

pub fn (mut m MachOObject) add_symbol(name string, addr u64, is_ext bool, sect u8) int {
	typ := if is_ext { u8(0x0f) } else { u8(0x0e) } // N_SECT | N_EXT : N_SECT

	// Check if symbol already exists (e.g., was added as undefined earlier)
	for i, mut s in m.symbols {
		if s.name == name {
			// Update the existing symbol to be defined
			s.type_ = typ
			s.sect = sect
			s.value = addr
			return i
		}
	}

	// Add new symbol
	idx := m.symbols.len
	name_off := m.str_table.len
	m.str_table << name.bytes()
	m.str_table << 0

	m.symbols << Symbol{
		name:     name
		type_:    typ
		sect:     sect
		desc:     0
		value:    addr
		name_off: name_off
	}
	return idx
}

pub fn (mut m MachOObject) add_undefined(name string) int {
	// Check for any existing symbol with this name (defined or undefined)
	for i, s in m.symbols {
		if s.name == name {
			return i
		}
	}

	idx := m.symbols.len
	name_off := m.str_table.len
	m.str_table << name.bytes()
	m.str_table << 0

	m.symbols << Symbol{
		name:     name
		type_:    0x01 // N_UNDF | N_EXT
		sect:     0
		desc:     0
		value:    0
		name_off: name_off
	}
	return idx
}

pub fn (mut m MachOObject) add_reloc(addr int, sym_idx int, typ int, pcrel bool) {
	m.relocs << RelocationInfo{
		addr:    addr
		sym_idx: sym_idx // 0-based symbol table index
		type_:   typ
		pcrel:   pcrel
		length:  2
		extern:  true
	}
}

pub fn (mut m MachOObject) write(path string) {
	mut buf := []u8{}

	n_sects := 3
	header_size := 32
	seg_cmd_size := 72 + (80 * n_sects)
	symtab_cmd_size := 24
	load_cmds_size := seg_cmd_size + symtab_cmd_size

	text_off := header_size + load_cmds_size
	text_len := m.text_data.len
	cstring_off := text_off + text_len
	cstring_len := m.str_data.len
	data_off := cstring_off + cstring_len
	data_len := m.data_data.len

	reloc_off := data_off + data_len
	sym_off := reloc_off + (m.relocs.len * 8)
	sym_len := m.symbols.len * 16
	str_off := sym_off + sym_len
	str_size := m.str_table.len

	// 1. Header
	write_u32_le(mut buf, mh_magic_64)
	write_u32_le(mut buf, u32(cpu_type_arm64))
	write_u32_le(mut buf, u32(cpu_subtype_arm64_all))
	write_u32_le(mut buf, 1) // MH_OBJECT
	write_u32_le(mut buf, 2) // ncmds
	write_u32_le(mut buf, u32(load_cmds_size))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	// 2. LC_SEGMENT_64
	write_u32_le(mut buf, u32(lc_segment_64))
	write_u32_le(mut buf, u32(seg_cmd_size))
	for _ in 0 .. 16 {
		buf << 0
	}
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(text_len + cstring_len + data_len))
	write_u64_le(mut buf, u64(text_off))
	write_u64_le(mut buf, u64(text_len + cstring_len + data_len))
	write_u32_le(mut buf, 7)
	write_u32_le(mut buf, 7)
	write_u32_le(mut buf, u32(n_sects))
	write_u32_le(mut buf, 0)

	// Section 1: __text
	write_string_fixed(mut buf, '__text', 16)
	write_string_fixed(mut buf, '__TEXT', 16)
	write_u64_le(mut buf, 0) // Addr 0
	write_u64_le(mut buf, u64(text_len))
	write_u32_le(mut buf, u32(text_off))
	write_u32_le(mut buf, 4)
	write_u32_le(mut buf, u32(reloc_off))
	write_u32_le(mut buf, u32(m.relocs.len))
	write_u32_le(mut buf, u32(0x80000400))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	// Section 2: __cstring
	write_string_fixed(mut buf, '__cstring', 16)
	write_string_fixed(mut buf, '__TEXT', 16)
	write_u64_le(mut buf, u64(text_len)) // Addr = text_len
	write_u64_le(mut buf, u64(cstring_len))
	write_u32_le(mut buf, u32(cstring_off))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 2) // S_CSTRING_LITERALS
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	// Section 3: __data
	write_string_fixed(mut buf, '__data', 16)
	write_string_fixed(mut buf, '__DATA', 16)
	write_u64_le(mut buf, u64(text_len + cstring_len)) // Addr = text_len + cstring_len
	write_u64_le(mut buf, u64(data_len))
	write_u32_le(mut buf, u32(data_off))
	write_u32_le(mut buf, 3) // align 8
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	// 3. LC_SYMTAB
	write_u32_le(mut buf, u32(lc_symtab))
	write_u32_le(mut buf, u32(symtab_cmd_size))
	write_u32_le(mut buf, u32(sym_off))
	write_u32_le(mut buf, u32(m.symbols.len))
	write_u32_le(mut buf, u32(str_off))
	write_u32_le(mut buf, u32(str_size))

	buf << m.text_data
	buf << m.str_data
	buf << m.data_data

	for r in m.relocs {
		write_u32_le(mut buf, u32(r.addr))
		mut info := u32(r.sym_idx)
		if r.pcrel {
			info |= (1 << 24)
		}
		info |= (u32(r.length) << 25)
		if r.extern {
			info |= (1 << 27)
		}
		info |= (u32(r.type_) << 28)
		write_u32_le(mut buf, info)
	}

	for s in m.symbols {
		write_u32_le(mut buf, u32(s.name_off))
		buf << s.type_
		buf << s.sect
		write_u16_le(mut buf, s.desc)
		write_u64_le(mut buf, s.value)
	}

	buf << m.str_table

	os.write_file_array(path, buf) or { panic(err) }
}

fn write_u32_le(mut b []u8, v u32) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
}

fn write_u64_le(mut b []u8, v u64) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
	b << u8(v >> 32)
	b << u8(v >> 40)
	b << u8(v >> 48)
	b << u8(v >> 56)
}

fn write_u16_le(mut b []u8, v u16) {
	b << u8(v)
	b << u8(v >> 8)
}

fn write_string_fixed(mut b []u8, s string, len int) {
	mut bytes := s.bytes()
	for bytes.len < len {
		bytes << 0
	}
	for i in 0 .. len {
		b << bytes[i]
	}
}
