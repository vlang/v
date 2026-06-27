// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import os

const macho_mh_magic_64 = u32(0xfeedfacf)
const macho_cpu_type_x86_64 = 0x01000007
const macho_cpu_subtype_x86_64_all = 3
const macho_mh_object = 1
const macho_lc_segment_64 = 0x19
const macho_lc_symtab = 0x2

const x86_64_reloc_signed = 1
const x86_64_reloc_branch = 2
const x86_64_reloc_got_load = 3
const x86_64_reloc_got = 4

pub struct MachOObject {
pub mut:
	text_data []u8
	data_data []u8
	rodata    []u8

	relocs      []MachORelocationInfo
	symbols     []MachOSymbol
	str_table   []u8
	sym_by_name map[string]int
}

struct MachORelocationInfo {
	addr    int
	sym_idx int
	pcrel   bool
	length  int
	extern  bool
	type_   int
}

struct MachOSymbol {
mut:
	name     string
	type_    u8
	sect     u8
	desc     u16
	value    u64
	name_off int
}

pub fn MachOObject.new() &MachOObject {
	return &MachOObject{
		str_table: [u8(0)]
	}
}

pub fn (mut m MachOObject) add_symbol(name string, value u64, is_ext bool, sect u8) int {
	typ := if is_ext { u8(0x0f) } else { u8(0x0e) }
	if i := m.sym_by_name[name] {
		mut s := &m.symbols[i]
		s.type_ = typ
		s.sect = sect
		s.value = value
		return i
	}

	idx := m.symbols.len
	name_off := m.add_string(name)
	m.symbols << MachOSymbol{
		name:     name
		type_:    typ
		sect:     sect
		desc:     0
		value:    value
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
	name_off := m.add_string(name)
	m.symbols << MachOSymbol{
		name:     name
		type_:    0x01 // N_UNDF | N_EXT
		sect:     0
		desc:     0
		value:    0
		name_off: name_off
	}
	m.sym_by_name[name] = idx
	return idx
}

pub fn (mut m MachOObject) add_reloc(addr int, sym_idx int, typ int, pcrel bool, length int) {
	m.relocs << MachORelocationInfo{
		addr:    addr
		sym_idx: sym_idx
		type_:   typ
		pcrel:   pcrel
		length:  length
		extern:  true
	}
}

fn (mut m MachOObject) add_string(s string) int {
	off := m.str_table.len
	m.str_table << s.bytes()
	m.str_table << 0
	return off
}

pub fn (mut m MachOObject) write(path string) {
	mut buf := []u8{}

	n_sects := 3
	header_size := 32
	seg_cmd_size := 72 + (80 * n_sects)
	symtab_cmd_size := 24
	load_cmds_size := seg_cmd_size + symtab_cmd_size

	text_off := align_int(header_size + load_cmds_size, 16)
	text_len := m.text_data.len
	rodata_off := align_int(text_off + text_len, 8)
	rodata_len := m.rodata.len
	data_off := align_int(rodata_off + rodata_len, 8)
	data_len := m.data_data.len

	text_addr := 0
	rodata_addr := align_int(text_addr + text_len, 8)
	data_addr := align_int(rodata_addr + rodata_len, 8)
	total_size := data_addr + data_len

	reloc_off := align_int(data_off + data_len, 8)
	sym_off := align_int(reloc_off + (m.relocs.len * 8), 8)
	sym_len := m.symbols.len * 16
	str_off := sym_off + sym_len
	str_size := m.str_table.len

	write_u32_le(mut buf, macho_mh_magic_64)
	write_u32_le(mut buf, u32(macho_cpu_type_x86_64))
	write_u32_le(mut buf, u32(macho_cpu_subtype_x86_64_all))
	write_u32_le(mut buf, macho_mh_object)
	write_u32_le(mut buf, 2)
	write_u32_le(mut buf, u32(load_cmds_size))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	write_u32_le(mut buf, macho_lc_segment_64)
	write_u32_le(mut buf, u32(seg_cmd_size))
	write_fixed_string(mut buf, '', 16)
	write_u64_le(mut buf, 0)
	write_u64_le(mut buf, u64(total_size))
	write_u64_le(mut buf, u64(text_off))
	write_u64_le(mut buf, u64(data_off + data_len - text_off))
	write_u32_le(mut buf, 7)
	write_u32_le(mut buf, 7)
	write_u32_le(mut buf, u32(n_sects))
	write_u32_le(mut buf, 0)

	write_fixed_string(mut buf, '__text', 16)
	write_fixed_string(mut buf, '__TEXT', 16)
	write_u64_le(mut buf, u64(text_addr))
	write_u64_le(mut buf, u64(text_len))
	write_u32_le(mut buf, u32(text_off))
	write_u32_le(mut buf, 4)
	write_u32_le(mut buf, u32(reloc_off))
	write_u32_le(mut buf, u32(m.relocs.len))
	write_u32_le(mut buf, u32(0x80000400))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	write_fixed_string(mut buf, '__const', 16)
	write_fixed_string(mut buf, '__TEXT', 16)
	write_u64_le(mut buf, u64(rodata_addr))
	write_u64_le(mut buf, u64(rodata_len))
	write_u32_le(mut buf, u32(rodata_off))
	write_u32_le(mut buf, 3)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	write_fixed_string(mut buf, '__data', 16)
	write_fixed_string(mut buf, '__DATA', 16)
	write_u64_le(mut buf, u64(data_addr))
	write_u64_le(mut buf, u64(data_len))
	write_u32_le(mut buf, u32(data_off))
	write_u32_le(mut buf, 3)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)

	write_u32_le(mut buf, macho_lc_symtab)
	write_u32_le(mut buf, u32(symtab_cmd_size))
	write_u32_le(mut buf, u32(sym_off))
	write_u32_le(mut buf, u32(m.symbols.len))
	write_u32_le(mut buf, u32(str_off))
	write_u32_le(mut buf, u32(str_size))

	for buf.len < text_off {
		buf << 0
	}
	buf << m.text_data
	for buf.len < rodata_off {
		buf << 0
	}
	buf << m.rodata
	for buf.len < data_off {
		buf << 0
	}
	buf << m.data_data

	for buf.len < reloc_off {
		buf << 0
	}
	for r in m.relocs {
		write_u32_le(mut buf, u32(r.addr))
		mut info := u32(r.sym_idx)
		if r.pcrel {
			info |= 1 << 24
		}
		info |= u32(r.length) << 25
		if r.extern {
			info |= 1 << 27
		}
		info |= u32(r.type_) << 28
		write_u32_le(mut buf, info)
	}

	for buf.len < sym_off {
		buf << 0
	}
	for s in m.symbols {
		write_u32_le(mut buf, u32(s.name_off))
		buf << s.type_
		buf << s.sect
		write_u16_le(mut buf, s.desc)
		write_u64_le(mut buf, m.symbol_value(s, rodata_addr, data_addr))
	}

	buf << m.str_table

	os.write_file_array(path, buf) or { panic(err) }
}

fn (m MachOObject) symbol_value(s MachOSymbol, rodata_addr int, data_addr int) u64 {
	return match s.sect {
		1 { s.value }
		2 { u64(rodata_addr) + s.value }
		3 { u64(data_addr) + s.value }
		else { s.value }
	}
}

fn align_int(value int, alignment int) int {
	if alignment <= 1 || value % alignment == 0 {
		return value
	}
	return value + (alignment - (value % alignment))
}

fn write_fixed_string(mut b []u8, s string, len int) {
	bytes := s.bytes()
	for i in 0 .. len {
		if i < bytes.len {
			b << bytes[i]
		} else {
			b << 0
		}
	}
}
