// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import os

const coff_image_file_machine_amd64 = u16(0x8664)
const coff_image_sym_class_external = u8(2)
const coff_image_sym_class_static = u8(3)
const coff_image_sym_dtype_function = u16(0x20)

const coff_image_rel_amd64_rel32 = u16(0x0004)

const coff_image_scn_cnt_code = u32(0x00000020)
const coff_image_scn_cnt_initialized_data = u32(0x00000040)
const coff_image_scn_align_8bytes = u32(0x00400000)
const coff_image_scn_align_16bytes = u32(0x00500000)
const coff_image_scn_mem_execute = u32(0x20000000)
const coff_image_scn_mem_read = u32(0x40000000)
const coff_image_scn_mem_write = u32(0x80000000)

pub struct CoffObject {
pub mut:
	text_data []u8
	data_data []u8
	rodata    []u8

	symbols     []CoffSymbol
	str_table   []u8
	sym_by_name map[string]int

	text_relocs []CoffRelocation
}

struct CoffSymbol {
mut:
	name          string
	name_off      int
	value         u32
	section       i16
	type_         u16
	storage_class u8
}

struct CoffRelocation {
	offset  u32
	sym_idx int
	type_   u16
}

struct CoffSection {
	name            string
	data            []u8
	relocs          []CoffRelocation
	characteristics u32
}

pub fn CoffObject.new() &CoffObject {
	return &CoffObject{}
}

pub fn (mut c CoffObject) add_symbol(name string, value u64, is_func bool, section u8) int {
	type_ := if is_func { coff_image_sym_dtype_function } else { u16(0) }
	storage_class := coff_symbol_storage_class(name, section)
	if i := c.sym_by_name[name] {
		mut s := &c.symbols[i]
		s.value = u32(value)
		s.section = i16(section)
		s.type_ = type_
		s.storage_class = storage_class
		return i
	}

	idx := c.symbols.len
	c.symbols << CoffSymbol{
		name:          name
		name_off:      c.add_string_if_needed(name)
		value:         u32(value)
		section:       i16(section)
		type_:         type_
		storage_class: storage_class
	}
	c.sym_by_name[name] = idx
	return idx
}

pub fn (mut c CoffObject) add_undefined(name string) int {
	if i := c.sym_by_name[name] {
		return i
	}

	idx := c.symbols.len
	c.symbols << CoffSymbol{
		name:          name
		name_off:      c.add_string_if_needed(name)
		value:         0
		section:       0
		type_:         0
		storage_class: coff_image_sym_class_external
	}
	c.sym_by_name[name] = idx
	return idx
}

fn coff_symbol_storage_class(name string, section u8) u8 {
	if section != 0 && name.starts_with('L_') {
		return coff_image_sym_class_static
	}
	return coff_image_sym_class_external
}

pub fn (mut c CoffObject) add_text_reloc(offset int, sym_idx int, type_ u16) {
	c.text_relocs << CoffRelocation{
		offset:  u32(offset)
		sym_idx: sym_idx
		type_:   type_
	}
}

fn (mut c CoffObject) add_string_if_needed(s string) int {
	if s.len <= 8 {
		return 0
	}
	off := 4 + c.str_table.len
	c.str_table << s.bytes()
	c.str_table << 0
	return off
}

pub fn (mut c CoffObject) write(path string) {
	sections := [
		CoffSection{
			name:            '.text'
			data:            c.text_data
			relocs:          c.text_relocs
			characteristics: coff_image_scn_cnt_code | coff_image_scn_mem_execute | coff_image_scn_mem_read | coff_image_scn_align_16bytes
		},
		CoffSection{
			name:            '.rdata'
			data:            c.rodata
			characteristics: coff_image_scn_cnt_initialized_data | coff_image_scn_mem_read | coff_image_scn_align_8bytes
		},
		CoffSection{
			name:            '.data'
			data:            c.data_data
			characteristics: coff_image_scn_cnt_initialized_data | coff_image_scn_mem_read | coff_image_scn_mem_write | coff_image_scn_align_8bytes
		},
	]

	header_size := 20
	section_header_size := 40
	reloc_entry_size := 10
	mut current_offset := header_size + (sections.len * section_header_size)

	mut raw_offsets := []int{len: sections.len}
	for i, section in sections {
		if section.data.len > 0 {
			current_offset = align_int(current_offset, 4)
			raw_offsets[i] = current_offset
			current_offset += section.data.len
		}
	}

	mut reloc_offsets := []int{len: sections.len}
	for i, section in sections {
		if section.relocs.len > 0 {
			current_offset = align_int(current_offset, 4)
			reloc_offsets[i] = current_offset
			current_offset += section.relocs.len * reloc_entry_size
		}
	}

	symbol_table_off := align_int(current_offset, 4)
	string_table_size := 4 + c.str_table.len

	mut buf := []u8{}
	write_u16_le(mut buf, coff_image_file_machine_amd64)
	write_u16_le(mut buf, u16(sections.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, u32(symbol_table_off))
	write_u32_le(mut buf, u32(c.symbols.len))
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)

	for i, section in sections {
		write_fixed_string(mut buf, section.name, 8)
		write_u32_le(mut buf, 0)
		write_u32_le(mut buf, 0)
		write_u32_le(mut buf, u32(section.data.len))
		write_u32_le(mut buf, u32(raw_offsets[i]))
		write_u32_le(mut buf, u32(reloc_offsets[i]))
		write_u32_le(mut buf, 0)
		nrelocs := coff_relocation_count_for_header(section) or { panic(err) }
		write_u16_le(mut buf, nrelocs)
		write_u16_le(mut buf, 0)
		write_u32_le(mut buf, section.characteristics)
	}

	for i, section in sections {
		if raw_offsets[i] == 0 {
			continue
		}
		for buf.len < raw_offsets[i] {
			buf << 0
		}
		buf << section.data
	}

	for i, section in sections {
		if reloc_offsets[i] == 0 {
			continue
		}
		for buf.len < reloc_offsets[i] {
			buf << 0
		}
		for reloc in section.relocs {
			write_u32_le(mut buf, reloc.offset)
			write_u32_le(mut buf, u32(reloc.sym_idx))
			write_u16_le(mut buf, reloc.type_)
		}
	}

	for buf.len < symbol_table_off {
		buf << 0
	}
	for sym in c.symbols {
		c.write_symbol_name(mut buf, sym)
		write_u32_le(mut buf, sym.value)
		write_u16_le(mut buf, u16(sym.section))
		write_u16_le(mut buf, sym.type_)
		buf << sym.storage_class
		buf << 0
	}

	write_u32_le(mut buf, u32(string_table_size))
	buf << c.str_table

	os.write_file_array(path, buf) or { panic(err) }
}

fn coff_relocation_count_for_header(section CoffSection) !u16 {
	if section.relocs.len > 0xffff {
		return error('COFF section ${section.name} has ${section.relocs.len} relocations; extended relocations are not supported')
	}
	return u16(section.relocs.len)
}

fn (c CoffObject) write_symbol_name(mut buf []u8, sym CoffSymbol) {
	if sym.name.len <= 8 {
		write_fixed_string(mut buf, sym.name, 8)
		return
	}
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, u32(sym.name_off))
}
