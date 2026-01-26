// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module x64

import os

// ELF64 Constants
const ei_mag0 = 0x7f
const ei_mag1 = 0x45 // 'E'
const ei_mag2 = 0x4c // 'L'
const ei_mag3 = 0x46 // 'F'

const elfclass64 = 2
const elfdata2lsb = 1
const ev_current = 1
const et_rel = 1 // Relocatable file
const em_x86_64 = 62

const sht_progbits = 1
const sht_symtab = 2
const sht_strtab = 3
const sht_rela = 4
const sht_nobits = 8

const shf_write = 0x1
const shf_alloc = 0x2
const shf_execinstr = 0x4

// Relocation Types for x86_64
const r_x86_64_64 = 1
const r_x86_64_pc32 = 2
const r_x86_64_plt32 = 4

pub struct ElfObject {
pub mut:
	text_data []u8
	data_data []u8
	rodata    []u8

	// Symbol Table
	symbols   []ElfSymbol
	str_table []u8

	// Relocations
	text_relocs []ElfRela

	// Section Names
	shstr_table []u8
}

struct ElfSymbol {
pub mut:
	name     string
	name_idx int
	info     u8
	shndx    u16
	value    u64
	size     u64
}

struct ElfRela {
	offset u64
	info   u64
	addend i64
}

pub fn ElfObject.new() &ElfObject {
	mut e := &ElfObject{
		str_table:   [u8(0)] // Starts with null byte
		shstr_table: [u8(0)]
	}
	// Add null symbol
	e.symbols << ElfSymbol{
		name:  ''
		shndx: 0
	}
	return e
}

pub fn (mut e ElfObject) add_symbol(name string, value u64, is_func bool, shndx u16) int {
	idx := e.symbols.len
	name_off := e.add_string(name)

	// STB_GLOBAL (1) << 4 | STT_FUNC (2) or STT_OBJECT (1) or STT_NOTYPE (0)
	type_ := if is_func { u8(2) } else { u8(1) } // Func : Object
	bind := u8(1) // Global
	info := (bind << 4) | type_

	e.symbols << ElfSymbol{
		name:     name
		name_idx: name_off
		info:     info
		shndx:    shndx
		value:    value
	}
	return idx
}

pub fn (mut e ElfObject) add_undefined(name string) int {
	// Check existing
	for i, s in e.symbols {
		if s.name == name && s.shndx == 0 && i > 0 {
			return i
		}
	}

	idx := e.symbols.len
	name_off := e.add_string(name)
	// Bind Global (1), Type NoType (0)
	info := u8(0x10)

	e.symbols << ElfSymbol{
		name:     name
		name_idx: name_off
		info:     info
		shndx:    0 // Undefined
		value:    0
	}
	return idx
}

pub fn (mut e ElfObject) add_text_reloc(offset u64, sym_idx int, type_ int, addend i64) {
	// info = (sym_idx << 32) | type
	info := (u64(sym_idx) << 32) | u64(type_)
	e.text_relocs << ElfRela{
		offset: offset
		info:   info
		addend: addend
	}
}

fn (mut e ElfObject) add_string(s string) int {
	off := e.str_table.len
	e.str_table << s.bytes()
	e.str_table << 0
	return off
}

fn (mut e ElfObject) add_sh_string(s string) int {
	off := e.shstr_table.len
	e.shstr_table << s.bytes()
	e.shstr_table << 0
	return off
}

pub fn (mut e ElfObject) write(path string) {
	mut buf := []u8{}

	// --- 1. Prepare Sections ---
	// Indices:
	// 0: Null
	// 1: .text
	// 2: .data
	// 3: .rodata (strings)
	// 4: .symtab
	// 5: .strtab
	// 6: .rela.text
	// 7: .shstrtab

	// Prepare section names in .shstrtab
	off_text_name := e.add_sh_string('.text')
	off_data_name := e.add_sh_string('.data')
	off_rodata_name := e.add_sh_string('.rodata')
	off_symtab_name := e.add_sh_string('.symtab')
	off_strtab_name := e.add_sh_string('.strtab')
	off_rela_text_name := e.add_sh_string('.rela.text')
	off_shstrtab_name := e.add_sh_string('.shstrtab')

	// Calculate Offsets
	ehdr_size := 64
	shdr_entry_size := 64
	num_sections := 8

	mut current_offset := ehdr_size

	// .text (Align 16)
	if current_offset % 16 != 0 {
		current_offset += (16 - (current_offset % 16))
	}
	off_text := current_offset
	current_offset += e.text_data.len

	// .data (Align 8)
	if current_offset % 8 != 0 {
		current_offset += (8 - (current_offset % 8))
	}
	off_data := current_offset
	current_offset += e.data_data.len

	// .rodata (Align 4)
	if current_offset % 4 != 0 {
		current_offset += (4 - (current_offset % 4))
	}
	off_rodata := current_offset
	current_offset += e.rodata.len

	// .symtab (Align 8)
	if current_offset % 8 != 0 {
		current_offset += (8 - (current_offset % 8))
	}
	off_symtab := current_offset
	size_symtab := e.symbols.len * 24
	current_offset += size_symtab

	// .strtab (Align 1)
	off_strtab := current_offset
	current_offset += e.str_table.len

	// .rela.text (Align 8)
	if current_offset % 8 != 0 {
		current_offset += (8 - (current_offset % 8))
	}
	off_rela_text := current_offset
	size_rela_text := e.text_relocs.len * 24
	current_offset += size_rela_text

	// .shstrtab (Align 1)
	off_shstrtab := current_offset
	current_offset += e.shstr_table.len

	// Section Headers (Align 8)
	if current_offset % 8 != 0 {
		current_offset += (8 - (current_offset % 8))
	}
	off_shdrs := current_offset

	// --- 2. Write ELF Header ---
	buf << u8(ei_mag0)
	buf << u8(ei_mag1)
	buf << u8(ei_mag2)
	buf << u8(ei_mag3)
	buf << u8(elfclass64)
	buf << u8(elfdata2lsb)
	buf << u8(ev_current)
	buf << u8(0) // ABI System V
	buf << u8(0) // ABI Version
	for _ in 0 .. 7 {
		buf << 0
	} // Pad

	write_u16_le(mut buf, et_rel)
	write_u16_le(mut buf, em_x86_64)
	write_u32_le(mut buf, ev_current)
	write_u64_le(mut buf, 0) // Entry
	write_u64_le(mut buf, 0) // Phdr off
	write_u64_le(mut buf, u64(off_shdrs))
	write_u32_le(mut buf, 0) // Flags
	write_u16_le(mut buf, u16(ehdr_size))
	write_u16_le(mut buf, 0) // Phdr entry size
	write_u16_le(mut buf, 0) // Phdr num
	write_u16_le(mut buf, u16(shdr_entry_size))
	write_u16_le(mut buf, u16(num_sections))
	write_u16_le(mut buf, 7) // Shstrndx

	// --- 3. Write Data with Padding ---

	// Pad to Text
	for buf.len < off_text {
		buf << 0
	}
	buf << e.text_data

	// Pad to Data
	for buf.len < off_data {
		buf << 0
	}
	buf << e.data_data

	// Pad to Rodata
	for buf.len < off_rodata {
		buf << 0
	}
	buf << e.rodata

	// Pad to Symtab
	for buf.len < off_symtab {
		buf << 0
	}
	for sym in e.symbols {
		write_u32_le(mut buf, u32(sym.name_idx))
		buf << sym.info
		buf << 0 // other
		write_u16_le(mut buf, sym.shndx)
		write_u64_le(mut buf, sym.value)
		write_u64_le(mut buf, sym.size)
	}

	// Strtab
	for buf.len < off_strtab {
		buf << 0
	}
	buf << e.str_table

	// Rela Text
	for buf.len < off_rela_text {
		buf << 0
	}
	for r in e.text_relocs {
		write_u64_le(mut buf, r.offset)
		write_u64_le(mut buf, r.info)
		write_u64_le(mut buf, u64(r.addend))
	}

	// Shstrtab
	for buf.len < off_shstrtab {
		buf << 0
	}
	buf << e.shstr_table

	// --- 4. Write Section Headers ---
	for buf.len < off_shdrs {
		buf << 0
	}

	// 0: Null
	write_shdr(mut buf, 0, 0, 0, 0, 0, 0, 0, 0, 0)

	// 1: .text
	write_shdr(mut buf, u32(off_text_name), sht_progbits, shf_alloc | shf_execinstr, u64(off_text),
		u64(e.text_data.len), 0, 0, 16, 0)

	// 2: .data
	write_shdr(mut buf, u32(off_data_name), sht_progbits, shf_alloc | shf_write, u64(off_data),
		u64(e.data_data.len), 0, 0, 8, 0)

	// 3: .rodata
	write_shdr(mut buf, u32(off_rodata_name), sht_progbits, shf_alloc, u64(off_rodata),
		u64(e.rodata.len), 0, 0, 4, 0)

	// 4: .symtab (EntSize = 24)
	mut first_global := 1
	for i, s in e.symbols {
		if (s.info >> 4) == 1 { // STB_GLOBAL
			first_global = i
			break
		}
	}
	write_shdr(mut buf, u32(off_symtab_name), sht_symtab, 0, u64(off_symtab), u64(size_symtab),
		5, u32(first_global), 8, 24)

	// 5: .strtab
	write_shdr(mut buf, u32(off_strtab_name), sht_strtab, 0, u64(off_strtab), u64(e.str_table.len),
		0, 0, 1, 0)

	// 6: .rela.text (EntSize = 24)
	write_shdr(mut buf, u32(off_rela_text_name), sht_rela, 0, u64(off_rela_text), u64(size_rela_text),
		4, 1, 8, 24)

	// 7: .shstrtab
	write_shdr(mut buf, u32(off_shstrtab_name), sht_strtab, 0, u64(off_shstrtab), u64(e.shstr_table.len),
		0, 0, 1, 0)

	os.write_file_array(path, buf) or { panic(err) }
}

fn write_shdr(mut b []u8, name u32, type_ u32, flags u64, off u64, size u64, link u32, info u32, align u64, entsize u64) {
	write_u32_le(mut b, name)
	write_u32_le(mut b, type_)
	write_u64_le(mut b, flags)
	write_u64_le(mut b, 0) // Addr
	write_u64_le(mut b, off)
	write_u64_le(mut b, size)
	write_u32_le(mut b, link)
	write_u32_le(mut b, info)
	write_u64_le(mut b, align)
	write_u64_le(mut b, entsize)
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
