// Copyright (c) 2026. Minimal ELF32 relocatable object writer.

module elf

import os

// ELF32 Constants
const elfclass32 = 1
const elfdata2lsb32 = 1

// ElfObject32 is a minimal ELF32 relocatable object.
// Reuses the same ElfObject struct but writes 32-bit format.
pub struct ElfObject32 {
	machine u16
pub mut:
	text_data []u8
	data_data []u8
	rodata    []u8

	elf_flags u32

	symbols        []ElfSymbol32
	symbol_by_name map[string]int
	str_table      []u8

	text_relocs []ElfRel32

	shstr_table []u8
}

pub struct ElfSymbol32 {
pub mut:
	name     string
	name_idx int
	info     u8
	shndx    u16
	value    u32
	size     u32
}

pub struct ElfRel32 {
pub mut:
	offset u32
	info   u32
	addend i32
}

pub fn ElfObject32.new(machine u16) &ElfObject32 {
	mut e := &ElfObject32{
		machine:        machine
		symbol_by_name: map[string]int{}
		str_table:      [u8(0)]
		shstr_table:    [u8(0)]
	}
	e.symbols << ElfSymbol32{
		name:  ''
		shndx: 0
	}
	return e
}

pub fn (mut e ElfObject32) add_symbol(name string, value u32, is_func bool, shndx u16) int {
	type_ := if is_func { u8(2) } else { u8(1) }
	bind := u8(1)
	info := (bind << 4) | type_

	if existing := e.symbol_by_name[name] {
		e.symbols[existing].info = info
		e.symbols[existing].shndx = shndx
		e.symbols[existing].value = value
		return existing
	}

	idx := e.symbols.len
	name_off := e.add_string(name)

	e.symbols << ElfSymbol32{
		name:     name
		name_idx: name_off
		info:     info
		shndx:    shndx
		value:    value
	}
	e.symbol_by_name[name] = idx
	return idx
}

pub fn (mut e ElfObject32) add_undefined(name string) int {
	if existing := e.symbol_by_name[name] {
		return existing
	}

	idx := e.symbols.len
	name_off := e.add_string(name)
	info := u8(0x10)

	e.symbols << ElfSymbol32{
		name:     name
		name_idx: name_off
		info:     info
		shndx:    0
		value:    0
	}
	e.symbol_by_name[name] = idx
	return idx
}

pub fn (mut e ElfObject32) add_text_reloc(offset u32, sym_idx int, type_ int, addend i32) {
	info := (u32(sym_idx) << 8) | u32(type_)
	e.text_relocs << ElfRel32{
		offset: offset
		info:   info
		addend: addend
	}
}

fn (mut e ElfObject32) add_string(s string) int {
	off := e.str_table.len
	e.str_table << s.bytes()
	e.str_table << 0
	return off
}

fn (mut e ElfObject32) add_sh_string(s string) int {
	off := e.shstr_table.len
	e.shstr_table << s.bytes()
	e.shstr_table << 0
	return off
}

pub fn (mut e ElfObject32) write(path string) {
	mut buf := []u8{}

	// Section names
	off_text_name := e.add_sh_string('.text')
	off_data_name := e.add_sh_string('.data')
	off_rodata_name := e.add_sh_string('.rodata')
	off_symtab_name := e.add_sh_string('.symtab')
	off_strtab_name := e.add_sh_string('.strtab')
	off_rela_text_name := e.add_sh_string('.rel.text')
	off_shstrtab_name := e.add_sh_string('.shstrtab')
	off_gnu_stack_name := e.add_sh_string('.note.GNU-stack')

	ehdr_size := 52
	shdr_entry_size := 40
	num_sections := 9

	mut current_offset := ehdr_size

	// .text (Align 4)
	if current_offset % 4 != 0 {
		current_offset += (4 - (current_offset % 4))
	}
	off_text := current_offset
	current_offset += e.text_data.len

	// .data (Align 4)
	if current_offset % 4 != 0 {
		current_offset += (4 - (current_offset % 4))
	}
	off_data := current_offset
	current_offset += e.data_data.len

	// .rodata (Align 4)
	if current_offset % 4 != 0 {
		current_offset += (4 - (current_offset % 4))
	}
	off_rodata := current_offset
	current_offset += e.rodata.len

	// .symtab (Align 4)
	if current_offset % 4 != 0 {
		current_offset += (4 - (current_offset % 4))
	}
	off_symtab := current_offset
	size_symtab := e.symbols.len * 16
	current_offset += size_symtab

	// .strtab (Align 1)
	off_strtab := current_offset
	current_offset += e.str_table.len

	// .rel.text (Align 4)
	if current_offset % 4 != 0 {
		current_offset += (4 - (current_offset % 4))
	}
	off_rela_text := current_offset
	size_rela_text := e.text_relocs.len * 8
	current_offset += size_rela_text

	// .shstrtab (Align 1)
	off_shstrtab := current_offset
	current_offset += e.shstr_table.len

	// Section Headers (Align 4)
	if current_offset % 4 != 0 {
		current_offset += (4 - (current_offset % 4))
	}
	off_shdrs := current_offset

	// --- Write ELF Header (52 bytes) ---
	buf << u8(0x7f)
	buf << u8(0x45) // 'E'
	buf << u8(0x4c) // 'L'
	buf << u8(0x46) // 'F'
	buf << u8(elfclass32)
	buf << u8(elfdata2lsb32)
	buf << u8(1) // EV_CURRENT
	buf << u8(0) // ABI System V
	for _ in 0 .. 8 {
		buf << 0
	}

	// e_type (ET_REL = 1)
	buf << u8(1)
	buf << u8(0)
	// e_machine
	buf << u8(e.machine)
	buf << u8(e.machine >> 8)
	// e_version
	buf << u8(1)
	buf << u8(0)
	buf << u8(0)
	buf << u8(0)
	// e_entry (0)
	for _ in 0 .. 4 {
		buf << 0
	}
	// e_phoff (0)
	for _ in 0 .. 4 {
		buf << 0
	}
	// e_shoff
	write_u32_le(mut buf, u32(off_shdrs))
	// e_flags
	write_u32_le(mut buf, e.elf_flags)
	// e_ehsize
	buf << u8(52)
	buf << u8(0)
	// e_phentsize (0)
	buf << u8(0)
	buf << u8(0)
	// e_phnum (0)
	buf << u8(0)
	buf << u8(0)
	// e_shentsize (40)
	buf << u8(40)
	buf << u8(0)
	// e_shnum
	buf << u8(num_sections)
	buf << u8(0)
	// e_shstrndx (7)
	buf << u8(7)
	buf << u8(0)

	// --- Write Data with Padding ---

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
		write_u32_le(mut buf, sym.value)
		write_u32_le(mut buf, sym.size)
		buf << sym.info
		buf << 0 // other
		write_u16_le(mut buf, sym.shndx)
	}

	// Strtab
	for buf.len < off_strtab {
		buf << 0
	}
	buf << e.str_table

	// Rel.text
	for buf.len < off_rela_text {
		buf << 0
	}
	for r in e.text_relocs {
		write_u32_le(mut buf, r.offset)
		write_u32_le(mut buf, r.info)
		write_u32_le(mut buf, u32(r.addend))
	}

	// Shstrtab
	for buf.len < off_shstrtab {
		buf << 0
	}
	buf << e.shstr_table

	// Section Headers
	for buf.len < off_shdrs {
		buf << 0
	}

	// 0: Null
	write_shdr32(mut buf, 0, 0, 0, 0, 0, 0, 0, 0, 0)

	// 1: .text
	write_shdr32(mut buf, u32(off_text_name), sht_progbits, shf_alloc | shf_execinstr, u32(off_text),
		u32(e.text_data.len), 0, 0, 4, 0)

	// 2: .data
	write_shdr32(mut buf, u32(off_data_name), sht_progbits, shf_alloc | shf_write, u32(off_data),
		u32(e.data_data.len), 0, 0, 4, 0)

	// 3: .rodata
	write_shdr32(mut buf, u32(off_rodata_name), sht_progbits, shf_alloc, u32(off_rodata),
		u32(e.rodata.len), 0, 0, 4, 0)

	// 4: .symtab (EntSize = 16)
	mut first_global := 1
	for i, s in e.symbols {
		if (s.info >> 4) == 1 {
			first_global = i
			break
		}
	}
	write_shdr32(mut buf, u32(off_symtab_name), sht_symtab, 0, u32(off_symtab), u32(size_symtab),
		5, u32(first_global), 4, 16)

	// 5: .strtab
	write_shdr32(mut buf, u32(off_strtab_name), sht_strtab, 0, u32(off_strtab), u32(e.str_table.len),
		0, 0, 1, 0)

	// 6: .rela.text (EntSize = 8)
	write_shdr32(mut buf, u32(off_rela_text_name), 4, 0, u32(off_rela_text), u32(size_rela_text),
		4, 1, 4, 12)

	// 7: .shstrtab
	write_shdr32(mut buf, u32(off_shstrtab_name), sht_strtab, 0, u32(off_shstrtab), u32(e.shstr_table.len),
		0, 0, 1, 0)

	// 8: .note.GNU-stack
	write_shdr32(mut buf, u32(off_gnu_stack_name), sht_progbits, 0, 0, 0, 0, 0, 1, 0)

	os.write_file_array(path, buf) or { panic(err) }
}

fn write_shdr32(mut b []u8, name u32, type_ u32, flags u32, off u32, size u32, link u32, info u32, align u32, entsize u32) {
	write_u32_le(mut b, name)
	write_u32_le(mut b, type_)
	write_u32_le(mut b, flags)
	write_u32_le(mut b, 0) // Addr
	write_u32_le(mut b, off)
	write_u32_le(mut b, size)
	write_u32_le(mut b, link)
	write_u32_le(mut b, info)
	write_u32_le(mut b, align)
	write_u32_le(mut b, entsize)
}
