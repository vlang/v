// Copyright (c) 2026. Big-endian ELF64 relocatable object writer (for s390x).

module elf

import os

// Big-endian ELF64 writer. Reuses ElfObject struct but writes BE format.

// write_be serializes the object to a big-endian ELF64 relocatable file.
pub fn (mut e ElfObject) write_be(path string) {
	mut buf := []u8{}

	// --- 1. Prepare Sections ---
	off_text_name := e.add_sh_string('.text')
	off_data_name := e.add_sh_string('.data')
	off_rodata_name := e.add_sh_string('.rodata')
	off_symtab_name := e.add_sh_string('.symtab')
	off_strtab_name := e.add_sh_string('.strtab')
	off_rela_text_name := e.add_sh_string('.rela.text')
	off_shstrtab_name := e.add_sh_string('.shstrtab')
	off_gnu_stack_name := e.add_sh_string('.note.GNU-stack')

	ehdr_size := 64
	shdr_entry_size := 64
	num_sections := 9

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
	buf << u8(0x7f)
	buf << u8(0x45) // 'E'
	buf << u8(0x4c) // 'L'
	buf << u8(0x46) // 'F'
	buf << u8(2) // ELFCLASS64
	buf << u8(2) // ELFDATA2MSB (big-endian)
	buf << u8(1) // EV_CURRENT
	buf << u8(0) // ABI System V
	buf << u8(0) // ABI Version
	for _ in 0 .. 7 {
		buf << 0
	}

	// Helper: write big-endian
	// e_type (ET_REL = 1)
	buf << u8(0)
	buf << u8(1)
	// e_machine
	buf << u8(e.machine >> 8)
	buf << u8(e.machine)
	// e_version
	buf << u8(0)
	buf << u8(0)
	buf << u8(0)
	buf << u8(1)
	// e_entry (0)
	for _ in 0 .. 8 {
		buf << 0
	}
	// e_phoff (0)
	for _ in 0 .. 8 {
		buf << 0
	}
	// e_shoff
	write_u64_be(mut buf, u64(off_shdrs))
	// e_flags
	write_u32_be(mut buf, e.elf_flags)
	// e_ehsize
	buf << u8(0)
	buf << u8(64)
	// e_phentsize (0)
	buf << u8(0)
	buf << u8(0)
	// e_phnum (0)
	buf << u8(0)
	buf << u8(0)
	// e_shentsize (64)
	buf << u8(0)
	buf << u8(64)
	// e_shnum
	buf << u8(0)
	buf << u8(num_sections)
	// e_shstrndx (7)
	buf << u8(0)
	buf << u8(7)

	// --- 3. Write Data with Padding ---

	for buf.len < off_text {
		buf << 0
	}
	buf << e.text_data

	for buf.len < off_data {
		buf << 0
	}
	buf << e.data_data

	for buf.len < off_rodata {
		buf << 0
	}
	buf << e.rodata

	for buf.len < off_symtab {
		buf << 0
	}
	for sym in e.symbols {
		write_u32_be(mut buf, u32(sym.name_idx))
		buf << sym.info
		buf << 0 // other
		write_u16_be(mut buf, sym.shndx)
		write_u64_be(mut buf, sym.value)
		write_u64_be(mut buf, sym.size)
	}

	for buf.len < off_strtab {
		buf << 0
	}
	buf << e.str_table

	for buf.len < off_rela_text {
		buf << 0
	}
	for r in e.text_relocs {
		write_u64_be(mut buf, r.offset)
		write_u64_be(mut buf, r.info)
		write_u64_be(mut buf, u64(r.addend))
	}

	for buf.len < off_shstrtab {
		buf << 0
	}
	buf << e.shstr_table

	for buf.len < off_shdrs {
		buf << 0
	}

	// 0: Null
	write_shdr_be(mut buf, 0, 0, 0, 0, 0, 0, 0, 0, 0)

	// 1: .text
	write_shdr_be(mut buf, u32(off_text_name), sht_progbits, shf_alloc | shf_execinstr, u64(off_text),
		u64(e.text_data.len), 0, 0, 16, 0)

	// 2: .data
	write_shdr_be(mut buf, u32(off_data_name), sht_progbits, shf_alloc | shf_write, u64(off_data),
		u64(e.data_data.len), 0, 0, 8, 0)

	// 3: .rodata
	write_shdr_be(mut buf, u32(off_rodata_name), sht_progbits, shf_alloc, u64(off_rodata),
		u64(e.rodata.len), 0, 0, 4, 0)

	// 4: .symtab (EntSize = 24)
	mut first_global := 1
	for i, s in e.symbols {
		if (s.info >> 4) == 1 {
			first_global = i
			break
		}
	}
	write_shdr_be(mut buf, u32(off_symtab_name), sht_symtab, 0, u64(off_symtab), u64(size_symtab),
		5, u32(first_global), 8, 24)

	// 5: .strtab
	write_shdr_be(mut buf, u32(off_strtab_name), sht_strtab, 0, u64(off_strtab), u64(e.str_table.len),
		0, 0, 1, 0)

	// 6: .rela.text (EntSize = 24)
	write_shdr_be(mut buf, u32(off_rela_text_name), sht_rela, 0, u64(off_rela_text), u64(size_rela_text),
		4, 1, 8, 24)

	// 7: .shstrtab
	write_shdr_be(mut buf, u32(off_shstrtab_name), sht_strtab, 0, u64(off_shstrtab), u64(e.shstr_table.len),
		0, 0, 1, 0)

	// 8: .note.GNU-stack
	write_shdr_be(mut buf, u32(off_gnu_stack_name), sht_progbits, 0, 0, 0, 0, 0, 1, 0)

	os.write_file_array(path, buf) or { panic(err) }
}

fn write_shdr_be(mut b []u8, name u32, type_ u32, flags u64, off u64, size u64, link u32, info u32, align u64, entsize u64) {
	write_u32_be(mut b, name)
	write_u32_be(mut b, type_)
	write_u64_be(mut b, flags)
	write_u64_be(mut b, 0) // Addr
	write_u64_be(mut b, off)
	write_u64_be(mut b, size)
	write_u32_be(mut b, link)
	write_u32_be(mut b, info)
	write_u64_be(mut b, align)
	write_u64_be(mut b, entsize)
}

fn write_u32_be(mut b []u8, v u32) {
	b << u8(v >> 24)
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
}

fn write_u64_be(mut b []u8, v u64) {
	b << u8(v >> 56)
	b << u8(v >> 48)
	b << u8(v >> 40)
	b << u8(v >> 32)
	b << u8(v >> 24)
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
}

fn write_u16_be(mut b []u8, v u16) {
	b << u8(v >> 8)
	b << u8(v)
}
