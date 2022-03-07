// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

const (
	elf_class32       = 1
	elf_class64       = 2

	elf_data_le       = 1
	elf_data_be       = 2

	elf_version       = 1
	elf_abiversion    = 0

	elf_type_rel      = 1
	elf_type_exec     = 2
	elf_type_dyn      = 3

	elf_amd64         = 0x3e
	elf_arm64         = 0xb7

	elf_osabi_none    = 0
	elf_osabi_hpux    = 1
	elf_osabi_netbsd  = 2
	elf_osabi_linux   = 3
	elf_osabi_freebsd = 9

	elf_header_size   = 0x40
	elf_phentry_size  = 0x38
)

const (
	segment_start = 0x400000
	placeholder   = 0
)

/*
struct Header64 {
	ident     u8  // File identification.
	@type     u16 // File type.
	machine   u16 // Machine architecture.
	version   u32 // ELF format version.
	entry     u64 // Entry point.
	phoff     u64 // Program header file offset.
	shoff     u64 // Section header file offset.
	flags     u32 // Architecture-specific flags.
	ehsize    u16 // Size of ELF header in bytes.
	phentsize u16 // Size of program header entry.
	phnum     u16 // Number of program header entries.
	shentsize u16 // Size of section header entry.
	shnum     u16 // Number of section header entries.
	shstrndx  u16 // Section name strings section.
}
*/

pub fn (mut g Gen) generate_elf_header() {
	elf_type := native.elf_type_dyn // PIE (use _exec for non-relocatable executables)

	g.buf << '\x7fELF'.bytes()
	g.buf << native.elf_class64
	g.buf << native.elf_data_le
	g.buf << native.elf_version
	g.buf << native.elf_osabi_none
	g.write64(0) // abiversion(1)+pad(6)+nident(1)
	g.write16(elf_type)

	if g.pref.arch == .arm64 {
		g.write16(native.elf_arm64)
	} else {
		g.write16(native.elf_amd64)
	}
	g.write32(native.elf_version)
	g.write64(native.segment_start + native.elf_header_size + native.elf_phentry_size) // e_entry
	g.write64(native.elf_header_size) // e_phoff
	g.write64(0) // e_shoff
	g.write32(0) // e_flags
	g.write16(native.elf_header_size)
	g.write16(native.elf_phentry_size)
	g.write16(1) // e_phnum
	g.write16(0) // e_shentsize
	// e_shnum := g.buf.len
	g.write16(0) // e_shnum (number of sections)
	g.write16(0) // e_shstrndx
	// Elf64_Phdr
	g.write32(1) // p_type
	g.write32(5) // p_flags
	g.write64(0) // p_offset
	g.write64(native.segment_start) // p_vaddr addr:050
	g.write64(native.segment_start) //
	g.file_size_pos = i64(g.buf.len)
	g.write64(0) // p_filesz PLACEHOLDER, set to file_size later // addr: 060
	g.write64(0) // p_memsz
	g.write64(0x1000) // p_align
	// write sections
	/*
	sections := []string{}
	g.write16_at(e_shnum, sections.len) // e_shnum (number of sections)

	for section in sections {
		// write section data
		println('section $section')
	}
	*/
	// user code starts here at
	// address: 00070 and a half
	if g.pref.is_verbose {
		eprintln('code_start_pos = $g.buf.len.hex()')
	}
	g.code_start_pos = i64(g.buf.len)
	g.debug_pos = g.buf.len
	g.call(native.placeholder) // call main function, it's not guaranteed to be the first, we don't know its address yet
	g.println('; call fn main')
}

fn (mut g Gen) elf_string_table() {
	for _, s in g.strs {
		match s.typ {
			.abs64 {
				// g.write64_at(native.segment_start + g.buf.len, int(g.str_pos[i]))
				g.write64_at(s.pos, g.buf.len)
			}
			.rel32 {
				g.write32_at(s.pos, g.buf.len - s.pos - 4)
			}
			else {
				g.n_error('unsupported string reloc type')
			}
		}
		g.write_string(s.str)
	}
}

pub fn (mut g Gen) generate_elf_footer() {
	g.elf_string_table()
	// file_size holds the address at the end of the code and const strings table
	file_size := g.buf.len
	g.write64_at(g.file_size_pos, file_size) // set file size 64 bit value
	g.write64_at(g.file_size_pos + 8, file_size)
	if g.pref.arch == .arm64 {
		bl_next := u32(0x94000001)
		g.write32_at(g.code_start_pos, int(bl_next))
	} else {
		// amd64
		// call main function, it's not guaranteed to be the first
		// we generated call(0) ("e8 0")
		// now need to replace "0" with a relative address of the main function
		// +1 is for "e8"
		// -5 is for "e8 00 00 00 00"
		g.write32_at(g.code_start_pos + 1, int(g.main_fn_addr - g.code_start_pos) - 5)
	}
	g.create_executable()
}
