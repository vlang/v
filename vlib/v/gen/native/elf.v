// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

const (
	mag0        = byte(0x7f)
	mag1        = `E`
	mag2        = `L`
	mag3        = `F`
	ei_class    = 4
	elfclass64  = 2
	elfdata2lsb = 1
	ev_current  = 1
)

// ELF file types
const (
	elf_osabi       = 0
	et_rel          = 1
	et_exec         = 2
	et_dyn          = 3
	e_machine_amd64 = 0x3e
	e_machine_arm64 = 0xb7
	shn_xindex      = 0xffff
	sht_null        = 0
)

const (
	segment_start = 0x400000
	placeholder   = 0
	sevens        = 0x77777777
)

pub fn (mut g Gen) generate_elf_header() {
	g.buf << [byte(native.mag0), native.mag1, native.mag2, native.mag3]
	g.buf << native.elfclass64 // file class
	g.buf << native.elfdata2lsb // data encoding
	g.buf << native.ev_current // file version
	g.buf << native.elf_osabi
	g.write64(0) // et_rel) // et_rel for .o
	g.write16(2) // e_type
	if g.pref.arch == .arm64 {
		g.write16(native.e_machine_arm64)
	} else {
		g.write16(native.e_machine_amd64)
	}
	g.write32(native.ev_current) // e_version
	eh_size := 0x40
	phent_size := 0x38
	g.write64(native.segment_start + eh_size + phent_size) // e_entry
	g.write64(0x40) // e_phoff
	g.write64(0) // e_shoff
	g.write32(0) // e_flags
	g.write16(eh_size) // e_ehsize
	g.write16(phent_size) // e_phentsize
	g.write16(1) // e_phnum
	g.write16(0) // e_shentsize
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

pub fn (mut g Gen) generate_elf_footer() {
	// Return 0
	/*
	g.mov(.edi, 0) // ret value
	g.mov(.eax, 60)
	g.syscall()
	*/
	// Strings table
	// Loop thru all strings and set the right addresses
	for i, s in g.strings {
		g.write64_at(native.segment_start + g.buf.len, int(g.str_pos[i]))
		g.write_string(s)
		g.write8(0)
	}
	// Now we know the file size, set it
	file_size := g.buf.len
	g.write64_at(file_size, g.file_size_pos) // set file size 64 bit value
	g.write64_at(file_size, g.file_size_pos + 8)
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
