// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module x64

import os

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
	elf_osabi         = 0
	et_rel            = 1
	et_exec           = 2
	et_dyn            = 3
	e_machine_amd64   = 0x3e
	e_machine_aarch64 = 183
	shn_xindex        = 0xffff
	sht_null          = 0
)

const (
	segment_start = 0x400000
	placeholder   = 0
	sevens        = 0x77777777
)

pub fn (mut g Gen) generate_elf_header() {
	g.buf << [byte(x64.mag0), x64.mag1, x64.mag2, x64.mag3]
	g.buf << x64.elfclass64 // file class
	g.buf << x64.elfdata2lsb // data encoding
	g.buf << x64.ev_current // file version
	g.buf << 1 // elf_osabi
	g.write64(0) // et_rel) // et_rel for .o
	g.write16(2) // e_type
	if g.pref.arch == .aarch64 {
		g.write16(x64.e_machine_aarch64)
	} else {
		g.write16(x64.e_machine_amd64)
	}
	g.write32(x64.ev_current) // e_version
	eh_size := 0x40
	phent_size := 0x38
	g.write64(x64.segment_start + eh_size + phent_size) // e_entry
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
	g.write64(x64.segment_start) // p_vaddr addr:050
	g.write64(x64.segment_start) //
	g.file_size_pos = i64(g.buf.len)
	g.write64(0) // p_filesz PLACEHOLDER, set to file_size later // addr: 060
	g.write64(0) // p_memsz
	g.write64(0x1000) // p_align
	// user code starts here at
	// address: 00070 and a half
	println('code_start_pos = $g.buf.len.hex()')
	g.code_start_pos = i64(g.buf.len)
	g.debug_pos = g.buf.len
	g.call(x64.placeholder) // call main function, it's not guaranteed to be the first, we don't know its address yet
	g.println('call fn main')
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
		g.write64_at(x64.segment_start + g.buf.len, int(g.str_pos[i]))
		g.write_string(s)
		g.write8(0)
	}
	// Now we know the file size, set it
	file_size := g.buf.len
	g.write64_at(file_size, g.file_size_pos) // set file size 64 bit value
	g.write64_at(file_size, g.file_size_pos + 8)
	// call main function, it's not guaranteed to be the first
	// we generated call(0) ("e8 0")
	// now need to replace "0" with a relative address of the main function
	// +1 is for "e8"
	// -5 is for "e8 00 00 00 00"
	g.write32_at(g.code_start_pos + 1, int(g.main_fn_addr - g.code_start_pos) - 5)
	// Create the binary
	mut f := os.create(g.out_name) or { panic(err) }
	os.chmod(g.out_name, 0o775) // make it an executable
	unsafe { f.write_ptr(g.buf.data, g.buf.len) }
	f.close()
	if g.pref.is_verbose {
		println('\nx64 elf binary has been successfully generated')
	}
}
