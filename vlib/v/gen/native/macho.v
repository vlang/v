// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

const (
	s_attr_some_instructions = 0x00000400
	s_attr_pure_instructions = 0x80000000
	s_attr_ext_reloc         = 0x00000200
	s_attr_loc_reloc         = 0x00000100
	//
	macho_symcmd_size        = 0x18
	macho_d_size             = 0x50
	mh_object                = 1
	mh_execute               = 2
	lc_dysymtab              = 0xb
	lc_load_dylib            = 0xc
	lc_load_dylinker         = 0xe
	lc_main                  = 0x80000028
	lc_segment_64            = 0x19
	lc_symtab                = 0x2
)

struct Symbol {
	str_entry  int
	symbol_typ int
	section    int
	desc       int
	val        i64
	name       string
	is_ext     bool
}

struct Reloc {
	addr  int
	pcrel int
	len   int
	ext   int
	typ   int
	snum  int // symbol index (if ext) or infile section number
}

fn (mut g Gen) macho_segment64_pagezero() {
	g.write32(native.lc_segment_64) // LC_SEGMENT_64
	g.write32(72) // cmdsize
	g.write_string_with_padding('__PAGEZERO', 16) // section name
	g.write64(0) // vmaddr
	g.write64(0x1000) // vmsize
	g.write64(0) // fileoff
	g.write64(0) // filesize

	g.write32(0) // maxprot
	g.write32(0) // initprot
	g.write32(0) // nsects
	g.write32(0) // flags
}

fn (mut g Gen) macho_segment64_linkedit() {
	g.write32(native.lc_segment_64)
	g.write32(0x48) // cmdsize
	g.write_string_with_padding('__LINKEDIT', 16)

	g.write64(0x3000) // vmaddr
	g.write64(0x1000) // vmsize
	g.write64(0x1000) // fileoff
	g.write64(0) // filesize
	g.write32(7) // maxprot
	g.write32(3) // initprot
	g.write32(0) // nsects
	g.write32(0) // flags
}

fn (mut g Gen) macho_header(ncmds int, bintype int) int {
	g.write32(0xfeedfacf) // MH_MAGIC_64
	if g.pref.arch == .arm64 {
		g.write32(0x0100000c) // CPU_TYPE_ARM64
		g.write32(0x00000000) // CPU_SUBTYPE_ARM64_ALL
	} else {
		g.write32(0x01000007) // CPU_TYPE_X64
		g.write32(0x80000003) // CPU_SUBTYPE_X64
	}
	g.write32(native.mh_execute) // filetype
	g.write32(ncmds) // ncmds

	cmdsize_offset := g.buf.len
	g.write32(0) // size of load commands

	g.write32(0) // flags
	g.write32(0) // reserved
	return cmdsize_offset
}

fn (mut g Gen) macho_segment64_text() []int {
	mut patch := []int{}
	g.write32(native.lc_segment_64) // LC_SEGMENT_64
	g.write32(152) // 152
	g.write_string_with_padding('__TEXT', 16) // section name
	g.write64(0x100000000) // vmaddr
	patch << g.buf.len
	g.write64(0x00001000) // + codesize) // vmsize
	g.write64(0x00000000) // filesize
	patch << g.buf.len
	g.write64(0x00001000) // + codesize) // filesize

	g.write32(7) // maxprot
	g.write32(5) // initprot
	g.write32(1) // nsects
	g.write32(0) // flags

	g.write_string_with_padding('__text', 16) // section name
	g.write_string_with_padding('__TEXT', 16) // segment name
	g.write64(0x0000000100001000) // vmaddr
	patch << g.buf.len
	g.write64(0) // vmsize
	g.write32(4096) // offset
	g.write32(0) // align

	g.write32(0) // reloff
	g.write32(0) // nreloc

	g.write32(0) // flags
	g.write32(0)

	g.write32(0) // reserved1
	g.write32(0) // reserved2
	return patch
}

fn (mut g Gen) macho_symtab() {
	g.write32(native.lc_symtab)
	g.write32(24)
	g.write32(0x1000)
	g.write32(0)
	g.write32(0x1000)
	g.write32(0)

	// lc_dysymtab
	g.write32(native.lc_dysymtab)
	g.write32(0x50)
	g.write32(0) // ilocalsym
	g.write32(0) // nlocalsym
	g.write32(0) // iextdefsym
	g.write32(0) // nextdefsym
	g.write32(0) // iundefsym
	g.write32(0) // nundefsym
	g.write32(0) // tocoff
	g.write32(0) // ntoc
	g.write32(0) // modtaboff
	g.write32(0) // nmodtab
	g.write32(0) // extrefsymoff
	g.write32(0) // nextrefsyms
	g.write32(0) // indirectsymoff
	g.write32(0) // nindirectsyms
	g.write32(0) // extreloff
	g.write32(0) // nextrel
	g.write32(0) // locreloff
	g.write32(0) // nlocrel
}

fn (mut g Gen) macho_dylibs() {
	g.write32(native.lc_load_dylinker)
	g.write32(32) // cmdsize (must be aligned to int32)
	g.write32(12) // offset
	g.write_string_with_padding('/usr/lib/dyld', 16)
	g.write32(0) // padding // can be removed

	g.write32(native.lc_load_dylib)
	g.write32(56) // cmdsize
	g.write32(24) // offset
	g.write32(0) // ts
	g.write32(1) // ver
	g.write32(1) // compat
	g.write_string_with_padding('/usr/lib/libSystem.B.dylib', 32)
}

fn (mut g Gen) macho_main(addr int) {
	g.write32(int(native.lc_main)) // LC_MAIN
	g.write32(24) // cmdsize
	g.write32(addr) // entrypoint
	g.write32(0) // initial_stacksize
}

pub fn (mut g Gen) generate_macho_header() {
	g.code_start_pos = 0x1000
	g.debug_pos = 0x1000
	cmdsize_offset := g.macho_header(8, native.mh_execute)
	g.macho_segment64_pagezero()

	g.size_pos = g.macho_segment64_text()
	g.macho_segment64_linkedit()
	g.macho_symtab()
	g.macho_dylibs()
	g.macho_main(0x1000)

	g.write32_at(cmdsize_offset, g.buf.len - 24)
	g.write_nulls(0x1000 - g.buf.len)
	g.call(0)
}

fn (mut g Gen) write_nulls(len int) {
	pad := 0x1000 - g.buf.len
	for _ in 0 .. pad {
		g.write8(0)
	}
}

pub fn (mut g Gen) generate_macho_object_header() {
	if g.pref.arch == .arm64 {
		g.write32(0xfeedfacf) // MH_MAGIC_64
		g.write32(0x0100000c) // CPU_TYPE_ARM64
		g.write32(0x00000000) // CPU_SUBTYPE_ARM64_ALL
	} else {
		g.write32(0xfeedfacf) // MH_MAGIC_64
		g.write32(0x01000007) // CPU_TYPE_X64
		g.write32(0x00000003) // CPU_SUBTYPE_X64
	}
	g.write32(native.mh_object) // MH_OBJECT
	text_offset := 0x138
	g.write32(4) // # of load commands
	g.write32(text_offset - 0x20) // size of load commands // 0x138-0x20
	// g.write32(0x00002000) // MH_SUBSECTIONS_VIA_SYMBOLS
	g.write32(0) // MH_SUBSECTIONS_VIA_SYMBOLS
	g.write32(0) // reserved
	////
	g.write32(0x19) // LC_SEGMENT_64
	g.write32(0x98) // command size
	g.zeroes(16) // segment name
	g.write64(0) // VM address
	g.write64(0x25) // VM size
	g.write64(text_offset) // file offset
	g.write64(0x25) // file size
	g.write32(0x7) // max vm protection
	g.write32(0x7) // initial vm protection
	g.write32(0x1) // # of sections
	g.write32(0) // flags
	////
	g.write_string_with_padding('__text', 16) // section name
	g.write_string_with_padding('__TEXT', 16) // segment name
	g.write64(0) // address
	g.write64(0x25) // size
	g.write32(text_offset) // offset
	g.write32(0x4) // alignment
	g.write32(0x160) // relocation offset
	g.write32(0x1) // # of relocations
	g.write32(int(native.s_attr_some_instructions | native.s_attr_pure_instructions))
	g.write32(0)
	g.write32(0)
	g.write32(0)
	/// ???
	g.write32(0x32)
	g.write32(0x18)

	g.write32(0x01)
	g.write32(0x000a0000) // minOS 10.0
	g.write32(0)
	g.write32(0)
	// lc_symtab
	g.sym_table_command()
	//
	g.write32(native.lc_dysymtab)
	g.write32(native.macho_d_size)
	g.write32(0)
	g.write32(2)
	g.write32(2)
	g.write32(1)
	g.write32(3)
	g.write32(1)
	for _ in 0 .. 12 {
		g.write32(0)
	}
	if g.pref.is_verbose {
		println('commands size = $g.buf.len')
		if g.buf.len != 0x138 {
			println('macho: invalid header size')
		}
	}

	if g.pref.arch == .arm64 {
		g.gen_arm64_helloworld()
	} else {
		// do nothing
	}
}

pub fn (mut g Gen) generate_macho_footer() {
	codesize := g.buf.len - 0x1000
	g.write_relocs()
	g.sym_table()
	stringtablesize := g.sym_string_table()
	delta := codesize + stringtablesize + 12 // code_offset_end - 0x1000// + stringtablesize
	g.write8(0)
	for o in g.size_pos {
		n := g.read32_at(o)
		g.write32_at(o, n + delta)
	}
	g.write64(0)
	// this is amd64-specific
	call_delta := int(g.main_fn_addr - g.code_start_pos) - 5
	g.write32_at(g.code_start_pos + 1, call_delta)
	g.create_executable()
}

fn (mut g Gen) sym_table_command() {
	g.syms << Symbol{
		str_entry: 0x19
		symbol_typ: 0xe
		section: 1
		val: 0
		name: '_start'
		is_ext: true
	}
	g.syms << Symbol{
		str_entry: 0x0e
		symbol_typ: 0xe
		// symbol_typ: SYM_DEF
		section: 1
		val: 0x18
		name: '_puts'
		is_ext: false
	}
	g.syms << Symbol{
		str_entry: 0x01
		symbol_typ: 0xf
		// symbol_typ: SYM_DEF
		section: 1
		// val: 0x27
		val: 0
		name: 'helloworld'
		is_ext: false
	}
	g.syms << Symbol{
		str_entry: 0x08
		symbol_typ: 0x1
		// symbol_typ: SYM_DEF
		section: 0
		// val: 0x27
		val: 0
		name: 'ltmp1'
		is_ext: false
	}
	g.write32(native.lc_symtab)
	g.write32(native.macho_symcmd_size)
	sym_table_offset := 0x168
	g.write32(sym_table_offset)
	g_syms_len := 4
	g.write32(g_syms_len)
	str_offset := 0x1a8
	g.write32(str_offset)
	str_size := 0x20
	g.write32(str_size)
}

pub fn (mut g Gen) zeroes(n int) {
	for _ in 0 .. n {
		g.buf << 0
	}
}

fn (mut g Gen) write_relocs() {
	if g.pref.is_verbose {
		println('relocs at $g.buf.len should be 0x160')
	}
	g.write32(0x8)
	g.write32(0x2d000003)
}

fn (mut g Gen) sym_table() {
	// strings first
	for sym in g.syms {
		// if !sym.is_ext {
		g.write_symbol(sym)
		//}
	}
	// now fns  (external syms)
	/*
	for sym in g.syms {
		if sym.is_ext {
			g.write_symbol(sym)
		}
	}
	*/
}

fn (mut g Gen) write_symbol(s Symbol) {
	// g.write8(0x77)
	g.write32(s.str_entry)
	g.write8(s.symbol_typ)
	g.write8(s.section)
	g.write8(0)
	g.write8(0)
	g.write64(s.val)
	// g.write16(s.desc)
}

fn (mut g Gen) sym_string_table() int {
	begin := g.buf.len
	g.zeroes(1)
	for _, s in g.strs {
		pos := g.buf.len - s.pos - 4
		match s.typ {
			.rel32 {
				g.write32_at(s.pos, pos)
			}
			else {
				if g.pref.os == .windows {
					// that should be .rel32, not windows-specific
					g.write32_at(s.pos, pos)
				} else {
					baddr := i64(0x100000000)
					g.write64_at(s.pos, g.buf.len + baddr)
				}
			}
		}
		g.write_string(s.str)
	}
	return g.buf.len - begin
}
