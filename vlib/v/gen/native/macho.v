// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
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
	lc_symtab                = 0x2
	lc_dymsymtab             = 0xB
	mh_object                = 1
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

pub fn (mut g Gen) generate_macho_header() {
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
	g.write32(native.s_attr_some_instructions | native.s_attr_pure_instructions)
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
	g.write32(native.lc_dymsymtab)
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
	g.write_relocs()
	g.sym_table()
	g.sym_string_table()
	g.write8(0)
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

fn (mut g Gen) sym_string_table() {
	g.zeroes(1)
	for sym in g.syms {
		g.write_string(sym.name)
		g.write8(0)
	}
}
