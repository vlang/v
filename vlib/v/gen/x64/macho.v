// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module x64

import os

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
	g.write32(0xfeedfacf) // MH_MAGIC_64
	g.write32(0x0100000c) // CPU_TYPE_ARM64
	g.write32(0x00000000) // CPU_SUBTYPE_ARM64_ALL
	g.write32(0x00000001) // MH_OBJECT
	g.write32(0x00000004) // # of load commands
	g.write32(0x118) // size of load commands
	// g.write32(0x00002000) // MH_SUBSECTIONS_VIA_SYMBOLS
	g.write32(0) // MH_SUBSECTIONS_VIA_SYMBOLS
	g.write32(0) // reserved
	////
	g.write32(0x19) // LC_SEGMENT_64
	g.write32(0x98) // command size
	g.zeroes(16) // segment name
	g.write64(0) // VM address
	g.write64(0x25) // VM size
	g.write64(0x138) // file offset
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
	g.write32(0x138) // offset
	g.write32(0x4) // alignment
	g.write32(0x160) // relocation offset
	g.write32(0x1) // # of relocations
	g.write32(x64.s_attr_some_instructions | x64.s_attr_pure_instructions)
	g.write32(0)
	g.write32(0)
	g.write32(0)
	/// ???
	g.write32(0x32)
	g.write32(0x18)

	g.write32(0x01)
	g.write32(0x000b0000)
	g.write32(0)
	g.write32(0)
	// lc_symtab
	g.sym_table_command()
	//
	g.write32(x64.lc_dymsymtab)
	g.write32(x64.macho_d_size)
	g.write32(0)
	g.write32(2)
	g.write32(2)
	g.write32(1)
	g.write32(3)
	g.write32(1)
	for _ in 0 .. 12 {
		g.write32(0)
	}
	// g.write32(0x77777777)
	// assembly
	g.mov_arm(.x0, 1)
	g.adr()
	g.bl()
	g.mov_arm(.x0, 0)
	g.mov_arm(.x16, 1)
	g.svc()
	//
	g.write_string('Hello WorlD!\n')
	g.write8(0) // padding?
	g.write8(0)
	g.write8(0)
	g.write_relocs()
	g.sym_table()
	g.sym_string_table()
	g.write8(0)
}

pub fn (mut g Gen) generate_macho_footer() {
	// Create the binary
	mut f := os.create(g.out_name) or { panic(err) }
	os.chmod(g.out_name, 0o775) // make it an executable
	unsafe { f.write_bytes(g.buf.data, g.buf.len) }
	f.close()
	// println('\narm64 mach-o binary has been successfully generated')
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
	g.write32(x64.lc_symtab)
	g.write32(x64.macho_symcmd_size)
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

enum Register2 {
	x0
	x1
	x2
	x3
	x4
	x5
	x6
	x7
	x8
	x9
	x10
	x11
	x12
	x13
	x14
	x15
	x16
}

fn (mut g Gen) mov_arm(reg Register2, val u64) {
	// m := u64(0xffff)
	// x := u64(val)
	// println('========')
	// println(x & ~m)
	// println(x & ~(m << 16))
	// g.write32(0x777777)
	r := int(reg)
	if r == 0 && val == 1 {
		g.write32(0xd2800020)
	} else if r == 0 {
		g.write32(0xd2800000)
	} else if r == 16 {
		g.write32(0xd2800030)
	}
	/*
	if 1 ^ (x & ~m) != 0 {
		// println('yep')
		g.write32(int(u64(0x52800000) | u64(r) | x << 5))
		g.write32(0x88888888)
		g.write32(int(u64(0x52800000) | u64(r) | x >> 11))
	} else if 1 ^ (x & ~(m << 16)) != 0 {
		// g.write32(int(u64(0x52800000) | u64(r) | x >> 11))
		// println('yep2')
		// g.write32(0x52a00000 | r | val >> 11)
	}
	*/
}

fn (mut g Gen) adr() {
	g.write32(0x100000a0)
}

fn (mut g Gen) bl() {
	// g.write32(0xa9400000)
	g.write32(0x94000000)
}

fn (mut g Gen) svc() {
	g.write32(0xd4001001)
}

fn (mut g Gen) write_relocs() {
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
