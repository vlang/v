// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

const (
	s_attr_some_instructions = 0x0400
	s_attr_pure_instructions = 0x80000000
	s_attr_ext_reloc         = 0x0200
	s_attr_loc_reloc         = 0x0100
	macho_symcmd_size        = 0x18
	macho_d_size             = 0x50
	mh_object                = 1
	mh_execute               = 2
	lc_dyld_chained_fixups   = 0x80000034
	lc_dyld_exports_trie     = 0x80000033
	lc_dyld_info_only        = 0x80000022
	lc_dysymtab              = 0xb
	lc_load_dylib            = 0xc
	lc_load_dylinker         = 0xe
	lc_main                  = 0x80000028
	lc_segment_64            = 0x19
	lc_symtab                = 0x2
	base_addr                = i64(0x1_0000_0000)
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
	g.macho_add_loadcommand(native.lc_segment_64, 72)
	g.write_string_with_padding('__PAGEZERO', 16) // section name
	g.write64(0) // vmaddr
	if g.pref.arch == .amd64 {
		g.write64(g.get_pagesize()) // vmsize
	} else {
		g.write64(native.base_addr) // vmsize
	}
	g.write64(0) // fileoff
	g.write64(0) // filesize
	g.write32(0) // maxprot
	g.write32(0) // initprot
	g.write32(0) // nsects
	g.write32(0) // flags
}

fn (mut g Gen) macho_add_loadcommand(typ u32, size int) {
	g.macho_ncmds++
	g.macho_cmdsize += size
	g.write32(int(typ))
	g.write32(size)
}

fn (mut g Gen) macho_patch_header() {
	g.write32_at(0x10, g.macho_ncmds)
	g.write32_at(0x14, g.macho_cmdsize)
}

// probably unnecessary
fn (mut g Gen) macho_chained_fixups() {
	g.macho_add_loadcommand(native.lc_dyld_chained_fixups, 16)
	g.write32(0x4000) // dataoff
	g.write32(56) // datasize

	g.macho_add_loadcommand(native.lc_dyld_exports_trie, 16)
	g.write32(0x4000) // dataoff
	g.write32(56) // datasize
}

fn (mut g Gen) macho_segment64_linkedit() {
	g.macho_add_loadcommand(native.lc_segment_64, 0x48)
	g.write_string_with_padding('__LINKEDIT', 16)

	if g.pref.arch == .amd64 {
		g.write64(0x3000) // vmaddr
		g.write64(0x1000) // vmsize
		g.write64(0x1000) // fileoff
	} else {
		// g.size_pos << g.buf.len
		// g.write64(native.base_addr + g.get_pagesize()) // vmaddr
		g.write64(g.get_pagesize() - 0x1000) // vmaddr
		g.write64(0) // g.get_pagesize()) // vmsize
		g.write64(g.get_pagesize()) // fileoff
	}
	g.write64(0) // filesize
	g.write32(7) // maxprot
	g.write32(3) // initprot // must be writeable
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

	if g.pref.arch == .arm64 {
		g.write32(0x00200085)
	} else {
		g.write32(0) // flags
	}
	g.write32(0) // reserved
	return cmdsize_offset
}

fn (mut g Gen) macho_segment64_text() []int {
	mut patch := []int{}
	g.macho_add_loadcommand(native.lc_segment_64, 152)
	g.write_string_with_padding('__TEXT', 16) // section name
	g.write64(native.base_addr) // vmaddr

	g.write64(g.get_pagesize() * 2) // vmsize
	g.write64(0) // fileoff
	g.write64(g.get_pagesize() + 63) // filesize

	g.write32(7) // maxprot
	g.write32(5) // initprot
	g.write32(1) // nsects
	g.write32(0) // flags

	g.write_string_with_padding('__text', 16) // section name
	g.write_string_with_padding('__TEXT', 16) // segment name
	if g.pref.arch == .arm64 {
		g.write64(native.base_addr + g.get_pagesize()) // vmaddr
		g.write64(0) // vmsize
		g.write32(0) // offset
		g.write32(4) // align
	} else {
		g.write64(native.base_addr + g.get_pagesize()) // vmaddr
		patch << g.buf.len
		g.write64(0) // vmsize
		g.write32(g.get_pagesize()) // offset
		g.write32(0) // align
	}

	g.write32(0) // reloff
	g.write32(0) // nreloc

	if g.pref.arch == .amd64 {
		g.write32(0) // flags
	} else {
		g.write32(0x80000400) // flags
	}
	g.write32(0)

	g.write32(0) // reserved1
	g.write32(0) // reserved2
	return patch
}

fn (mut g Gen) macho_symtab() {
	if g.pref.arch == .arm64 {
		g.macho_add_loadcommand(native.lc_dyld_info_only, 48)
		g.write32(0) // rebase_off
		g.write32(0) // rebase_size
		g.write32(0) // bind_off
		g.write32(0) // bind_size
		g.write32(0) // weak_bind_off
		g.write32(0) // weak_bind_size
		g.write32(0) // lazy_bind_off
		g.write32(0) // lazy_bind_size
		g.write32(g.get_pagesize()) // export_off
		g.write32(56) // export_size
	}

	g.macho_add_loadcommand(native.lc_symtab, 24)
	g.write32(0x1000) // symoff
	g.write32(0) // nsyms
	g.write32(0x1000) // stroff
	g.write32(0) // strsize

	g.macho_add_loadcommand(native.lc_dysymtab, 0x50)
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
	g.macho_add_loadcommand(native.lc_load_dylinker, 32)
	g.write32(12) // offset
	g.write_string_with_padding('/usr/lib/dyld', 16)
	g.write32(0) // padding // can be removed

	g.macho_add_loadcommand(native.lc_load_dylib, 56)
	g.write32(24) // offset
	g.write32(0) // ts
	// g.write32(0x051f6403) // g.write32(1) // current version
	g.write32(0x10000) // current version
	g.write32(0x10000) // compatibility version
	g.write_string_with_padding('/usr/lib/libSystem.B.dylib', 32)
}

fn (mut g Gen) macho_main(addr int) {
	g.macho_add_loadcommand(native.lc_main, 24)
	g.write32(addr) // entrypoint
	g.write32(0) // initial_stacksize
}

pub fn (mut g Gen) generate_macho_header() {
	pagesize := g.get_pagesize()
	g.code_start_pos = pagesize
	g.debug_pos = pagesize
	ncmds := 0 // 9+ 2 -2 -3 -1
	cmdsize_offset := g.macho_header(ncmds, native.mh_execute)
	g.macho_segment64_pagezero()

	g.size_pos = g.macho_segment64_text()
	if g.pref.arch == .amd64 {
		g.macho_segment64_linkedit()
	}
	// g.macho_chained_fixups()
	g.macho_symtab()
	g.macho_dylibs()
	g.macho_main(pagesize)

	g.write32_at(cmdsize_offset, g.buf.len - 24)
	g.write_nulls(pagesize - g.buf.len)
	g.call(0)
}

fn (mut g Gen) get_pagesize() int {
	if g.pref.arch == .arm64 {
		return 0x4000 // 16KB
	}
	return 0x1000 // 4KB
}

fn (mut g Gen) write_nulls(len int) {
	pad := g.get_pagesize() - g.buf.len
	for _ in 0 .. pad {
		g.write8(0)
	}
}

pub fn (mut g Gen) generate_macho_object_header() {
	if g.pref.arch == .arm64 {
		g.write32(0xfeedfacf) // MH_MAGIC_64
		g.write32(0x0100000c) // CPU_TYPE_ARM64
		g.write32(0) // CPU_SUBTYPE_ARM64_ALL
	} else {
		g.write32(0xfeedfacf) // MH_MAGIC_64
		g.write32(0x01000007) // CPU_TYPE_X64
		g.write32(3) // CPU_SUBTYPE_X64
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
	if g.pref.arch == .arm64 {
		g.write32(4) // alignment
	} else {
		g.write32(0) // alignment
	}
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
	g.macho_add_loadcommand(native.lc_dysymtab, native.macho_d_size)
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
		// eprintln('$n + $delta')
		g.write32_at(o, n + delta)
	}
	g.write64(0)
	g.macho_patch_header()
	if g.pref.arch == .amd64 {
		call_delta := int(g.main_fn_addr - g.code_start_pos) - 5
		g.write32_at(g.code_start_pos + 1, call_delta)
		g.create_executable()
	} else {
		call_delta := int(g.main_fn_addr - g.code_start_pos)
		if (call_delta % 4) != 0 || call_delta < 0 {
			g.n_error('Invalid entrypoint->main delta ($call_delta)')
		} else {
			blop := (0x94 << 24) | (call_delta / 4)
			g.write32_at(g.code_start_pos, int(blop))
			g.write_nulls(g.get_pagesize() - g.buf.len)
			g.create_executable()
		}
	}
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
	g.macho_add_loadcommand(native.lc_symtab, native.macho_symcmd_size)
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
					g.write64_at(s.pos, g.buf.len + native.base_addr)
				}
			}
		}
		g.write_string(s.str)
	}
	return g.buf.len - begin
}
