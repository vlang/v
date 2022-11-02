// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import os

const (
	elf_class32              = 1
	elf_class64              = 2

	elf_data_le              = 1
	elf_data_be              = 2

	elf_version              = 1
	elf_abiversion           = 0

	// elf type
	elf_type_none            = 0
	elf_type_rel             = 1
	elf_type_exec            = 2
	elf_type_dyn             = 3
	elf_type_core            = 4

	elf_amd64                = 0x3e
	elf_arm64                = 0xb7

	elf_osabi_none           = 0
	elf_osabi_hpux           = 1
	elf_osabi_netbsd         = 2
	elf_osabi_linux          = 3
	elf_osabi_freebsd        = 9

	elf_header_size          = 0x40
	elf_phentry_size         = 0x38

	// elf program header type
	elf_pt_null              = 0
	elf_pt_load              = 1
	elf_pt_dynamic           = 2
	elf_pt_interp            = 3
	elf_pt_note              = 4
	elf_pt_shlib             = 5
	elf_pt_phdr              = 6
	elf_pt_tls               = 7

	// offset of e_entry field in the elf header
	elf_e_entry_offset       = 24

	// alignment of program headers
	elf_p_align              = 0x1000

	// elf section header size
	elf_shentry_size         = 0x40

	// elf section type
	elf_sht_null             = 0x00
	elf_sht_progbits         = 0x01
	elf_sht_symtab           = 0x02
	elf_sht_strtab           = 0x03
	elf_sht_rela             = 0x04
	elf_sht_hash             = 0x05
	elf_sht_dynamic          = 0x06
	elf_sht_note             = 0x07
	elf_sht_nobits           = 0x08
	elf_sht_rel              = 0x09
	elf_sht_shlib            = 0x0a
	elf_sht_dynsym           = 0x0b

	// elf section flags
	elf_shf_write            = u64(0x01)
	elf_shf_alloc            = u64(0x02)
	elf_shf_execinstr        = u64(0x04)
	elf_shf_merge            = u64(0x10)
	elf_sht_strings          = u64(0x20)
	elf_shf_info_link        = u64(0x40)
	elf_shf_link_order       = u64(0x80)
	elf_shf_os_nonconforming = u64(0x100)
	elf_shf_groub            = u64(0x200)
	elf_shf_tls              = u64(0x400)

	// elf symbol tables
	elf_symtab_size          = 0x18
	elf_dynamic_size         = 0x10
	elf_rela_size            = 0x18
	elf_rel_size             = 0x10
	elf_sh_symtab_entsize    = elf_symtab_size
	elf_sh_symtab_align      = 8

	// elf symbol binding
	elf_stb_local            = u8(0)
	elf_stb_global           = u8(1)

	// elf symbol types
	elf_stt_notype           = u8(0)
	elf_stt_object           = u8(1)
	elf_stt_func             = u8(2)
	elf_stt_section          = u8(3)
	elf_stt_file             = u8(4)

	// elf symbol visibility
	elf_stv_default          = i8(0)

	// elf relocation types
	elf_r_amd64_none         = 0
	elf_r_amd64_64           = 1
	elf_r_amd64_pc32         = 2
	elf_r_amd64_got32        = 3
	elf_r_amd64_plt32        = 4
	elf_r_amd64_copy         = 5
	elf_r_amd64_glob_dat     = 6
	elf_r_amd64_jump_slot    = 7
	elf_r_amd64_relative     = 8
	elf_r_amd64_gotpcrel     = 9
	elf_r_amd64_32           = 10
	elf_r_amd64_32s          = 11
	elf_r_amd64_16           = 12
	elf_r_amd64_pc16         = 13
	elf_r_amd64_8            = 14
	elf_r_amd64_pc8          = 15
	elf_r_amd64_pc64         = 24
	elf_r_amd64_gotoff64     = 25
	elf_r_amd64_gotpc32      = 26
	elf_r_amd64_size32       = 32
	elf_r_amd64_size64       = 33
	elf_r_amd64_gotpcrelx    = 0x29
)

const (
	segment_start = 0x400000
	placeholder   = 0
)

struct ElfHeader {
mut:
	ident_class      i8  // 64 or 32-bit format.
	ident_data       i8  // Endianness.
	ident_version    i8  // ELF version.
	ident_osabi      i8  // Target operating system.
	ident_abiversion i8  // Further specification of the ABI version.
	typ              i16 // Object file type.
	machine          i16 // Target instruction set architecture.
	version          int // ELF version.
	entry            i64 // Memory address of the entry point.
	phoff            i64 // Pointer to the start of the program header table.
	shoff            i64 // Pointer to the start of the section header table.
	flags            int // Flags.
	ehsize           i16 // Header size.
	phentsize        i16 // Size of program headers.
	phnum            i16 // Number of program headers.
	shentsize        i16 // Size of section headers.
	shnum            i16 // Number of section headers.
	shstrndx         i16 // Section index to string table containing section names.
}

fn (mut g Gen) default_elf_header() ElfHeader {
	machine := if g.pref.arch == .arm64 {
		native.elf_arm64
	} else {
		native.elf_amd64
	}

	return ElfHeader{
		ident_class: native.elf_class64
		ident_data: native.elf_data_le
		ident_version: native.elf_version
		ident_osabi: native.elf_osabi_none
		ident_abiversion: native.elf_abiversion
		typ: native.elf_type_none
		machine: i16(machine)
		version: native.elf_version
		phoff: native.elf_header_size
		ehsize: native.elf_header_size
		phentsize: native.elf_phentry_size
		shentsize: native.elf_shentry_size
	}
}

fn (mut g Gen) gen_elf_header(h ElfHeader) {
	g.write('\x7fELF'.bytes())
	g.println('; \\x7fELF')
	g.write8(h.ident_class)
	g.write8(h.ident_data)
	g.write8(h.ident_version)
	g.write8(h.ident_osabi)
	g.write8(h.ident_abiversion)

	// padding
	for _ in 0 .. 7 {
		g.write8(0)
	}
	g.println('; e_ident')

	g.write16(h.typ)
	g.println('; e_type')
	g.write16(h.machine)
	g.println('; e_machine')
	g.write32(h.version)
	g.println('; e_version')
	g.write64(h.entry)
	g.println('; e_entry')
	g.write64(h.phoff)
	g.println('; e_phoff')
	g.write64(h.shoff)
	g.println('; e_shoff')
	g.write32(h.flags)
	g.println('; e_flags')
	g.write16(h.ehsize)
	g.println('; e_ehsize')
	g.write16(h.phentsize)
	g.println('; e_phentsize')
	g.write16(h.phnum)
	g.println('; e_phnum')
	g.write16(h.shentsize)
	g.println('; e_shentsize')
	g.write16(h.shnum)
	g.println('; e_shnum')
	g.write16(h.shstrndx)
	g.println('; e_shstrndx')

	g.println('^^^ ELF header (64)')
}

struct ProgramHeader {
mut:
	typ    int // Program header type.
	flags  int // Segment-independent flags.
	offset i64 // Offset of the segment in the file image.
	vaddr  i64 // Virtual segment address.
	paddr  i64 // Physical segment address.
	filesz i64 // Segment size in the file.
	memsz  i64 // Segment size in memory.
	align  i64 // Segment alignment.
}

fn (mut g Gen) create_program_header(typ int, flags int, align i64) ProgramHeader {
	return ProgramHeader{
		typ: typ
		flags: flags
		align: align
	}
}

fn (mut p ProgramHeader) set_addr(addr i64) {
	p.vaddr = addr
	p.paddr = addr
}

fn (mut g Gen) gen_program_header(p ProgramHeader) {
	g.write32(p.typ) // p_type
	g.println('; p_type')
	g.write32(p.flags) // p_flags
	g.println('; p_flags')
	g.write64(p.offset) // p_offset
	g.println('; p_offset')
	g.write64(if p.vaddr == 0 {
		native.segment_start
	} else {
		p.vaddr
	}) // p_vaddr
	g.println('; p_vaddr')
	g.write64(if p.paddr == 0 {
		native.segment_start
	} else {
		p.paddr
	}) // p_paddr
	g.println('; p_paddr')
	if p.filesz == 0 {
		g.file_size_pos = g.pos()
	}
	g.write64(p.filesz) // p_filesz
	g.println('; p_filesz')
	g.write64(p.memsz) // p_memsz
	g.println('; p_memsz')
	g.write64(p.align) // p_align
	g.println('; p_align')
	g.println('; ^^^ program header (64)')
}

struct SectionHeader {
mut:
	name      int // Offset to name string in .shstrtab.
	typ       int // Section type.
	flags     i64 // Section attributes.
	addr      i64 // Section address.
	offset    i64 // Section address offset.
	size      i64 // Section size.
	link      int // Section index for associated section types.
	info      int // Extra section information.
	addralign i64 // Section Alignment (must be power of two).
	entsize   i64 // Section entry size.

	pos i64 // not part of the actual data
}

struct SymbolTableSection {
	str_name string // string name (not generated)
mut:
	name  int // Index to name in .strtab.
	info  i8  // symbol type and binding attribute.
	other i8  // Symbol visibility.
	shndx i16 // Related section header table index.
	value i64 // value of the associated symbol.
	size  i64 // Symbol size.
}

fn (mut g Gen) create_symbol_table_section(str_name string, info u8, bind u8, other i8, value i64, size i64, shndx i16) SymbolTableSection {
	return SymbolTableSection{
		str_name: str_name
		info: i8(info | bind << 4)
		other: other
		value: value
		size: size
		shndx: shndx
	}
}

struct StringTableSection {
mut:
	strings []string
}

fn (mut g Gen) create_string_table_section(strings []string) StringTableSection {
	return StringTableSection{strings}
}

struct ProgBitsSection {
mut:
	bytes []u8 // Bytes of information defined by the program.
}

fn (mut g Gen) create_progbits_section(bytes []u8) ProgBitsSection {
	return ProgBitsSection{bytes}
}

struct RelASection {
	name string // not in the actual data
mut:
	offset i64 // Location at which to apply the relocation action.
	info   i64 // Symbol table index and type of relocation to apply.
	addend i64 // Constant addent for computing the value of the relocation field.
}

fn (mut g Gen) create_rela_section(name string, offset i64, sym int, typ u32, addend i64) RelASection {
	return RelASection{
		name: name
		offset: offset
		info: i64((u64(sym) << 32) + typ)
		addend: addend
	}
}

struct HashSection {
	// TODO
}

fn (mut g Gen) create_hash_section() HashSection {
	return HashSection{}
}

struct DynamicSection {
mut:
	tag i64 // Type of the dynamic Section.
	un  i64 // Consists of d_val and d_ptr, represents program virtual addresses.
}

fn (mut g Gen) create_dynamic_section(tag i64, val i64) DynamicSection {
	return DynamicSection{tag, val}
}

struct NoteSection {
mut:
	namesz int // Length of the name field in bytes.
	descsz int // Length of the descriptor field in bytes.
	typ    int // Type of the node

	name []u8 // Name string of the note.
	desc []u8 // Descripition string of the node, must be aligned by 4 bytes
}

fn (mut g Gen) create_note_section(typ int, name string, desc string) NoteSection {
	return NoteSection{
		namesz: name.len
		descsz: desc.len
		typ: typ
		name: name.bytes()
		desc: desc.bytes()
	}
}

struct NoBitsSection {
	// The NoBits section does not occupy any bytes in the file
}

fn (mut g Gen) chreate_nobits_section() NoBitsSection {
	return NoBitsSection{}
}

struct RelSection {
mut:
	offset i64 // Location at which to apply the relocation action.
	info   i64 // Symbol table index and type of relocation to apply.
}

fn (mut g Gen) create_rel_section(offset i64, sym u32, typ u32) RelSection {
	return RelSection{
		offset: offset
		info: i64((u64(sym) << 32) + typ)
	}
}

struct ShLibSection {
mut:
	data []u8 // Reserved bytes with unspecified semantics
}

fn (mut g Gen) create_shlib_section(data []u8) ShLibSection {
	return ShLibSection{data}
}

struct DynSymSection {
mut:
	symbols []SymbolTableSection
}

fn (mut g Gen) create_dynsym_section(symbols []SymbolTableSection) DynSymSection {
	return DynSymSection{symbols}
}

type SectionData = DynSymSection
	| HashSection
	| NoBitsSection
	| NoteSection
	| ProgBitsSection
	| RelSection
	| ShLibSection
	| StringTableSection
	| []DynamicSection
	| []RelASection
	| []SymbolTableSection

struct Section {
	name string
mut:
	header SectionHeader
	data   SectionData = DynSymSection{}
}

fn (mut g Gen) create_section(name string, typ int, link int, info int, addralign i64, entsize i64, data SectionData) Section {
	return Section{
		name: name
		header: SectionHeader{
			typ: typ
			link: link
			info: info
			addralign: addralign
			entsize: entsize
		}
		data: data
	}
}

fn (mut g Gen) create_shstrtab(mut sections []Section) {
	mut names := []string{len: sections.len + 1}
	mut offset := 1

	for i, mut section in sections {
		names[i] = section.name
		section.header.name = offset
		offset += section.name.len + 1
	}

	names[sections.len] = '.shstrtab'

	mut shstrtab := g.create_section(names[sections.len], native.elf_sht_strtab, 0, 0,
		1, 0, g.create_string_table_section(names))
	shstrtab.header.name = offset

	sections << shstrtab
}

fn (mut g Gen) create_symtab(mut sections []Section, mut table []SymbolTableSection) {
	mut names := []string{len: table.len}
	mut offset := 1
	mut local_symbols := 0

	for i, mut entry in table {
		names[i] = entry.str_name

		entry.name = offset

		if (entry.info >> 4) == native.elf_stb_local {
			local_symbols++
		}

		offset += entry.str_name.len + 1
	}

	sections << g.create_section('.strtab', native.elf_sht_strtab, 0, 0, 1, 0, g.create_string_table_section(names))

	sections << // index of .strtab
	g.create_section('.symtab', native.elf_sht_symtab, sections.len - 1, local_symbols,
		native.elf_sh_symtab_align, native.elf_sh_symtab_entsize, table)
}

fn (mut g Gen) create_relocation(name string, mut sections []Section, table []RelASection) Section {
	mut section := g.create_section(name, native.elf_sht_rela, g.find_section_header('.symtab',
		sections), 1, 8, 24, table)
	section.header.flags = i64(native.elf_shf_info_link)
	sections << section
	return section
}

fn (mut g Gen) create_progbits(name string, flags u64, data []u8) Section {
	mut section := g.create_section(name, native.elf_sht_progbits, 0, 0, 1, data.len,
		ProgBitsSection{data})
	section.header.flags = i64(flags)
	return section
}

fn (mut g Gen) find_section_header(name string, sections []Section) int {
	for i, section in sections {
		if name == section.name {
			return i
		}
	}
	return 0
}

fn (mut g Gen) gen_section_header(mut sh SectionHeader) {
	sh.pos = g.pos()

	g.write32(sh.name) // sh_name
	g.println('; sh_name')
	g.write32(sh.typ) // sh_type
	g.println('; sh_type')
	g.write64(sh.flags) // sh_flags
	g.println('; sh_addr')
	g.write64(sh.addr) // sh_addr
	g.println('; sh_name')
	if sh.offset == 0 {
		g.write64(sh.pos)
		sh.offset = sh.pos
	} else {
		g.write64(sh.offset)
	} // sh_offset
	g.println('; sh_offset')
	g.write64(sh.size) // sh_size
	g.println('; sh_size')
	g.write32(sh.link) // sh_link
	g.println('; sh_link')
	g.write32(sh.info) // sh_info
	g.println('; sh_info')
	g.write64(sh.addralign) // sh_addralign
	g.println('; sh_addralign')
	g.write64(sh.entsize) // sh_entsize
	g.println('; sh_entsize')
}

fn (mut g Gen) gen_sections(mut sections []Section) {
	for mut section in sections {
		g.gen_section_header(mut section.header)
		g.println('; ^^^ section header (64) "$section.name"')
	}
}

fn (mut g Gen) gen_symtab_data(section Section, data []SymbolTableSection) {
	for symbol in data {
		if symbol.str_name == 'main' { // represents the C main function
			g.start_symbol_addr = g.pos()
		}

		g.write32(symbol.name)
		g.write8(symbol.info)
		g.write8(symbol.other)
		g.write16(symbol.shndx)
		g.write64(symbol.value)
		g.write64(symbol.size)
		g.println('; SHT_SYMTAB $symbol.str_name')
	}

	size := native.elf_symtab_size * data.len
	g.write64_at(section.header.pos + 32, i64(size))
}

fn (mut g Gen) gen_section_data(sections []Section) {
	for section in sections {
		data := section.data

		// write the actual offset of the section data
		g.write64_at(section.header.pos + 24, i64(g.pos()))

		match data {
			StringTableSection {
				start := g.pos()

				g.write8(0) // null-prefixed
				for str in data.strings {
					g.write(str.bytes())
					g.write8(0) // null-terminate string
					g.println('; "$str"')
				}
				g.write8(0) // null-postfixed

				size := g.pos() - start
				g.write64_at(section.header.pos + 32, i64(size))
			}
			[]SymbolTableSection {
				g.gen_symtab_data(section, data)
			}
			ProgBitsSection {
				// progbits have to be handled by the user.
			}
			[]RelASection {
				if data.len == 0 {
					continue
				}

				for rela in data {
					g.write64(rela.offset)
					g.fn_addr[rela.name] = rela.offset
					g.write64(rela.info)
					g.write64(rela.addend)
					g.println('; SHT_RELA `$rela.name` ($rela.offset, $rela.info, $rela.addend)')
				}

				size := native.elf_rela_size * data.len
				g.write64_at(section.header.pos + 32, i64(size))
			}
			HashSection {
				// TODO
				g.n_error('ELF Section `SHT_HASH` not implemented yet')
			}
			[]DynamicSection {
				for dyn in data {
					g.write64(dyn.tag)
					g.write64(dyn.un)
					g.println('; SHT_DYNAMIC ($dyn.tag, $dyn.un)')
				}

				size := native.elf_dynamic_size * data.len
				g.write64_at(section.header.pos + 32, i64(size))
			}
			NoteSection {
				start := g.pos()

				g.write32(data.namesz)
				g.write32(data.descsz)
				g.write32(data.typ)
				g.write(data.name)

				for _ in 0 .. ((g.pos() % 4) + 1) {
					g.write8(0) // align by 4 bytes + null terminator
				}

				g.write(data.desc)
				g.write8(0)

				size := g.pos() - start
				g.write64_at(section.header.pos + 32, i64(size))
			}
			NoBitsSection {
				// Nothing to do
			}
			RelSection {
				g.write64(data.offset)
				g.write64(data.info)
				g.println('; SHT_REL ($data.offset, $data.info)')

				size := native.elf_rel_size
				g.write64_at(section.header.pos + 32, i64(size))
			}
			ShLibSection {
				g.write(data.data)

				size := data.data.len
				g.write64_at(section.header.pos + 32, i64(size))
			}
			DynSymSection {
				g.gen_symtab_data(section, data.symbols)
			}
		}
	}
}

pub fn (mut g Gen) symtab_get_index(symbols []SymbolTableSection, name string) int {
	for i, sym in symbols {
		if sym.str_name == name {
			return i
		}
	}
	return 0
}

pub fn (mut g Gen) generate_linkable_elf_header() {
	elf_type := native.elf_type_rel // PIE (use _exec for non-relocatable executables)

	// generate program headers
	mut program_headers := []ProgramHeader{}
	program_headers << g.create_program_header(native.elf_pt_load, 5, native.elf_p_align)
	// generate sections
	mut sections := [
		Section{}, // null section as first section
		g.create_progbits('.text', native.elf_shf_alloc | native.elf_shf_execinstr, []),
		g.create_progbits('.data', native.elf_shf_write | native.elf_shf_alloc, []),
		g.create_progbits('.bss', native.elf_shf_write | native.elf_shf_alloc, []),
	]

	g.symbol_table = [
		SymbolTableSection{}, // first is null
		g.create_symbol_table_section('main', native.elf_stt_notype, native.elf_stb_global,
			native.elf_stv_default, 0, 0, i16(g.find_section_header('.text', sections))), // main label points to entry point address
		g.create_symbol_table_section('_GLOBAL_OFFSET_TABLE_', native.elf_stt_notype,
			native.elf_stb_global, native.elf_stv_default, 0, 0, 0),
	]

	for symbol in g.extern_symbols {
		g.symbol_table << g.create_symbol_table_section(symbol[2..], native.elf_stt_notype,
			native.elf_stb_global, native.elf_stv_default, 0, 0, 0)
	}
	g.create_symtab(mut sections, mut g.symbol_table) // create the .symtab section
	g.create_relocation('.rela.text', mut sections, [])
	g.create_shstrtab(mut sections) // create the .shstrtab section (this must be the last section!)

	mut elf_header := g.default_elf_header()

	elf_header.typ = i16(elf_type)
	elf_header.shoff = native.elf_header_size + native.elf_phentry_size * program_headers.len
	elf_header.phnum = i16(program_headers.len)
	elf_header.shnum = i16(sections.len)
	elf_header.shstrndx = i16(g.find_section_header('.shstrtab', sections))

	g.gen_elf_header(elf_header)

	// write program headers
	for header in program_headers {
		g.gen_program_header(header)
	}

	// write section headers
	g.gen_sections(mut sections)

	// write sections
	g.gen_section_data(sections)

	g.elf_rela_section = sections[g.find_section_header('.rela.text', sections)]

	// user code starts here
	if g.pref.is_verbose {
		eprintln('code_start_pos = $g.buf.len.hex()')
	}

	g.code_start_pos = g.pos()
	g.debug_pos = int(g.pos())
	// if g.start_symbol_addr > 0 {
	//	g.write64_at(g.start_symbol_addr + native.elf_symtab_size - 16, g.code_start_pos)
	//}

	text_section := sections[g.find_section_header('.text', sections)]
	g.elf_text_header_addr = text_section.header.offset
	g.write64_at(g.elf_text_header_addr + 24, g.pos()) // write the code start pos to the text section

	g.call(native.placeholder)
	g.println('; call main.main')
	if g.pref.arch == .arm64 {
	} else {
		g.mov64(.rax, 0)
	}
	g.ret()
	g.println('; return 0')

	g.debug_pos = g.buf.len
}

pub fn (mut g Gen) generate_simple_elf_header() {
	elf_type := native.elf_type_exec

	mut phdr := g.create_program_header(native.elf_pt_load, 5, native.elf_p_align)
	phdr.vaddr = native.segment_start
	phdr.paddr = native.segment_start

	mut elf_header := g.default_elf_header()
	elf_header.typ = i16(elf_type)
	elf_header.phnum = i16(1)
	elf_header.shoff = i64(0)
	elf_header.shentsize = i16(0)
	elf_header.shnum = i16(0)
	elf_header.shstrndx = i16(0)
	elf_header.entry = native.segment_start + native.elf_header_size + native.elf_phentry_size

	g.gen_elf_header(elf_header)

	// write program header
	g.gen_program_header(phdr)

	// user code starts here
	if g.pref.is_verbose {
		eprintln('code_start_pos = $g.buf.len.hex()')
	}

	g.code_start_pos = g.pos()
	g.debug_pos = int(g.pos())

	g.call(native.placeholder)
	g.println('; call main.main')

	// generate exit syscall
	match g.pref.arch {
		.arm64 {
			g.mov_arm(.x16, 0)
			g.mov_arm(.x0, 0)
			g.svc()
		}
		.amd64 {
			g.mov(.edi, 0)
			g.mov(.eax, g.nsyscall_exit())
			g.syscall()
		}
		else {
			g.n_error('unsupported platform $g.pref.arch')
		}
	}
}

pub fn (mut g Gen) elf_string_table() {
	mut generated := map[string]int{}

	for _, s in g.strs {
		pos := generated[s.str] or { g.buf.len }

		match s.typ {
			.abs64 {
				g.write64_at(s.pos, pos)
			}
			.rel32 {
				g.write32_at(s.pos, pos - s.pos - 4)
			}
			else {
				g.n_error('unsupported string reloc type')
			}
		}

		if s.str !in generated {
			generated[s.str] = pos
			g.write_string(s.str)
		}
	}
}

pub fn (mut g Gen) gen_rela_section() {
	mut relocations := []RelASection{}
	for call_pos, symbol in g.extern_fn_calls {
		relocations << g.create_rela_section(symbol, call_pos - g.code_start_pos + 2,
			g.symtab_get_index(g.symbol_table, symbol[2..]), native.elf_r_amd64_gotpcrelx,
			-4)
	}
	g.elf_rela_section.data = relocations
	g.gen_section_data([g.elf_rela_section])
}

pub fn (mut g Gen) generate_elf_footer() {
	g.elf_string_table()

	// write size of text section into section header
	if g.elf_text_header_addr != -1 {
		g.write64_at(g.elf_text_header_addr + 32, g.pos() - g.code_start_pos)
	}

	if g.extern_symbols.len != 0 {
		g.gen_rela_section()
	}

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

pub fn (mut g Gen) prepend_vobjpath(paths []string) []string {
	vopath := os.getenv('VOBJPATH')
	if vopath == '' {
		return paths
	}
	mut res := paths.clone()
	res.insert(0, vopath)
	return res
}

pub fn (mut g Gen) find_o_path(fname string) string {
	opaths := match g.pref.arch {
		.amd64 {
			g.prepend_vobjpath(['/usr/lib/x86_64-linux-gnu', '/usr/lib64', '/usr/lib'])
		}
		.arm64 {
			g.prepend_vobjpath(['/usr/lib/aarch64-linux-gnu', '/usr/lib'])
		}
		else {
			g.n_error('unknown architecture $g.pref.arch')
			['/dev/null']
		}
	}

	for opath in opaths {
		fpath := os.join_path_single(opath, fname)
		if os.is_file(fpath) {
			return fpath
		}
	}
	return '/dev/null'
}

pub fn (mut g Gen) get_lpaths() string {
	lpaths := match g.pref.arch {
		.amd64 {
			g.prepend_vobjpath(['/usr/lib/x86_64-linux-gnu', '/usr/lib64', '/lib64', '/usr/lib',
				'/lib'])
		}
		.arm64 {
			g.prepend_vobjpath(['/usr/lib/aarch64-linux-gnu', '/usr/lib', '/lib'])
		}
		else {
			g.n_error('unknown architecture $g.pref.arch')
			['/dev/null']
		}
	}
	return lpaths.map('-L$it').join(' ')
}

pub fn (mut g Gen) link_elf_file(obj_file string) {
	crt1 := g.find_o_path('crt1.o')
	crti := g.find_o_path('crti.o')
	crtn := g.find_o_path('crtn.o')
	lpaths := g.get_lpaths()

	arch := match g.pref.arch {
		.amd64 {
			'elf_x86_64'
		}
		.arm64 {
			'aarch64elf'
		}
		else {
			g.n_error('unknown architecture $g.pref.arch')
			'elf_x86_64' // default to x86_64
		}
	}

	dynamic_linker := match g.pref.arch {
		.amd64 { '/lib64/ld-linux-x86-64.so.2' }
		.arm64 { '/lib/ld-linux-aarch64.so.1' }
		else { '/dev/null' }
	}

	linker_args := [
		'-v',
		lpaths,
		'-m $arch',
		'-dynamic-linker',
		dynamic_linker,
		crt1,
		crti,
		'-lc',
		'-lm',
		'-lpthread',
		crtn,
		'$obj_file',
		'-o $g.out_name',
	]
	slinker_args := linker_args.join(' ')

	mut ld := 'ld'

	custom_linker := os.getenv('VLINKER')
	if custom_linker != '' {
		ld = custom_linker
	}
	linker_path := os.real_path(ld)
	linker_cmd := '${os.quoted_path(linker_path)} $slinker_args'
	if g.pref.is_verbose {
		println(linker_cmd)
	}

	res := os.execute(linker_cmd)
	if res.exit_code != 0 {
		g.n_error('ELF linking failed ($ld):\n$res.output')
		return
	}

	if g.pref.is_verbose {
		println('linking with $ld finished successfully:\n$res.output')
	}
}
