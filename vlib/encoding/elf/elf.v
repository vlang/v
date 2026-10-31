module elf

import os

// Machine names an ELF e_machine architecture. Unlisted architectures map
// to .unknown. Discriminants are the raw e_machine numbers.
pub enum Machine {
	unknown = 0xFFFF
	none    = 0
	x86     = 3
	mips    = 8
	arm     = 40
	x86_64  = 62
	aarch64 = 183
	riscv   = 243
}

// machine_from_value maps an e_machine number.
fn machine_from_value(v u16) Machine {
	match v {
		0 { return .none }
		3 { return .x86 }
		8 { return .mips }
		40 { return .arm }
		62 { return .x86_64 }
		183 { return .aarch64 }
		243 { return .riscv }
		else { return .unknown }
	}
}

// SectionType names an ELF section type. Unlisted types map to .unknown.
pub enum SectionType {
	unknown
	null
	progbits
	symtab
	strtab
	rela
	hash
	dynamic
	note
	nobits
	rel
	dynsym
}

// section_type_from_value maps an sh_type number.
fn section_type_from_value(v u32) SectionType {
	match v {
		0 { return .null }
		1 { return .progbits }
		2 { return .symtab }
		3 { return .strtab }
		4 { return .rela }
		5 { return .hash }
		6 { return .dynamic }
		7 { return .note }
		8 { return .nobits }
		9 { return .rel }
		11 { return .dynsym }
		else { return .unknown }
	}
}

// SectionHeader is one decoded ELF64 section header with its resolved name.
pub struct SectionHeader {
pub mut:
	name string
pub:
	typ     SectionType
	flags   u64
	addr    u64
	offset  u64
	size    u64
	link    u32
	info    u32
	align   u64
	entsize u64
}

// Symbol is one decoded ELF64 symbol table entry with its resolved name.
pub struct Symbol {
pub:
	name  string
	value u64
	size  u64
	info  u8
	other u8
	shndx u16
}

// bind returns the symbol binding (upper nibble of info: 0 local, 1 global..).
pub fn (s &Symbol) bind() u8 {
	return s.info >> 4
}

// typ returns the symbol type (lower nibble of info: 0 none, 1 object,
// 2 func..).
pub fn (s &Symbol) typ() u8 {
	return s.info & 0x0F
}

// File is a parsed ELF64 file: header fields plus section headers. It is the
// V-idiomatic equivalent of Zig's std.elf / Go's debug/elf readers, starting
// with 64-bit little-endian objects (32-bit and big-endian are clear errors).
pub struct File {
mut:
	data     []u8
	sections []SectionHeader
pub:
	machine Machine
	entry   u64
}

// parse_file reads and parses the ELF file at path.
pub fn parse_file(path string) !&File {
	return parse_bytes(os.read_bytes(path)!)
}

// parse_bytes parses one in-memory ELF64 image.
pub fn parse_bytes(data []u8) !&File {
	if data.len < 64 {
		return error('elf: truncated ELF header')
	}
	if data[0] != 0x7F || data[1] != `E` || data[2] != `L` || data[3] != `F` {
		return error('elf: bad magic, not an ELF file')
	}
	if data[4] != 2 {
		return error('elf: only 64-bit objects are supported')
	}
	if data[5] != 1 {
		return error('elf: only little-endian objects are supported')
	}
	if data[6] != 1 || read_u32(data, 20)! != 1 {
		return error('elf: unsupported ELF version')
	}
	if read_u16(data, 52)! < 64 {
		return error('elf: bad ELF header size')
	}
	shoff := read_u64(data, 40)!
	shentsize := read_u16(data, 58)!
	shnum := read_u16(data, 60)!
	shstrndx := read_u16(data, 62)!
	mut f := &File{
		data:    data.clone()
		machine: machine_from_value(read_u16(data, 18)!)
		entry:   read_u64(data, 24)!
	}
	if shnum == 0 {
		if shoff != 0 {
			return error('elf: extended section numbering is not supported')
		}
		if shstrndx != 0 {
			return error('elf: bad string table index')
		}
		return f
	}
	if shentsize < 64 {
		return error('elf: bad section header size')
	}
	if shstrndx >= shnum {
		return error('elf: bad string table index')
	}
	// Check the entire table before calculating individual header offsets.
	table_size := u64(shnum) * u64(shentsize)
	if shoff == 0 || table_size > u64(data.len) || shoff > u64(data.len) - table_size {
		return error('elf: section headers out of bounds')
	}
	for i in 0 .. int(shnum) {
		base := u64(shoff) + u64(i) * u64(shentsize)
		name_off := read_u32(data, base)!
		f.sections << SectionHeader{
			name:    ''
			typ:     section_type_from_value(read_u32(data, base + 4)!)
			flags:   read_u64(data, base + 8)!
			addr:    read_u64(data, base + 16)!
			offset:  read_u64(data, base + 24)!
			size:    read_u64(data, base + 32)!
			link:    read_u32(data, base + 40)!
			info:    read_u32(data, base + 44)!
			align:   read_u64(data, base + 48)!
			entsize: read_u64(data, base + 56)!
			// name resolved below once the string table is known
			// (name_off kept via a second pass)
		}
		_ = name_off
	}
	// SHN_UNDEF denotes a file without section names.
	if shstrndx == 0 {
		return f
	}
	// Resolve names through the string-table section.
	strtab := f.sections[int(shstrndx)]
	if strtab.typ != .strtab {
		return error('elf: section names require a string table')
	}
	strtab_bytes := f.slice(strtab.offset, strtab.size)!
	for i in 0 .. f.sections.len {
		base := u64(shoff) + u64(i) * u64(shentsize)
		name_off := read_u32(data, base)!
		f.sections[i].name = cstring(strtab_bytes, u64(name_off))!
	}
	return f
}

// sections returns all section headers in file order (index 0 is the null
// section, per the format).
pub fn (f &File) sections() []SectionHeader {
	return f.sections.clone()
}

// symbols reads every symtab/dynsym section, resolving names through each
// table's linked string table. Files without any symbol table are an error.
pub fn (f &File) symbols() ![]Symbol {
	mut out := []Symbol{}
	mut found := false
	for sh in f.sections {
		if sh.typ != .symtab && sh.typ != .dynsym {
			continue
		}
		found = true
		strtab := f.slice_of(sh.link, 'string table')!
		if sh.entsize < 24 || sh.size % sh.entsize != 0 {
			return error('elf: bad symbol entry size')
		}
		// Bound the full table before indexing entries or narrowing their offsets.
		symbol_data := f.slice(sh.offset, sh.size)!
		for i in 0 .. int(sh.size / sh.entsize) {
			off := u64(i) * sh.entsize
			name_off := read_u32(symbol_data, off)!
			op := int(off)
			info := symbol_data[op + 4]
			other := symbol_data[op + 5]
			shndx := read_u16(symbol_data, off + 6)!
			value := read_u64(symbol_data, off + 8)!
			size := read_u64(symbol_data, off + 16)!
			out << Symbol{
				name:  cstring(strtab, u64(name_off))!
				value: value
				size:  size
				info:  info
				other: other
				shndx: shndx
			}
		}
	}
	if !found {
		return error('elf: no symbol table present')
	}
	return out
}

// slice returns size bytes at offset, bounds-checked.
fn (f &File) slice(offset u64, size u64) ![]u8 {
	if size > u64(f.data.len) || offset > u64(f.data.len) - size {
		return error('elf: section out of bounds')
	}
	return f.data[int(offset)..int(offset + size)].clone()
}

// slice_of returns the full content of section index idx.
fn (f &File) slice_of(idx u32, what string) ![]u8 {
	if idx >= u32(f.sections.len) {
		return error('elf: bad ${what} index')
	}
	sh := f.sections[idx]
	if sh.typ != .strtab {
		return error('elf: ${what} section is not a string table')
	}
	return f.slice(sh.offset, sh.size)
}

// cstring reads a NUL-terminated string from table at offset.
fn cstring(table []u8, offset u64) !string {
	if offset >= u64(table.len) {
		return error('elf: string offset out of bounds')
	}
	mut end := offset
	for end < u64(table.len) && table[int(end)] != 0 {
		end++
	}
	if end >= u64(table.len) {
		return error('elf: unterminated string')
	}
	return table[int(offset)..int(end)].bytestr()
}

// read_u16 reads a little-endian u16, bounds-checked.
fn read_u16(data []u8, pos u64) !u16 {
	if pos + 2 < pos || pos + 2 > u64(data.len) {
		return error('elf: truncated u16')
	}
	p := int(pos)
	return u16(data[p]) | u16(data[p + 1]) << 8
}

// read_u32 reads a little-endian u32, bounds-checked.
fn read_u32(data []u8, pos u64) !u32 {
	if pos + 4 < pos || pos + 4 > u64(data.len) {
		return error('elf: truncated u32')
	}
	p := int(pos)
	return u32(data[p]) | u32(data[p + 1]) << 8 | u32(data[p + 2]) << 16 | u32(data[p + 3]) << 24
}

// read_u64 reads a little-endian u64, bounds-checked.
fn read_u64(data []u8, pos u64) !u64 {
	if pos + 8 < pos || pos + 8 > u64(data.len) {
		return error('elf: truncated u64')
	}
	p := int(pos)
	return u64(data[p]) | u64(data[p + 1]) << 8 | u64(data[p + 2]) << 16 | u64(data[p + 3]) << 24 | u64(data[p + 4]) << 32 | u64(data[p + 5]) << 40 | u64(data[p + 6]) << 48 | u64(data[p + 7]) << 56
}
