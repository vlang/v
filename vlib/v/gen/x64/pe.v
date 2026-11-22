// Copyright (c) 2026. PE/COFF (x86_64) object writer for the V x64 native backend.
module x64

import os
import v.gen.elf

pub fn write_pe_from_elf(e &elf.ElfObject, path string) {
	mut buf := []u8{}

	// Sections: .text, .data, .rdata
	// We'll build section raw data first.
	text_data := e.text_data
	data_data := e.data_data
	rdata_data := e.rodata

	// COFF file header
	machine_amd64 := u16(0x8664)
	nsections := u16(3)
	// Symbol table: count defined + undefined. We'll build symbols list.
	// First pass: collect symbol names and assign indices.
	mut sym_names := []string{}
	mut sym_section := []int{} // 1-based section number, 0 = undefined
	mut sym_value := []u32{}
	mut sym_storage := []u8{} // 2 = external
	mut sym_type := []u16{} // 0x20 for function
	mut sym_name_to_idx := map[string]int{}

	// null symbol? COFF doesn't require null symbol.

	for sym in e.symbols {
		if sym.name == '' {
			continue
		}
		idx := sym_names.len
		sym_names << sym.name
		if sym.shndx == 0 {
			sym_section << 0
		} else if sym.shndx == 1 {
			sym_section << 1
		} else if sym.shndx == 3 {
			sym_section << 3
		} else {
			sym_section << 2
		}
		sym_value << u32(sym.value)
		sym_storage << u8(2) // external
		if sym.shndx == 1 {
			sym_type << u16(0x20) // function
		} else {
			sym_type << u16(0)
		}
		sym_name_to_idx[sym.name] = idx
	}
	// Externals (undefined) already included with shndx 0.

	// Also need symbols for externals referenced in relocs but not in symbol table.
	// elf symbols list includes undefined entries (shndx 0) for externals, so covered.

	nsyms := u32(sym_names.len)

	// String table for long names (>8 chars).
	mut strtab := []u8{}
	strtab << u8(0)
	strtab << u8(0)
	strtab << u8(0)
	strtab << u8(0) // size placeholder
	// We'll append names > 8 chars and record offsets.
	mut name_storage := [][]u8{} // 8-byte name field per symbol
	for i, nm in sym_names {
		mut field := []u8{len: 8, init: 0}
		if nm.len <= 8 {
			for j in 0 .. nm.len {
				field[j] = u8(nm[j])
			}
		} else {
			// offset into string table (relative to start of strtab, including 4-byte size)
			off := strtab.len
			for c in nm.bytes() {
				strtab << c
			}
			strtab << 0
			// store offset in first 4 bytes as little-endian (with high bit set? COFF uses /4)
			// COFF long name: first 4 bytes = 0, next 4 = offset
			field[0] = 0
			field[1] = 0
			field[2] = 0
			field[3] = 0
			field[4] = u8(off)
			field[5] = u8(off >> 8)
			field[6] = u8(off >> 16)
			field[7] = u8(off >> 24)
		}
		name_storage << field
		_ = i
	}
	// fix strtab size
	strtab_size := u32(strtab.len)
	strtab[0] = u8(strtab_size)
	strtab[1] = u8(strtab_size >> 8)
	strtab[2] = u8(strtab_size >> 16)
	strtab[3] = u8(strtab_size >> 24)

	// Layout
	file_hdr_size := 20
	sect_hdr_size := 40
	nsec := 3
	headers_size := file_hdr_size + sect_hdr_size * nsec

	// section raw data offsets
	text_raw_off := headers_size
	text_raw_size := text_data.len
	data_raw_off := text_raw_off + text_raw_size
	data_raw_size := data_data.len
	rdata_raw_off := data_raw_off + data_raw_size
	rdata_raw_size := rdata_data.len

	// relocs per section
	// text relocs
	mut text_relocs := []int{} // indices into reloc list
	// build reloc entries: VirtualAddress (offset in section), SymbolTableIndex, Type
	mut reloc_entries := [][]u8{} // each 10 bytes
	for r in e.text_relocs {
		sym_idx := int(r.info >> 32)
		// find symbol name for this elf sym idx
		// elf symbol at sym_idx
		if sym_idx >= e.symbols.len {
			continue
		}
		esym := e.symbols[sym_idx]
		// find our index
		our_idx := sym_name_to_idx[esym.name] or { continue }
		// REL32 = 4
		mut entry := []u8{len: 10, init: 0}
		off := u32(r.offset)
		entry[0] = u8(off)
		entry[1] = u8(off >> 8)
		entry[2] = u8(off >> 16)
		entry[3] = u8(off >> 24)
		idxv := u32(our_idx)
		entry[4] = u8(idxv)
		entry[5] = u8(idxv >> 8)
		entry[6] = u8(idxv >> 16)
		entry[7] = u8(idxv >> 24)
		// type REL32 = 4
		entry[8] = 4
		entry[9] = 0
		reloc_entries << entry
		text_relocs << reloc_entries.len - 1
	}
	text_reloc_count := u16(reloc_entries.len)
	text_reloc_off := rdata_raw_off + rdata_raw_size
	// pad to 4? COFF relocs can be anywhere; keep contiguous.
	// symbol table offset
	symtab_off := text_reloc_off + reloc_entries.len * 10
	// string table after symbols
	strtab_off := symtab_off + int(nsyms) * 18

	// --- Write file header ---
	// Machine
	write_u16_le_pe(mut buf, machine_amd64)
	// NumberOfSections
	write_u16_le_pe(mut buf, nsections)
	// TimeDateStamp
	write_u32_le_pe(mut buf, 0)
	// PointerToSymbolTable
	write_u32_le_pe(mut buf, u32(symtab_off))
	// NumberOfSymbols
	write_u32_le_pe(mut buf, nsyms)
	// SizeOfOptionalHeader
	write_u16_le_pe(mut buf, 0)
	// Characteristics
	write_u16_le_pe(mut buf, 0)

	// --- Section headers ---
	// .text
	write_section_header(mut buf, '.text', u32(text_raw_size), u32(text_raw_size), u32(text_raw_off), u32(text_reloc_off), u16(text_reloc_count), 0x60000020)
	// .data
	write_section_header(mut buf, '.data', u32(data_raw_size), u32(data_raw_size), u32(data_raw_off), 0, 0, 0xc0000040)
	// .rdata
	write_section_header(mut buf, '.rdata', u32(rdata_raw_size), u32(rdata_raw_size), u32(rdata_raw_off), 0, 0, 0x40000040)

	// --- Raw data ---
	// pad to text_raw_off (already at headers_size)
	buf << text_data
	buf << data_data
	buf << rdata_data

	// relocs
	for re in reloc_entries {
		buf << re
	}

	// symbol table
	for i in 0 .. int(nsyms) {
		// name (8 bytes)
		buf << name_storage[i]
		// Value
		write_u32_le_pe(mut buf, sym_value[i])
		// SectionNumber (i16)
		write_i16_le_pe(mut buf, i16(sym_section[i]))
		// Type
		write_u16_le_pe(mut buf, sym_type[i])
		// StorageClass
		buf << sym_storage[i]
		// NumberOfAuxSymbols
		buf << u8(0)
	}

	// string table
	buf << strtab

	os.write_file_array(path, buf) or { panic(err) }
}

fn write_section_header(mut buf []u8, name string, size u32, raw_size u32, raw_off u32, reloc_off u32, nreloc u16, chars u32) {
	mut nb := name.bytes()
	for nb.len < 8 {
		nb << 0
	}
	for i in 0 .. 8 {
		buf << nb[i]
	}
	write_u32_le_pe(mut buf, 0) // VirtualSize
	write_u32_le_pe(mut buf, 0) // VirtualAddress
	write_u32_le_pe(mut buf, raw_size) // SizeOfRawData
	write_u32_le_pe(mut buf, raw_off) // PointerToRawData
	write_u32_le_pe(mut buf, reloc_off) // PointerToRelocations
	write_u32_le_pe(mut buf, 0) // PointerToLinenumbers
	write_u16_le_pe(mut buf, nreloc) // NumberOfRelocations
	write_u16_le_pe(mut buf, 0) // NumberOfLinenumbers
	write_u32_le_pe(mut buf, chars) // Characteristics
}

fn write_u16_le_pe(mut b []u8, v u16) {
	b << u8(v)
	b << u8(v >> 8)
}

fn write_u32_le_pe(mut b []u8, v u32) {
	b << u8(v)
	b << u8(v >> 8)
	b << u8(v >> 16)
	b << u8(v >> 24)
}

fn write_i16_le_pe(mut b []u8, v i16) {
	b << u8(v)
	b << u8(v >> 8)
}
