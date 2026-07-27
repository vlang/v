// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

const elf64_header_size = u64(64)
const elf64_section_header_size = u64(64)
const elf64_symbol_size = u64(24)
const elf64_rela_size = u64(24)
const elf64_section_count = u16(6)
const elf64_private_data_section_count = u16(7)

const elf64_sht_progbits = u32(1)
const elf64_sht_symtab = u32(2)
const elf64_sht_strtab = u32(3)
const elf64_sht_rela = u32(4)
const elf64_sht_nobits = u32(8)

const elf64_shf_alloc = u64(0x2)
const elf64_shf_execinstr = u64(0x4)
const elf64_shf_write = u64(0x1)
const elf64_shf_info_link = u64(0x40)
const elf64_allocatable_size_limit = u64(0x8000_0000)
const elf64_r_x86_64_64 = u64(1)
const elf64_r_x86_64_pc32 = u64(2)
const elf64_r_x86_64_plt32 = u64(4)
const elf64_r_x86_64_gotpcrel = u64(9)
const elf64_r_x86_64_32 = u64(10)
const elf64_r_x86_64_32s = u64(11)
const elf64_call_addend = i64(-4)

struct Elf64Layout {
	text_offset            u64
	rela_text_offset       u64
	symtab_offset          u64
	strtab_offset          u64
	shstrtab_offset        u64
	section_headers_offset u64
	file_size              u64
}

struct Elf64PrivateDataLayout {
	text_offset            u64
	rela_text_offset       u64
	data_offset            u64
	symtab_offset          u64
	strtab_offset          u64
	shstrtab_offset        u64
	section_headers_offset u64
	file_size              u64
}

struct Elf64ObjectDataRelocationEncoding {
	typ   u64
	width u64
}

struct Elf64ObjectDataRelocation {
	offset u64
	info   u64
	addend i64
}

struct Elf64ObjectDataSection {
	name       string
	type_      u32
	flags      u64
	alignment  u64
	entry_size u64
mut:
	name_offset   u32
	offset        u64
	semantic_size u64
	link          u32
	info          u32
	bytes         []u8
}

struct Elf64ObjectDataPreflight {
	sections               []Elf64ObjectDataSection
	section_headers_offset u64
	file_size              u64
	shstrtab_index         u16
}

fn elf64_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('ELF64 ' + label + ' overflows u64')
	}
	return left + right
}

fn elf64_checked_mul(left u64, right u64, label string) !u64 {
	if left != 0 && right > max_u64 / left {
		return error('ELF64 ' + label + ' overflows u64')
	}
	return left * right
}

fn elf64_checked_host_size(value u64) !int {
	if value > u64(max_int) {
		return error('ELF64 output exceeds the host array limit')
	}
	return int(value)
}

fn elf64_private_data_symbol_count(data_count u64, function_count u64) !u64 {
	non_null_count := elf64_checked_add(data_count, function_count, 'non-null symbol count')!
	symbol_count := elf64_checked_add(non_null_count, 1, 'symbol count')!
	if symbol_count > u64(max_u32) {
		return error('ELF64 symbol count exceeds u32')
	}
	return symbol_count
}

fn elf64_align(value u64, alignment u64, label string) !u64 {
	if alignment == 0 {
		return error('ELF64 ' + label + ' has zero alignment')
	}
	remainder := value % alignment
	if remainder == 0 {
		return value
	}
	return elf64_checked_add(value, alignment - remainder, label)
}

fn elf64_append_string(mut table []u8, value string) !u32 {
	growth := elf64_checked_add(u64(value.len), 1, 'string table size')!
	if growth > u64(max_u32) || u64(table.len) > u64(max_u32) - growth {
		return error('ELF64 string table exceeds the u32 index range')
	}
	if table.len >= max_int || value.len > max_int - table.len - 1 {
		return error('ELF64 string table exceeds the host array limit')
	}
	offset := u32(table.len)
	table << value.bytes()
	table << u8(0)
	return offset
}

fn elf64_build_layout(text_size u64, rela_text_size u64, symtab_size u64, strtab_size u64, shstrtab_size u64) !Elf64Layout {
	mut cursor := elf64_header_size
	text_offset := elf64_align(cursor, 16, '.text offset')!
	cursor = elf64_checked_add(text_offset, text_size, '.text extent')!
	rela_text_offset := elf64_align(cursor, 8, '.rela.text offset')!
	cursor = elf64_checked_add(rela_text_offset, rela_text_size, '.rela.text extent')!
	symtab_offset := elf64_align(cursor, 8, '.symtab offset')!
	cursor = elf64_checked_add(symtab_offset, symtab_size, '.symtab extent')!
	strtab_offset := cursor
	cursor = elf64_checked_add(strtab_offset, strtab_size, '.strtab extent')!
	shstrtab_offset := cursor
	cursor = elf64_checked_add(shstrtab_offset, shstrtab_size, '.shstrtab extent')!
	section_headers_offset := elf64_align(cursor, 8, 'section header offset')!
	section_headers_size := elf64_checked_mul(u64(elf64_section_count), elf64_section_header_size,
		'section header table size')!
	file_size := elf64_checked_add(section_headers_offset, section_headers_size, 'file size')!
	return Elf64Layout{
		text_offset:            text_offset
		rela_text_offset:       rela_text_offset
		symtab_offset:          symtab_offset
		strtab_offset:          strtab_offset
		shstrtab_offset:        shstrtab_offset
		section_headers_offset: section_headers_offset
		file_size:              file_size
	}
}

fn elf64_build_private_data_layout(text_size u64, rela_text_size u64, data_size u64, data_alignment u64, symtab_size u64, strtab_size u64, shstrtab_size u64) !Elf64PrivateDataLayout {
	mut cursor := elf64_header_size
	text_offset := elf64_align(cursor, 16, '.text offset')!
	cursor = elf64_checked_add(text_offset, text_size, '.text extent')!
	rela_text_offset := elf64_align(cursor, 8, '.rela.text offset')!
	cursor = elf64_checked_add(rela_text_offset, rela_text_size, '.rela.text extent')!
	data_offset := elf64_align(cursor, data_alignment, '.data offset')!
	cursor = elf64_checked_add(data_offset, data_size, '.data extent')!
	symtab_offset := elf64_align(cursor, 8, '.symtab offset')!
	cursor = elf64_checked_add(symtab_offset, symtab_size, '.symtab extent')!
	strtab_offset := cursor
	cursor = elf64_checked_add(strtab_offset, strtab_size, '.strtab extent')!
	shstrtab_offset := cursor
	cursor = elf64_checked_add(shstrtab_offset, shstrtab_size, '.shstrtab extent')!
	section_headers_offset := elf64_align(cursor, 8, 'section header offset')!
	section_headers_size := elf64_checked_mul(u64(elf64_private_data_section_count),
		elf64_section_header_size, 'section header table size')!
	file_size := elf64_checked_add(section_headers_offset, section_headers_size, 'file size')!
	return Elf64PrivateDataLayout{
		text_offset:            text_offset
		rela_text_offset:       rela_text_offset
		data_offset:            data_offset
		symtab_offset:          symtab_offset
		strtab_offset:          strtab_offset
		shstrtab_offset:        shstrtab_offset
		section_headers_offset: section_headers_offset
		file_size:              file_size
	}
}

fn elf64_write_u16(mut output []u8, value u16) {
	output << u8(value)
	output << u8(value >> 8)
}

fn elf64_write_u32(mut output []u8, value u32) {
	output << u8(value)
	output << u8(value >> 8)
	output << u8(value >> 16)
	output << u8(value >> 24)
}

fn elf64_write_u64(mut output []u8, value u64) {
	output << u8(value)
	output << u8(value >> 8)
	output << u8(value >> 16)
	output << u8(value >> 24)
	output << u8(value >> 32)
	output << u8(value >> 40)
	output << u8(value >> 48)
	output << u8(value >> 56)
}

fn elf64_pad_to(mut output []u8, target u64) ! {
	if target > u64(max_int) {
		return error('ELF64 output offset exceeds the host array limit')
	}
	if u64(output.len) > target {
		return error('ELF64 internal layout moved backwards')
	}
	for u64(output.len) < target {
		output << u8(0)
	}
}

fn elf64_write_header(mut output []u8, section_headers_offset u64) {
	output << [u8(0x7f), 0x45, 0x4c, 0x46, 0x02, 0x01, 0x01, 0x00, 0x00]
	for _ in 0 .. 7 {
		output << u8(0)
	}
	elf64_write_u16(mut output, u16(1))
	elf64_write_u16(mut output, u16(62))
	elf64_write_u32(mut output, u32(1))
	elf64_write_u64(mut output, u64(0))
	elf64_write_u64(mut output, u64(0))
	elf64_write_u64(mut output, section_headers_offset)
	elf64_write_u32(mut output, u32(0))
	elf64_write_u16(mut output, u16(64))
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(64))
	elf64_write_u16(mut output, elf64_section_count)
	elf64_write_u16(mut output, u16(5))
}

fn elf64_write_private_data_header(mut output []u8, section_headers_offset u64) {
	output << [u8(0x7f), 0x45, 0x4c, 0x46, 0x02, 0x01, 0x01, 0x00, 0x00]
	for _ in 0 .. 7 {
		output << u8(0)
	}
	elf64_write_u16(mut output, u16(1))
	elf64_write_u16(mut output, u16(62))
	elf64_write_u32(mut output, u32(1))
	elf64_write_u64(mut output, u64(0))
	elf64_write_u64(mut output, u64(0))
	elf64_write_u64(mut output, section_headers_offset)
	elf64_write_u32(mut output, u32(0))
	elf64_write_u16(mut output, u16(64))
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(64))
	elf64_write_u16(mut output, elf64_private_data_section_count)
	elf64_write_u16(mut output, u16(6))
}

fn elf64_write_section_header(mut output []u8, name u32, type_ u32, flags u64, address u64, offset u64, size u64, link u32, info u32, alignment u64, entry_size u64) {
	elf64_write_u32(mut output, name)
	elf64_write_u32(mut output, type_)
	elf64_write_u64(mut output, flags)
	elf64_write_u64(mut output, address)
	elf64_write_u64(mut output, offset)
	elf64_write_u64(mut output, size)
	elf64_write_u32(mut output, link)
	elf64_write_u32(mut output, info)
	elf64_write_u64(mut output, alignment)
	elf64_write_u64(mut output, entry_size)
}

fn elf64_validate_allocatable_size(name string, size u64) ! {
	if size > elf64_allocatable_size_limit {
		return error('ELF64 allocatable section ${name} size ${size} requires SHF_X86_64_LARGE')
	}
}

fn elf64_validate_legacy_allocatable_sizes(text_size u64, has_data bool, data_size u64) ! {
	elf64_validate_allocatable_size('.text', text_size)!
	if has_data {
		elf64_validate_allocatable_size('.data', data_size)!
	}
}

fn elf64_object_data_relocation_encoding(mapped ObjectDataFormatRelocation) !Elf64ObjectDataRelocationEncoding {
	return match mapped {
		.elf_64 {
			Elf64ObjectDataRelocationEncoding{
				typ:   elf64_r_x86_64_64
				width: 8
			}
		}
		.elf_32 {
			Elf64ObjectDataRelocationEncoding{
				typ:   elf64_r_x86_64_32
				width: 4
			}
		}
		.elf_32s {
			Elf64ObjectDataRelocationEncoding{
				typ:   elf64_r_x86_64_32s
				width: 4
			}
		}
		.elf_pc32 {
			Elf64ObjectDataRelocationEncoding{
				typ:   elf64_r_x86_64_pc32
				width: 4
			}
		}
		.elf_gotpcrel {
			Elf64ObjectDataRelocationEncoding{
				typ:   elf64_r_x86_64_gotpcrel
				width: 4
			}
		}
		else {
			error('ELF64 object data relocation ${mapped} is unsupported')
		}
	}
}

fn elf64_object_data_relocation_info(symbol_index u64, typ u64) !u64 {
	if symbol_index > u64(max_u32) {
		return error('ELF64 relocation symbol index exceeds u32')
	}
	if typ > u64(max_u32) {
		return error('ELF64 relocation type exceeds u32')
	}
	return (symbol_index << 32) | typ
}

fn elf64_object_data_rela_bytes(relocations []Elf64ObjectDataRelocation) ![]u8 {
	size := elf64_checked_mul(u64(relocations.len), elf64_rela_size, 'RELA section size')!
	mut bytes := []u8{cap: elf64_checked_host_size(size)!}
	for relocation in relocations {
		elf64_write_u64(mut bytes, relocation.offset)
		elf64_write_u64(mut bytes, relocation.info)
		elf64_write_u64(mut bytes, u64(relocation.addend))
	}
	if u64(bytes.len) != size {
		return error('ELF64 internal RELA section size mismatch')
	}
	return bytes
}

fn elf64_object_data_section_index(sections []Elf64ObjectDataSection, name string) !u32 {
	for index, section in sections {
		if section.name == name {
			return u32(index)
		}
	}
	return error('ELF64 required section ${name} is absent')
}

fn elf64_object_data_symbol_section(sections []Elf64ObjectDataSection, kind ObjectDataSectionKind) !u16 {
	name := match kind {
		.rodata { '.rodata' }
		.data { '.data' }
		.bss { '.bss' }
		.text { '.text' }
		.unknown { return error('ELF64 object data symbol section is missing') }
	}
	index := elf64_object_data_section_index(sections, name)!
	if index > u32(max_u16) {
		return error('ELF64 section index exceeds u16')
	}
	return u16(index)
}

fn elf64_object_data_write_symbol(mut output []u8, name u32, info u8, other u8, section u16, value u64, size u64) {
	elf64_write_u32(mut output, name)
	output << info
	output << other
	elf64_write_u16(mut output, section)
	elf64_write_u64(mut output, value)
	elf64_write_u64(mut output, size)
}

fn elf64_object_data_preflight(o &Object) !Elf64ObjectDataPreflight {
	o.validate_with_object_data()!
	if object_data_is_empty(&o.object_data) {
		return error('ELF64 object-data preflight requires object data')
	}
	elf64_validate_allocatable_size('.text', u64(o.text.len))!
	for section in o.object_data.sections {
		name := match section.kind {
			.rodata { '.rodata' }
			.data { '.data' }
			.bss { '.bss' }
			else { return error('ELF64 unsupported object data section ${section.kind}') }
		}
		elf64_validate_allocatable_size(name, section.size)!
	}

	rodata_index := object_data_find_section(o.object_data.sections, .rodata)
	object_data_index := object_data_find_section(o.object_data.sections, .data)
	bss_index := object_data_find_section(o.object_data.sections, .bss)
	has_rodata := rodata_index >= 0
	has_data := o.private_data.len != 0 || object_data_index >= 0
	has_bss := bss_index >= 0

	mut private_alignment := u64(1)
	for symbol in o.private_data_symbols {
		if symbol.alignment > private_alignment {
			private_alignment = symbol.alignment
		}
	}
	mut object_data_offset := u64(0)
	mut data_alignment := private_alignment
	mut data_bytes := o.private_data.clone()
	if object_data_index >= 0 {
		object_section := o.object_data.sections[object_data_index]
		object_data_offset = elf64_align(u64(o.private_data.len), object_section.alignment,
			'merged .data object offset')!
		if o.private_data.len == 0 {
			data_alignment = object_section.alignment
		} else if object_section.alignment > data_alignment {
			data_alignment = object_section.alignment
		}
		merged_size := elf64_checked_add(object_data_offset, object_section.size,
			'merged .data size')!
		elf64_validate_allocatable_size('.data', merged_size)!
		_ = elf64_checked_host_size(merged_size)!
		for u64(data_bytes.len) < object_data_offset {
			data_bytes << u8(0)
		}
		data_bytes << object_section.bytes
	}
	if has_data {
		elf64_validate_allocatable_size('.data', u64(data_bytes.len))!
	}

	private_and_object_count := elf64_checked_add(u64(o.private_data_symbols.len),
		u64(o.object_data.symbols.len), 'local symbol count')!
	symbol_count := elf64_private_data_symbol_count(private_and_object_count,
		u64(o.symbols.len))!
	object_symbol_base := elf64_checked_add(1, u64(o.private_data_symbols.len),
		'object data symbol base')!
	function_symbol_base := elf64_checked_add(object_symbol_base,
		u64(o.object_data.symbols.len), 'function symbol base')!
	if function_symbol_base > u64(max_u32) {
		return error('ELF64 first global symbol index exceeds u32')
	}

	mut text_object_relocation_count := u64(0)
	mut rodata_relocation_count := u64(0)
	mut data_relocation_count := u64(0)
	for relocation in o.object_data.relocations {
		match relocation.source_section {
			.text {
				text_object_relocation_count = elf64_checked_add(text_object_relocation_count,
					1, '.text object relocation count')!
			}
			.rodata {
				rodata_relocation_count = elf64_checked_add(rodata_relocation_count, 1,
					'.rodata relocation count')!
			}
			.data {
				data_relocation_count = elf64_checked_add(data_relocation_count, 1,
					'.data relocation count')!
			}
			else {
				return error('ELF64 unsupported relocation source ${relocation.source_section}')
			}
		}
	}
	text_relocation_count := elf64_checked_add(u64(o.call_relocations.len),
		text_object_relocation_count, '.text relocation count')!
	_ = elf64_checked_host_size(text_relocation_count)!
	_ = elf64_checked_host_size(rodata_relocation_count)!
	_ = elf64_checked_host_size(data_relocation_count)!

	mut text_relocations := []Elf64ObjectDataRelocation{
		cap: elf64_checked_host_size(text_relocation_count)!
	}
	mut rodata_relocations := []Elf64ObjectDataRelocation{
		cap: elf64_checked_host_size(rodata_relocation_count)!
	}
	mut data_relocations := []Elf64ObjectDataRelocation{
		cap: elf64_checked_host_size(data_relocation_count)!
	}
	for relocation in o.call_relocations {
		symbol_index := elf64_checked_add(function_symbol_base, u64(relocation.symbol_id),
			'CALL symbol index')!
		text_relocations << Elf64ObjectDataRelocation{
			offset: relocation.offset
			info:   elf64_object_data_relocation_info(symbol_index,
				elf64_r_x86_64_plt32)!
			addend: elf64_call_addend
		}
	}
	for relocation in o.object_data.relocations {
		mapped := object_data_map_relocation(&relocation, .elf_x86_64)!
		encoding := elf64_object_data_relocation_encoding(mapped)!
		if encoding.width != object_data_relocation_width_size(relocation.kind,
			relocation.width)! {
			return error('ELF64 object data relocation width does not match mapping')
		}
		target_index := elf64_checked_add(object_symbol_base,
			u64(relocation.target_symbol.id), 'object data relocation symbol index')!
		mut physical_offset := relocation.offset
		if relocation.source_section == .data {
			physical_offset = elf64_checked_add(object_data_offset, physical_offset,
				'merged .data relocation offset')!
		}
		physical := Elf64ObjectDataRelocation{
			offset: physical_offset
			info:   elf64_object_data_relocation_info(target_index, encoding.typ)!
			addend: relocation.addend
		}
		match relocation.source_section {
			.text { text_relocations << physical }
			.rodata { rodata_relocations << physical }
			.data { data_relocations << physical }
			else { return error('ELF64 unsupported relocation source') }
		}
	}
	text_relocations.sort(a.offset < b.offset)
	rodata_relocations.sort(a.offset < b.offset)
	data_relocations.sort(a.offset < b.offset)

	has_rela_rodata := rodata_relocations.len != 0
	has_rela_data := data_relocations.len != 0
	mut symtab_index := u32(3)
	if has_rodata {
		symtab_index++
	}
	if has_data {
		symtab_index++
	}
	if has_bss {
		symtab_index++
	}
	if has_rela_rodata {
		symtab_index++
	}
	if has_rela_data {
		symtab_index++
	}
	strtab_index := symtab_index + 1
	shstrtab_index := symtab_index + 2

	mut sections := []Elf64ObjectDataSection{cap: int(shstrtab_index) + 1}
	sections << Elf64ObjectDataSection{
		name:      ''
		type_:     0
		alignment: 0
	}
	sections << Elf64ObjectDataSection{
		name:          '.text'
		type_:         elf64_sht_progbits
		flags:         elf64_shf_alloc | elf64_shf_execinstr
		alignment:     16
		semantic_size: u64(o.text.len)
		bytes:         o.text.clone()
	}
	sections << Elf64ObjectDataSection{
		name:          '.rela.text'
		type_:         elf64_sht_rela
		flags:         elf64_shf_info_link
		alignment:     8
		entry_size:    elf64_rela_size
		semantic_size: elf64_checked_mul(u64(text_relocations.len), elf64_rela_size,
			'.rela.text size')!
		link:          symtab_index
		info:          1
		bytes:         elf64_object_data_rela_bytes(text_relocations)!
	}
	mut rodata_section_index := u32(0)
	if has_rodata {
		rodata := o.object_data.sections[rodata_index]
		rodata_section_index = u32(sections.len)
		sections << Elf64ObjectDataSection{
			name:          '.rodata'
			type_:         elf64_sht_progbits
			flags:         elf64_shf_alloc
			alignment:     rodata.alignment
			semantic_size: rodata.size
			bytes:         rodata.bytes.clone()
		}
	}
	mut data_section_index := u32(0)
	if has_data {
		data_section_index = u32(sections.len)
		sections << Elf64ObjectDataSection{
			name:          '.data'
			type_:         elf64_sht_progbits
			flags:         elf64_shf_alloc | elf64_shf_write
			alignment:     data_alignment
			semantic_size: u64(data_bytes.len)
			bytes:         data_bytes
		}
	}
	if has_bss {
		bss := o.object_data.sections[bss_index]
		sections << Elf64ObjectDataSection{
			name:          '.bss'
			type_:         elf64_sht_nobits
			flags:         elf64_shf_alloc | elf64_shf_write
			alignment:     bss.alignment
			semantic_size: bss.size
		}
	}
	if has_rela_rodata {
		sections << Elf64ObjectDataSection{
			name:          '.rela.rodata'
			type_:         elf64_sht_rela
			flags:         elf64_shf_info_link
			alignment:     8
			entry_size:    elf64_rela_size
			semantic_size: elf64_checked_mul(u64(rodata_relocations.len),
				elf64_rela_size, '.rela.rodata size')!
			link:          symtab_index
			info:          rodata_section_index
			bytes:         elf64_object_data_rela_bytes(rodata_relocations)!
		}
	}
	if has_rela_data {
		sections << Elf64ObjectDataSection{
			name:          '.rela.data'
			type_:         elf64_sht_rela
			flags:         elf64_shf_info_link
			alignment:     8
			entry_size:    elf64_rela_size
			semantic_size: elf64_checked_mul(u64(data_relocations.len), elf64_rela_size,
				'.rela.data size')!
			link:          symtab_index
			info:          data_section_index
			bytes:         elf64_object_data_rela_bytes(data_relocations)!
		}
	}
	if u32(sections.len) != symtab_index {
		return error('ELF64 internal dynamic section index mismatch')
	}

	mut strtab := [u8(0)]
	mut private_name_offsets := []u32{cap: o.private_data_symbols.len}
	for symbol in o.private_data_symbols {
		private_name_offsets << elf64_append_string(mut strtab, symbol.name)!
	}
	mut object_name_offsets := []u32{cap: o.object_data.symbols.len}
	for symbol in o.object_data.symbols {
		if symbol.kind == .named {
			object_name_offsets << elf64_append_string(mut strtab, symbol.name)!
		} else {
			object_name_offsets << u32(0)
		}
	}
	mut function_name_offsets := []u32{cap: o.symbols.len}
	for symbol in o.symbols {
		function_name_offsets << elf64_append_string(mut strtab, symbol.name)!
	}

	symtab_size := elf64_checked_mul(symbol_count, elf64_symbol_size, '.symtab size')!
	mut symtab := []u8{cap: elf64_checked_host_size(symtab_size)!}
	elf64_object_data_write_symbol(mut symtab, 0, 0, 0, 0, 0, 0)
	data_symbol_section := if o.private_data_symbols.len == 0 {
		u16(0)
	} else {
		elf64_object_data_symbol_section(sections, .data)!
	}
	for index, symbol in o.private_data_symbols {
		elf64_object_data_write_symbol(mut symtab, private_name_offsets[index], 0x01, 0,
			data_symbol_section, symbol.offset, symbol.size)
	}
	for index, symbol in o.object_data.symbols {
		mut value := symbol.offset
		if symbol.section == .data {
			value = elf64_checked_add(object_data_offset, value, 'object data symbol value')!
		}
		elf64_object_data_write_symbol(mut symtab, object_name_offsets[index], 0x01, 0,
			elf64_object_data_symbol_section(sections, symbol.section)!, value, symbol.size)
	}
	for index, symbol in o.symbols {
		if symbol.intentional_external {
			elf64_object_data_write_symbol(mut symtab, function_name_offsets[index], 0x12, 0,
				0, 0, 0)
		} else {
			elf64_object_data_write_symbol(mut symtab, function_name_offsets[index], 0x12, 0,
				1, symbol.offset, symbol.size)
		}
	}
	if u64(symtab.len) != symtab_size {
		return error('ELF64 internal object-data symbol table size mismatch')
	}
	sections << Elf64ObjectDataSection{
		name:          '.symtab'
		type_:         elf64_sht_symtab
		alignment:     8
		entry_size:    elf64_symbol_size
		semantic_size: symtab_size
		link:          strtab_index
		info:          u32(function_symbol_base)
		bytes:         symtab
	}
	sections << Elf64ObjectDataSection{
		name:          '.strtab'
		type_:         elf64_sht_strtab
		alignment:     1
		semantic_size: u64(strtab.len)
		bytes:         strtab
	}
	sections << Elf64ObjectDataSection{
		name:      '.shstrtab'
		type_:     elf64_sht_strtab
		alignment: 1
	}
	if u32(sections.len) != shstrtab_index + 1 {
		return error('ELF64 internal section count mismatch')
	}

	mut shstrtab := [u8(0)]
	for index in 1 .. sections.len {
		sections[index].name_offset = elf64_append_string(mut shstrtab, sections[index].name)!
	}
	sections[int(shstrtab_index)].bytes = shstrtab
	sections[int(shstrtab_index)].semantic_size = u64(sections[int(shstrtab_index)].bytes.len)

	mut cursor := elf64_header_size
	for index in 1 .. sections.len {
		section_offset := elf64_align(cursor, sections[index].alignment,
			'${sections[index].name} offset')!
		sections[index].offset = section_offset
		if sections[index].type_ == elf64_sht_nobits {
			cursor = section_offset
			continue
		}
		if sections[index].semantic_size != u64(sections[index].bytes.len) {
			return error('ELF64 internal ${sections[index].name} payload size mismatch')
		}
		cursor = elf64_checked_add(section_offset, sections[index].semantic_size,
			'${sections[index].name} extent')!
	}
	section_headers_offset := elf64_align(cursor, 8, 'section header offset')!
	section_headers_size := elf64_checked_mul(u64(sections.len), elf64_section_header_size,
		'section header table size')!
	file_size := elf64_checked_add(section_headers_offset, section_headers_size, 'file size')!
	_ = elf64_checked_host_size(file_size)!
	return Elf64ObjectDataPreflight{
		sections:               sections
		section_headers_offset: section_headers_offset
		file_size:              file_size
		shstrtab_index:         u16(shstrtab_index)
	}
}

fn elf64_write_object_data_header(mut output []u8, section_headers_offset u64, section_count u16, shstrtab_index u16) {
	output << [u8(0x7f), 0x45, 0x4c, 0x46, 0x02, 0x01, 0x01, 0x00, 0x00]
	for _ in 0 .. 7 {
		output << u8(0)
	}
	elf64_write_u16(mut output, u16(1))
	elf64_write_u16(mut output, u16(62))
	elf64_write_u32(mut output, u32(1))
	elf64_write_u64(mut output, u64(0))
	elf64_write_u64(mut output, u64(0))
	elf64_write_u64(mut output, section_headers_offset)
	elf64_write_u32(mut output, u32(0))
	elf64_write_u16(mut output, u16(64))
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(64))
	elf64_write_u16(mut output, section_count)
	elf64_write_u16(mut output, shstrtab_index)
}

fn elf64_object_data_relocatable_bytes(o &Object) ![]u8 {
	preflight := elf64_object_data_preflight(o)!
	mut output := []u8{cap: elf64_checked_host_size(preflight.file_size)!}
	elf64_write_object_data_header(mut output, preflight.section_headers_offset,
		u16(preflight.sections.len), preflight.shstrtab_index)
	for section in preflight.sections[1..] {
		elf64_pad_to(mut output, section.offset)!
		if section.type_ != elf64_sht_nobits {
			output << section.bytes
		}
	}
	elf64_pad_to(mut output, preflight.section_headers_offset)!
	for section in preflight.sections {
		elf64_write_section_header(mut output, section.name_offset, section.type_, section.flags,
			0, section.offset, section.semantic_size, section.link, section.info,
			section.alignment, section.entry_size)
	}
	if u64(output.len) != preflight.file_size {
		return error('ELF64 internal object-data layout size mismatch')
	}
	return output
}

fn elf64_relocatable_bytes(o &Object) ![]u8 {
	if !object_data_is_empty(&o.object_data) {
		return elf64_object_data_relocatable_bytes(o)
	}
	if o.private_data.len != 0 {
		return elf64_private_data_relocatable_bytes(o)
	}
	elf64_validate_legacy_allocatable_sizes(u64(o.text.len), false, 0)!
	o.validate()!

	mut strtab := [u8(0)]
	mut symbol_name_offsets := []u32{cap: o.symbols.len}
	for symbol in o.symbols {
		symbol_name_offsets << elf64_append_string(mut strtab, symbol.name)!
	}

	mut shstrtab := [u8(0)]
	text_name := elf64_append_string(mut shstrtab, '.text')!
	rela_text_name := elf64_append_string(mut shstrtab, '.rela.text')!
	symtab_name := elf64_append_string(mut shstrtab, '.symtab')!
	strtab_name := elf64_append_string(mut shstrtab, '.strtab')!
	shstrtab_name := elf64_append_string(mut shstrtab, '.shstrtab')!

	text_size := u64(o.text.len)
	rela_text_size := elf64_checked_mul(u64(o.call_relocations.len), elf64_rela_size,
		'.rela.text size')!
	symbol_count := elf64_private_data_symbol_count(0, u64(o.symbols.len))!
	symtab_size := elf64_checked_mul(symbol_count, elf64_symbol_size, '.symtab size')!
	layout := elf64_build_layout(text_size, rela_text_size, symtab_size, u64(strtab.len),
		u64(shstrtab.len))!
	if layout.file_size > u64(max_int) {
		return error('ELF64 output exceeds the host array limit')
	}

	mut output := []u8{cap: int(layout.file_size)}
	elf64_write_header(mut output, layout.section_headers_offset)

	elf64_pad_to(mut output, layout.text_offset)!
	output << o.text

	elf64_pad_to(mut output, layout.rela_text_offset)!
	for relocation in o.call_relocations {
		symbol_index := elf64_checked_add(u64(relocation.symbol_id), 1, 'CALL symbol index')!
		relocation_info := (symbol_index << 32) | elf64_r_x86_64_plt32
		elf64_write_u64(mut output, relocation.offset)
		elf64_write_u64(mut output, relocation_info)
		elf64_write_u64(mut output, u64(elf64_call_addend))
	}

	elf64_pad_to(mut output, layout.symtab_offset)!
	for _ in 0 .. 24 {
		output << u8(0)
	}
	for index, symbol in o.symbols {
		elf64_write_u32(mut output, symbol_name_offsets[index])
		output << u8(0x12)
		output << u8(0)
		if symbol.intentional_external {
			elf64_write_u16(mut output, u16(0))
			elf64_write_u64(mut output, u64(0))
			elf64_write_u64(mut output, u64(0))
		} else {
			elf64_write_u16(mut output, u16(1))
			elf64_write_u64(mut output, symbol.offset)
			elf64_write_u64(mut output, symbol.size)
		}
	}

	elf64_pad_to(mut output, layout.strtab_offset)!
	output << strtab
	elf64_pad_to(mut output, layout.shstrtab_offset)!
	output << shstrtab

	elf64_pad_to(mut output, layout.section_headers_offset)!
	elf64_write_section_header(mut output, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
	elf64_write_section_header(mut output, text_name, elf64_sht_progbits,
		elf64_shf_alloc | elf64_shf_execinstr, 0, layout.text_offset, text_size, 0, 0, 16, 0)
	elf64_write_section_header(mut output, rela_text_name, elf64_sht_rela, 0, 0,
		layout.rela_text_offset, rela_text_size, 3, 1, 8, elf64_rela_size)
	elf64_write_section_header(mut output, symtab_name, elf64_sht_symtab, 0, 0,
		layout.symtab_offset, symtab_size, 4, 1, 8, elf64_symbol_size)
	elf64_write_section_header(mut output, strtab_name, elf64_sht_strtab, 0, 0,
		layout.strtab_offset, u64(strtab.len), 0, 0, 1, 0)
	elf64_write_section_header(mut output, shstrtab_name, elf64_sht_strtab, 0, 0,
		layout.shstrtab_offset, u64(shstrtab.len), 0, 0, 1, 0)

	if u64(output.len) != layout.file_size {
		return error('ELF64 internal layout size mismatch')
	}
	return output
}

fn elf64_private_data_relocatable_bytes(o &Object) ![]u8 {
	if !object_data_is_empty(&o.object_data) {
		return elf64_object_data_relocatable_bytes(o)
	}
	elf64_validate_legacy_allocatable_sizes(u64(o.text.len), true, u64(o.private_data.len))!
	o.validate()!
	if o.private_data_symbols.len == 0 {
		return error('ELF64 private data has no symbols')
	}

	mut strtab := [u8(0)]
	mut data_name_offsets := []u32{cap: o.private_data_symbols.len}
	for symbol in o.private_data_symbols {
		data_name_offsets << elf64_append_string(mut strtab, symbol.name)!
	}
	mut function_name_offsets := []u32{cap: o.symbols.len}
	for symbol in o.symbols {
		function_name_offsets << elf64_append_string(mut strtab, symbol.name)!
	}

	mut shstrtab := [u8(0)]
	text_name := elf64_append_string(mut shstrtab, '.text')!
	rela_text_name := elf64_append_string(mut shstrtab, '.rela.text')!
	data_name := elf64_append_string(mut shstrtab, '.data')!
	symtab_name := elf64_append_string(mut shstrtab, '.symtab')!
	strtab_name := elf64_append_string(mut shstrtab, '.strtab')!
	shstrtab_name := elf64_append_string(mut shstrtab, '.shstrtab')!

	mut data_alignment := u64(1)
	for symbol in o.private_data_symbols {
		if symbol.alignment > data_alignment {
			data_alignment = symbol.alignment
		}
	}
	text_size := u64(o.text.len)
	rela_text_size := elf64_checked_mul(u64(o.call_relocations.len), elf64_rela_size,
		'.rela.text size')!
	symbol_count := elf64_private_data_symbol_count(u64(o.private_data_symbols.len),
		u64(o.symbols.len))!
	symtab_size := elf64_checked_mul(symbol_count, elf64_symbol_size, '.symtab size')!
	layout := elf64_build_private_data_layout(text_size, rela_text_size, u64(o.private_data.len),
		data_alignment, symtab_size, u64(strtab.len), u64(shstrtab.len))!
	output_capacity := elf64_checked_host_size(layout.file_size)!

	mut output := []u8{cap: output_capacity}
	elf64_write_private_data_header(mut output, layout.section_headers_offset)
	elf64_pad_to(mut output, layout.text_offset)!
	output << o.text
	elf64_pad_to(mut output, layout.rela_text_offset)!
	for relocation in o.call_relocations {
		symbol_index := elf64_checked_add(elf64_checked_add(1, u64(o.private_data_symbols.len),
			'CALL symbol index')!, u64(relocation.symbol_id), 'CALL symbol index')!
		relocation_info := (symbol_index << 32) | elf64_r_x86_64_plt32
		elf64_write_u64(mut output, relocation.offset)
		elf64_write_u64(mut output, relocation_info)
		elf64_write_u64(mut output, u64(elf64_call_addend))
	}
	elf64_pad_to(mut output, layout.data_offset)!
	output << o.private_data
	elf64_pad_to(mut output, layout.symtab_offset)!
	for _ in 0 .. 24 {
		output << u8(0)
	}
	for index, symbol in o.private_data_symbols {
		elf64_write_u32(mut output, data_name_offsets[index])
		output << u8(0x01)
		output << u8(0)
		elf64_write_u16(mut output, u16(3))
		elf64_write_u64(mut output, symbol.offset)
		elf64_write_u64(mut output, symbol.size)
	}
	for index, symbol in o.symbols {
		elf64_write_u32(mut output, function_name_offsets[index])
		output << u8(0x12)
		output << u8(0)
		if symbol.intentional_external {
			elf64_write_u16(mut output, u16(0))
			elf64_write_u64(mut output, u64(0))
			elf64_write_u64(mut output, u64(0))
		} else {
			elf64_write_u16(mut output, u16(1))
			elf64_write_u64(mut output, symbol.offset)
			elf64_write_u64(mut output, symbol.size)
		}
	}
	elf64_pad_to(mut output, layout.strtab_offset)!
	output << strtab
	elf64_pad_to(mut output, layout.shstrtab_offset)!
	output << shstrtab

	elf64_pad_to(mut output, layout.section_headers_offset)!
	elf64_write_section_header(mut output, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
	elf64_write_section_header(mut output, text_name, elf64_sht_progbits,
		elf64_shf_alloc | elf64_shf_execinstr, 0, layout.text_offset, text_size, 0, 0, 16, 0)
	elf64_write_section_header(mut output, rela_text_name, elf64_sht_rela, 0, 0,
		layout.rela_text_offset, rela_text_size, 4, 1, 8, elf64_rela_size)
	elf64_write_section_header(mut output, data_name, elf64_sht_progbits,
		elf64_shf_alloc | elf64_shf_write, 0, layout.data_offset, u64(o.private_data.len), 0, 0,
		data_alignment, 0)
	elf64_write_section_header(mut output, symtab_name, elf64_sht_symtab, 0, 0,
		layout.symtab_offset, symtab_size, 5, u32(1 + o.private_data_symbols.len), 8,
		elf64_symbol_size)
	elf64_write_section_header(mut output, strtab_name, elf64_sht_strtab, 0, 0,
		layout.strtab_offset, u64(strtab.len), 0, 0, 1, 0)
	elf64_write_section_header(mut output, shstrtab_name, elf64_sht_strtab, 0, 0,
		layout.shstrtab_offset, u64(shstrtab.len), 0, 0, 1, 0)

	if u64(output.len) != layout.file_size {
		return error('ELF64 internal private-data layout size mismatch')
	}
	return output
}
